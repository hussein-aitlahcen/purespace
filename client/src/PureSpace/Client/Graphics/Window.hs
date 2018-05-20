--Window.hs ---

-- Copyright (C) 2018 Hussein Ait-Lahcen

-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE MultiParamTypeClasses #-}

module PureSpace.Client.Graphics.Window
  (
    createGameWindow
  )
  where

import qualified Data.Map                        as M
import qualified Data.Vector                     as V
import           Graphics.GLUtil.BufferObjects   (makeBuffer)
import           Graphics.UI.GLUT                as GLUT hiding (Position,
                                                          ortho2D, position,
                                                          uniform)
import           Linear                          hiding (identity)
import           PureSpace.Client.Graphics
import           PureSpace.Common.Concurrent
import           PureSpace.Common.Game.Collision
import           PureSpace.Common.Game.Types
import           PureSpace.Common.Lens
import           PureSpace.Common.Prelude

{-
############################
Everything under this line is complete garbage atm
############################
-}

type SpriteName = String
type SpritesByName = M.Map SpriteName GraphicsSprite

data GraphicsSprite = GraphicsSprite Sprite VertexArrayObject deriving Show

openGLVersion :: (Int, Int)
openGLVersion = (3, 3)

fps :: Int
fps = 120

createGameWindow :: (MonadIO m,
                     MonadState s m,
                     MonadError e m,
                     AsResourceError e,
                     HasDeviceState s,
                     HasShaderState s,
                     HasShaderProgramState s,
                     AsGraphicsError e,
                     AsAssetError e,
                     AsShaderError e,
                     AsShaderProgramError e)
                 => m ()
createGameWindow = do
  initialContextVersion $= openGLVersion
  atlas                    <- loadAssetJSON ("sprite_atlas", spriteSheetPath)
  (_, _)                   <- getArgsAndInitialize
  window                   <- createWindow "PureSpace"
  (program, sprites) <- initContext atlas
  displayCallback $= debugDisplay program sprites
  idleCallback    $= Just (postRedisplay (Just window))
  loadInputState
  windowLoop

loadInputState :: (MonadIO m,
                  MonadState s m,
                  HasDeviceState s)
               => m ()
loadInputState = do
  inputChan <- liftIO $ atomically newBroadcastTChan
  deviceInputState .= Just inputChan
  keyboardMouseCallback $= Just (inputStream inputChan)
  readInputChan <- liftIO $ atomically $ dupTChan inputChan
  _             <- liftIO $ forkIO $ loop readInputChan
  pure ()
  where
    loop c = do
      event <- atomically $ readTChan c
      print event
      loop c

windowLoop :: MonadIO m => m ()
windowLoop = mainLoopEvent *> delayLoop *> windowLoop
  where
    delayLoop =
      let microsecond = 1000000 :: Float
          step        = round $ microsecond / fromIntegral fps
      in liftIO $ threadDelay step

initContext :: (MonadIO m,
                MonadState s m,
                MonadError e m,
                AsResourceError e,
                HasShaderState s,
                HasShaderProgramState s,
                AsGraphicsError e,
                AsShaderError e,
                AsShaderProgramError e)
            => SpriteAtlas
            -> m (Program, SpritesByName)
initContext (SpriteAtlas image sprites) = do
  liftIO $ putStrLn "initialize"
  program <- loadGameShaderProgram [(VertexShader  , shadersPath <> "/sprite.vert")
                                  , (FragmentShader, shadersPath <> "/sprite.frag")]
  initialDisplayMode          $= [DoubleBuffered]
  blend                       $= Enabled
  blendFunc                   $= (SrcAlpha, OneMinusSrcAlpha)
  (SpriteTexture textW textH text) <- createTexture image
  let vertices = initVertices textW textH =<< sprites
  buffer          <- liftIO $ makeBuffer ArrayBuffer vertices
  graphicsSprites <- traverse (uncurry (createGraphicsSprite buffer)) (enumerate sprites)
  textureBinding Texture2D $= Just text
  let spriteMap = buildSpriteMap graphicsSprites
  pure (program, spriteMap)

initVertices :: Int -> Int -> Sprite -> [GLfloat]
initVertices textW textH (Sprite _ x y w h) =
  spriteVertices x y w h textW textH

buildSpriteMap :: [GraphicsSprite]-> SpritesByName
buildSpriteMap =
  let namedTuple g@(GraphicsSprite (Sprite name _ _ _ _) _) = (name, g)
  in M.fromList . fmap namedTuple

createGraphicsSprite :: MonadIO m => BufferObject -> Int -> Sprite -> m GraphicsSprite
createGraphicsSprite buffer i sprite =
  pure GraphicsSprite
  <*> pure sprite
  <*> spriteVAO buffer i

vaoByName :: SpriteName-> SpritesByName -> Maybe VertexArrayObject
vaoByName name sprites =
  let vao (GraphicsSprite _ v) = v
  in fmap vao $ name `M.lookup` sprites

{-
############################
Warning: even worse than above, eye bleeding may occurs
############################
-}

entities :: [TestEntity]
entities = [TestEntity (V2 400 200) 98 75 (V2 (-60)   0),
            TestEntity (V2 0   200) 98 75 (V2   60    0),
            TestEntity (V2 200 400) 98 75 (V2    0 (-60)),
            TestEntity (V2 0     0) 98 75 (V2    60  100)]

mapWidth :: GridSize
mapWidth = 600

debugDisplay :: Program -> SpritesByName -> DisplayCallback
debugDisplay program sprites = do
  Size w h          <- GLUT.get windowSize
  time              <- elapsedTime
  clear [ColorBuffer, DepthBuffer]
  currentProgram $= Just program
  let orangeShip = "playerShip1_blue.png" `vaoByName` sprites
      elapsedSeconds = fromIntegral time / 1000
      stepEntities =  (\e -> e & position %~ (+ e ^. velocity * elapsedSeconds)) <$> entities
  debugCollision stepEntities
  traverse_ (\x -> sequence_ $ fmap ($ x) (displaySprite (fromIntegral w) (fromIntegral h) <$> stepEntities)) orangeShip
  currentProgram $= Nothing
  swapBuffers
  postRedisplay Nothing
  where
    uniformP = uniform program
    displaySprite w h (TestEntity p _ _ v) vao = do
      uniformP "mProjection" $ ortho2D 3 w h
      -- WHY SHOULD I SCALE TO GET IT NICE ??
      uniformP "mModelView"  $ rotate2D (angleOf v) $ translate2D worldToUV $ scale2D 3.3 identity
      bindVertexArrayObject $= Just vao
      spriteDraw
      bindVertexArrayObject $= Nothing
      where
        -- pi/2 because of the sprite initial position :/
        angleOf (V2 x y) = atan2 y x - pi/2
        worldToUV =
          let mapHW = mapWidth / 2
          in (p - V2 mapHW mapHW) ^/ mapHW

debugCollision :: [TestEntity] -> DisplayCallback
debugCollision e =
  let grid       = createCollisionGrid mapWidth 20 e
      collisions = V.toList $ computeCollisions grid
  in traverse_ print collisions

data TestEntity = TestEntity (V2 Float) Int Int (V2 Float) deriving (Show, Eq)

instance HasPosition TestEntity where
  position =
    let f (TestEntity a _ _ _)   = a
        g (TestEntity _ b c d) a = TestEntity a b c d
    in lens f g

instance HasVelocity TestEntity where
  velocity =
    let f (TestEntity _ _ _ d)   = d
        g (TestEntity a b c _) d = TestEntity a b c d
    in lens f g

instance HasWidth TestEntity where
  width =
    let f (TestEntity _ b _ _)   = b
        g (TestEntity a _ c d) b = TestEntity a b c d
    in lens f g

instance HasHeight TestEntity where
  height =
    let f (TestEntity _ _ c _)   = c
        g (TestEntity a b _ d) c = TestEntity a b c d
    in lens f g
