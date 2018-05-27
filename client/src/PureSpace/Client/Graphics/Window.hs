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

import qualified Data.Map                      as M
import           Graphics.GLUtil.BufferObjects (makeBuffer)
import           Graphics.UI.GLUT              as GLUT hiding (One, Position,
                                                        ortho2D, position,
                                                        uniform)
import           Linear                        hiding (angle, identity)
import           PureSpace.Client.Graphics
import           PureSpace.Common.Concurrent
import           PureSpace.Common.Game.Entity
import           PureSpace.Common.Game.GameFSM
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
                     MonadReader r m,
                     HasGameConfig r,
                     HasGridSize r,
                     HasGridDivision r,
                     AsResourceError e,
                     HasDeviceState s,
                     HasShaderState s,
                     HasShaderProgramState s,
                     AsGraphicsError e,
                     AsAssetError e,
                     AsShaderError e,
                     AsShaderProgramError e,
                     HasGameState c,
                     HasSpatialGrid c
                    )
                 => TChan c -> m ()
createGameWindow sink = do
  initialContextVersion $= openGLVersion
  atlas              <- loadAssetJSON ("sprite_atlas", spriteSheetPath)
  (_, _)             <- getArgsAndInitialize
  _                  <- createWindow "PureSpace"
  (program, sprites) <- initContext atlas
  gameConf           <- view gameConfig
  windowSize $= Size 800 600
  displayCallback $= debugDisplay gameConf sink program sprites
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
windowLoop = mainLoopEvent *> delayLoop *> postRedisplay Nothing *> windowLoop
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

vaoByName :: SpritesByName -> SpriteName -> Maybe VertexArrayObject
vaoByName sprites name =
  let vao (GraphicsSprite _ v) = v
  in fmap vao $ name `M.lookup` sprites

{-
############################
Warning: even worse than above, eye bleeding may occurs
VISUAL TESTING PURPOSES ONLY
############################
-}

debugDisplay :: (HasGameConfig s,
                 HasGridSize s,
                 HasGameState c,
                 HasSpatialGrid c,
                 HasGridDivision s)
             => s
             -> TChan c
             -> Program
             -> SpritesByName
             -> DisplayCallback
debugDisplay config sink program sprites = do
  Size w h <- GLUT.get windowSize
  clear [ColorBuffer, DepthBuffer]
  currentProgram $= Just program
  nextGame <- liftIO $ atomically $ readTChan sink
  let (Just shipVAO) = sprites `vaoByName` "playerShip1_blue.png"
      (Just projVAO) = sprites `vaoByName` "laserBlue05.png"
      display :: (HasPosition s, HasVelocity s, HasAngle s) => VertexArrayObject -> s -> DisplayCallback
      display                            = displaySprite config program (fromIntegral w) (fromIntegral h)
      displayEntity (EntityShip s)       = display shipVAO s
      displayEntity (EntityProjectile p) = display projVAO p
      -- TODO: add base vao
      displayEntity _                    = putStrLn "bases not drawable yet"
  traverse_ displayEntity $ eliminateSpatialGrid (nextGame ^. spatialGrid)
  currentProgram $= Nothing
  swapBuffers

displaySprite :: (HasGameConfig a, HasGridSize a, HasPosition s, HasVelocity s, HasAngle s) => a -> Program -> Float -> Float -> VertexArrayObject -> s -> DisplayCallback
displaySprite config program w h vao s = do
  -- 2D orthographic projection relative to the map size
  uniformP "mProjection" $ ortho2D (max (gs ^. _x) (gs ^. _y)) w h
  -- center the camera according to the map width/height
  uniformP "mView"       $ translate2D (-(config ^. gridSize) / 2) identity
  -- we subtract pi/2 to the angle because
  -- of the sprite initial position in the texture
  uniformP "mModel"      $ rotate2D (s ^. angle - pi/2) $ translate2D (s ^. position) identity
  bindVertexArrayObject $= Just vao
  spriteDraw
  bindVertexArrayObject $= Nothing
  where
    uniformP = uniform program
    gs = config ^. gridSize
