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

import           Data.IORef
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
  atlas              <- loadAssetJSON ("sprite_atlas", spriteSheetPath)
  (_, _)             <- getArgsAndInitialize
  window             <- createWindow "PureSpace"
  (program, sprites) <- initContext atlas
  gameRef            <- liftIO $ newIORef game
  displayCallback $= debugDisplay gameRef program sprites
  idleCallback    $= Just (postRedisplay (Just window))
  loadInputState
  windowLoop
  where
    game = GameState [PlayerState One [] [Ship sc One 100 0 (V2 0 400) (V2 0 0) 0,
                                          Ship sc One 100 0 (V2 0 200) (V2 0 0) 0,
                                          Ship sc One 100 0 (V2 0 0) (V2 0 0) 0] 0 [],
                      PlayerState Two [] [Ship sc Two 100 0 (V2 1000 400) (V2 0 0) 0,
                                          Ship sc Two 100 0 (V2 1000 200) (V2 0 0) 0,
                                          Ship sc Two 100 0 (V2 1000   0) (V2 0 0) 0] 0 []]
      where
        pc = ProjectileCaracteristics (ProjectileType Laser 2 6) 10 (V2 500 500)
        sc = ShipCaracteristics (ShipType Fighter 100 75) pc 100 (V2 300 300) 1 500

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

-- TODO: game config
mapWidth :: GridSize
mapWidth = 1500

debugDisplay :: IORef GameState -> Program -> SpritesByName -> DisplayCallback
debugDisplay gameStateRef program sprites = do
  Size w h <- GLUT.get windowSize
  clear [ColorBuffer, DepthBuffer]
  currentProgram $= Just program
  game <- readIORef gameStateRef
  let elapsedSeconds                     = 1 / fromIntegral fps -- totally fake so what ?
      (grid, nextGame)                           = runState (updateGame elapsedSeconds) game
      (Just shipVAO)                     = sprites `vaoByName` "playerShip1_blue.png"
      (Just projVAO)                     = sprites `vaoByName` "laserBlue05.png"
      display :: (HasPosition s, HasVelocity s, HasAngle s) => VertexArrayObject -> s -> DisplayCallback
      display                            = displaySprite program (fromIntegral w) (fromIntegral h)
      displayEntity (EntityShip s)       = display shipVAO s
      displayEntity (EntityProjectile p) = display projVAO p
      displayEntity _                    = putStrLn "bases not drawable yet"
  writeIORef gameStateRef nextGame
  traverse_ displayEntity $ eliminateSpatialGrid grid
  currentProgram $= Nothing
  swapBuffers
  postRedisplay Nothing

displaySprite :: (HasPosition s, HasVelocity s, HasAngle s) => Program -> Float -> Float -> VertexArrayObject -> s -> DisplayCallback
displaySprite program w h vao s = do
  uniformP "mProjection" $ ortho2D mapWidth w h
  uniformP "mModelView"  $ rotate2D (s ^. angle - pi/2) $ translate2D (s ^. position) identity
  bindVertexArrayObject $= Just vao
  spriteDraw
  bindVertexArrayObject $= Nothing
  where
    uniformP = uniform program
