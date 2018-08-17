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
  ( createGameWindow
  ) where

import Data.IORef
import qualified Data.Map as M
import Graphics.GLUtil.BufferObjects (makeBuffer)
import Graphics.UI.GLUT as GLUT hiding
  ( One
  , Position
  , ortho2D
  , position
  , uniform
  )
import Linear hiding (angle, identity)
import PureSpace.Client.Graphics
import PureSpace.Common.Concurrent
import PureSpace.Common.Game.Entity
import PureSpace.Common.Game.GameFSM
import PureSpace.Common.Lens
import PureSpace.Common.Prelude

{-
############################
Everything under this line is complete garbage atm
############################
-}
type SpriteName = String

type SpritesByName = M.Map SpriteName GraphicsSprite

data GraphicsSprite =
  GraphicsSprite Sprite
                 VertexArrayObject
  deriving (Show)

openGLVersion :: (Int, Int)
openGLVersion = (3, 3)

fps :: Int
fps = 120

createGameWindow ::
     ( MonadIO m
     , MonadState s m
     , MonadError e m
     , MonadReader r m
     , HasGameConfig r
     , HasGridSize r
     , HasGridDivision r
     , AsResourceError e
     , HasDeviceState s
     , HasShaderState s
     , HasShaderProgramState s
     , AsGraphicsError e
     , AsAssetError e
     , AsShaderError e
     , AsShaderProgramError e
     )
  => m ()
createGameWindow = do
  initialContextVersion $= openGLVersion
  (_, _) <- getArgsAndInitialize
  _ <- createWindow "PureSpace"
  (program, sprites) <- initContext
  gameConf <- view gameConfig
  gameRef <- liftIO $ newIORef (game gameConf)
  displayCallback $= debugDisplay gameConf gameRef program sprites
  windowSize $= Size 800 600
  loadInputState
  windowLoop
  where
    game (GameConfig a b) =
      GameState
        (createSpatialGrid a b ships)
        [PlayerState 0 One 0, PlayerState 1 Two 0]
        (ships ++ bases)
        0
      where
        pc =
          ProjectileCaracteristics (ProjectileType Laser 2 6) 10 (V2 1500 1500)
        sc =
          ShipCaracteristics
            (ShipType Fighter 100 75)
            pc
            100
            (V2 300 300)
            1
            (CircleRange 500)
        ships =
          EntityShip <$>
          [ Ship sc One 100 0 (V2 0 2000) (V2 0 0) 0 0 0
          , Ship sc One 100 0 (V2 0 1000) (V2 0 0) 0 1 0
          , Ship sc One 100 0 (V2 0 0) (V2 0 0) 0 2 0
          , Ship sc Two 100 0 (V2 2000 2000) (V2 0 0) 0 3 1
          , Ship sc Two 100 0 (V2 2000 1000) (V2 0 0) 0 4 1
          , Ship sc Two 100 0 (V2 2000 0) (V2 0 0) 0 5 1
          ]
        bases =
          EntityBase <$>
          [ Base
              (BaseCaracteristics 100 True 40 40)
              One
              0
              100
              (V2 0 345)
              0
              [Fleet (FleetCaracteristics [FleetComposition sc 0] 2.2 0) 0]
              6
              0
              []
          , Base
              (BaseCaracteristics 100 True 40 40)
              Two
              0
              100
              (V2 2000 786)
              0
              [Fleet (FleetCaracteristics [FleetComposition sc 0] 4.2 0) 0]
              6
              1
              []
          ]

loadInputState :: (MonadIO m, MonadState s m, HasDeviceState s) => m ()
loadInputState = do
  inputChan <- liftIO $ atomically newBroadcastTChan
  deviceInputState .= Just inputChan
  keyboardMouseCallback $= Just (inputStream inputChan)
  readInputChan <- liftIO $ atomically $ dupTChan inputChan
  _ <- liftIO $ forkIO $ loop readInputChan
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
          step = round $ microsecond / fromIntegral fps
       in liftIO $ threadDelay step

initContext ::
     ( MonadIO m
     , MonadState s m
     , MonadError e m
     , AsAssetError e
     , AsResourceError e
     , HasShaderState s
     , HasShaderProgramState s
     , AsGraphicsError e
     , AsShaderError e
     , AsShaderProgramError e
     )
  => m (Program, SpritesByName)
initContext =
  let glContext = do
        initialDisplayMode $= [DoubleBuffered]
        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
      graphicsContext =
        let shaderContext = loadGameShaderProgram =<< shaders
            textureContext = do
              (SpriteTexture textW textH text) <-
                createTexture =<< spritesAtlasPath
              (SpriteAtlas sprites) <- loadSpritesAtlasDefinition
              buffer <-
                liftIO $
                makeBuffer ArrayBuffer (initVertices textW textH =<< sprites)
              textureBinding Texture2D $= Just text
              buildSpriteMap <$>
                traverse
                  (uncurry (createGraphicsSprite buffer))
                  (enumerate sprites)
         in pure (,) <*> shaderContext <*> textureContext
   in glContext *> graphicsContext

initVertices :: Int -> Int -> Sprite -> [GLfloat]
initVertices textW textH (Sprite _ x y w h) = spriteVertices x y w h textW textH

buildSpriteMap :: [GraphicsSprite] -> SpritesByName
buildSpriteMap =
  let namedTuple g@(GraphicsSprite (Sprite name _ _ _ _) _) = (name, g)
   in M.fromList . fmap namedTuple

createGraphicsSprite ::
     MonadIO m => BufferObject -> Int -> Sprite -> m GraphicsSprite
createGraphicsSprite buffer i sprite =
  pure GraphicsSprite <*> pure sprite <*> spriteVAO buffer i

spriteByName :: SpritesByName -> SpriteName -> Maybe VertexArrayObject
spriteByName sprites name =
  let vao (GraphicsSprite _ v) = v
   in fmap vao $ name `M.lookup` sprites

{-
############################
Warning: even worse than above, eye bleeding may occurs
VISUAL TESTING PURPOSES ONLY
############################
-}
debugDisplay ::
     (HasGameConfig s, HasGridSize s, HasGridDivision s)
  => s
  -> IORef GameState
  -> Program
  -> SpritesByName
  -> DisplayCallback
debugDisplay config gameStateRef program sprites = do
  Size w h <- GLUT.get windowSize
  clear [ColorBuffer, DepthBuffer]
  currentProgram $= Just program
  game <- liftIO $ readIORef gameStateRef
  -- TODO: totally fake so what ?
  let elapsedSeconds = 1 / fromIntegral fps
      nextGame = execState (runReaderT (updateGame elapsedSeconds) config) game
      -- TODO: not total fucker
      (Just shipVAOBlue) = sprites `spriteByName` "playerShip1_blue.png"
      (Just shipVAORed) = sprites `spriteByName` "playerShip1_red.png"
      (Just projVAOBlue) = sprites `spriteByName` "laserBlue05.png"
      (Just projVAORed) = sprites `spriteByName` "laserRed05.png"
      (Just baseVAOBlue) = sprites `spriteByName` "ufoBlue.png"
      (Just baseVAORed) = sprites `spriteByName` "ufoRed.png"
      display ::
           (HasPosition s, HasAngle s)
        => VertexArrayObject
        -> s
        -> DisplayCallback
      display = displaySprite config program (fromIntegral w) (fromIntegral h)
      displayEntity (EntityShip s) =
        let sVAO =
              case s ^. team of
                One -> shipVAOBlue
                Two -> shipVAORed
         in display sVAO s
      displayEntity (EntityProjectile p) =
        let pVAO =
              case p ^. team of
                One -> projVAOBlue
                Two -> projVAORed
         in display pVAO p
      displayEntity (EntityBase b) =
        let spVAO =
              case b ^. team of
                One -> baseVAOBlue
                Two -> baseVAORed
         in display spVAO b
  writeIORef gameStateRef nextGame
  traverse_ displayEntity $ eliminateSpatialGrid (nextGame ^. spatialGrid)
  currentProgram $= Nothing
  swapBuffers

displaySprite ::
     (HasGameConfig a, HasGridSize a, HasPosition s, HasAngle s)
  => a
  -> Program
  -> Float
  -> Float
  -> VertexArrayObject
  -> s
  -> DisplayCallback
displaySprite config program w h vao s
  -- 2D orthographic projection relative to the map size
 = do
  uniformP "mProjection" $ ortho2D (max (gs ^. _x) (gs ^. _y)) w h
  -- center the camera according to the map width/height
  uniformP "mView" $ translate2D (-(config ^. gridSize) / 2) identity
  -- we subtract pi/2 to the angle because
  -- of the sprite initial position in the texture
  uniformP "mModel" $
    rotate2D (s ^. angle - pi / 2) $
    translate2D (s ^. position) $ scale2D 2 identity
  bindVertexArrayObject $= Just vao
  spriteDraw
  bindVertexArrayObject $= Nothing
  where
    uniformP = uniform program
    gs = config ^. gridSize
