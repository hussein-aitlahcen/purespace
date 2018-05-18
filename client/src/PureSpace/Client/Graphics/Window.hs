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

import           Graphics.GLUtil.BufferObjects (makeBuffer)
import           Graphics.UI.GLUT              as GLUT hiding (ortho2D, rotate,
                                                        translate, uniform)
import qualified Data.Map                        as M
import           PureSpace.Client.Graphics
import           PureSpace.Common.Concurrent
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
  displayCallback $= display program sprites
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
  sampleAlphaToOne            $= Enabled
  sampleAlphaToCoverage       $= Enabled
  depthBounds                 $= Nothing
  depthFunc                   $= Nothing
  (SpriteTexture textW textH text) <- createTexture image
  let vertices = initVertices textW textH =<< sprites
  buffer          <- liftIO $ makeBuffer ArrayBuffer vertices
  graphicsSprites <- traverse (uncurry (createGraphicsSprite buffer)) (enumerate sprites)
  currentProgram           $= Just program
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

display :: Program -> SpritesByName -> DisplayCallback
display program sprites = do
  Size w h          <- GLUT.get windowSize
  time              <- elapsedTime
  clear [ColorBuffer, DepthBuffer]
  let orangeShip = "playerShip3_orange.png" `vaoByName` sprites
  maybe (pure ()) (displaySprite (projection w h) time) orangeShip
  swapBuffers
  postRedisplay Nothing
  where
    uniformP      = uniform program
    projection    = ortho2D 1
    modelView t = rotate (fromIntegral t / 360) identity
    displaySprite proj time vao = do
      uniformP "mProjection"  proj
      uniformP "mModelView" $ modelView time
      bindVertexArrayObject $= Just vao
      spriteDraw
      bindVertexArrayObject $= Nothing

vaoByName :: SpriteName-> SpritesByName -> Maybe VertexArrayObject
vaoByName name sprites =
  let vao (GraphicsSprite _ v) = v
  in fmap vao $ name `M.lookup` sprites
