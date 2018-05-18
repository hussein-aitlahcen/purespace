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

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Data.List                     as L
import           Graphics.GLUtil.BufferObjects (makeBuffer)
import           Graphics.UI.GLUT              as GLUT hiding (ortho2D, rotate,
                                                        uniform)
import           PureSpace.Client.Graphics
import           PureSpace.Common.Monad
import           PureSpace.Common.Prelude

{-
############################
Everything under this line is complete garbage atm
############################
-}

data GraphicsSprite = GraphicsSprite Sprite VertexArrayObject deriving Show

openGLVersion :: (Int, Int)
openGLVersion = (3, 3)

fps :: Int
fps = 120

createGameWindow :: (MonadIO m,
                     MonadState s m,
                     MonadError e m,
                     AsResourceError e,
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
  (text, sprites, program) <- initContext atlas
  let (Just ship) = L.find (\(GraphicsSprite (Sprite name _ _ _ _) _) -> name == "playerShip3_orange.png") sprites
  displayCallback $= display program text ship
  idleCallback    $= Just (postRedisplay (Just window))
  inputChan     <- liftIO $ atomically newBroadcastTChan
  keyboardMouseCallback $= Just (inputStream inputChan)
  readInputChan <- liftIO $ atomically $ dupTChan inputChan
  liftIO $ forkIO $ loop readInputChan
  windowLoop
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
            -> m (TextureObject, [GraphicsSprite], Program)
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
  pure (text, graphicsSprites, program)

initVertices :: Int -> Int -> Sprite -> [GLfloat]
initVertices textW textH (Sprite _ x y w h) =
  spriteVertices x y w h textW textH

createGraphicsSprite :: MonadIO m => BufferObject -> Int -> Sprite -> m GraphicsSprite
createGraphicsSprite buffer i sprite =
  pure GraphicsSprite
  <*> pure sprite
  <*> spriteVAO buffer i

display :: Program -> TextureObject -> GraphicsSprite -> DisplayCallback
display program text (GraphicsSprite _ vao) = do
  Size width height <- GLUT.get windowSize
  time              <- elapsedTime
  clear [ColorBuffer, DepthBuffer]
  currentProgram           $= Just program
  textureBinding Texture2D $= Just text
  uniformP "mProjection" $ projection width height
  uniformP "mModelView"  $ modelView time
  bindVertexArrayObject $= Just vao
  drawArrays Triangles 0 6
  bindVertexArrayObject $= Nothing
  currentProgram        $= Nothing
  swapBuffers
  postRedisplay Nothing
  where
    uniformP    = uniform program
    projection  = ortho2D 1
    modelView t = rotate (fromIntegral t / 360) identity
