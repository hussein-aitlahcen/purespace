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

import           Data.List                       as L
import           Graphics.UI.GLUT                as GLUT hiding (ortho2D,
                                                          rotate, uniform)
import           PureSpace.Client.Assets.Sprites
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

createGameWindow :: (MonadIO m,
                     MonadState s m,
                     MonadError e m,
                     HasShaderState s,
                     HasShaderProgramState s,
                     AsGraphicsError e,
                     AsAssetError e,
                     AsShaderError e,
                     AsShaderProgramError e)
                 => m ()
createGameWindow = do
  initialContextVersion $= openGLVersion
  atlas                    <- loadAtlas
  (_, _)                   <- getArgsAndInitialize
  window                   <- createWindow "PureSpace"
  (text, sprites, program) <- initContext atlas
  let (Just ship) = L.find (\(GraphicsSprite (Sprite name _ _ _ _) _) -> name == "playerShip1_blue.png") sprites
  displayCallback $= display program text ship
  idleCallback    $= Just (postRedisplay (Just window))
  mainLoop

initContext :: (MonadIO m,
                MonadState s m,
                MonadError e m,
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
  (SpriteTexture w h text) <- createTexture image
  graphicsSprites          <- traverse (initSpriteBuffer w h) sprites
  pure (text, graphicsSprites, program)

initSpriteBuffer :: MonadIO m => Int -> Int -> Sprite -> m GraphicsSprite
initSpriteBuffer imgW imgH sprite@(Sprite _ x y w h) =
  pure GraphicsSprite
  <*> pure sprite
  <*> spriteVAO triangles
  where
    norm a b = fromIntegral a / fromIntegral b
    nX = norm x imgW
    nY = norm y imgH
    nW = norm w imgW
    nH = norm h imgH
    triangles =
      [
        -- vertexX, vertexY, textureX, textureY,
        -- interpreted as vec4 by the vertex shader
        -nW/2,  nH/2, nX, nY,
         nW/2,  nH/2, nX + nW, nY,
         nW/2, -nH/2, nX + nW, nY + nH,

        -nW/2,  nH/2, nX, nY,
         nW/2, -nH/2, nX + nW, nY + nH,
        -nW/2, -nH/2, nX, nY + nH
      ]

display :: Program -> TextureObject -> GraphicsSprite -> DisplayCallback
display program text (GraphicsSprite _ vao) = do
  Size width height <- GLUT.get windowSize
  clear [ColorBuffer, DepthBuffer]
  time <- elapsedTime
  currentProgram           $= Just program
  textureBinding Texture2D $= Just text
  uniformP "mProjection" (ortho2D 1 width height)
  uniformP "mModelView"  (rotate (fromIntegral time / 360) identity)
  bindVertexArrayObject $= Just vao
  drawArrays Triangles 0 6
  bindVertexArrayObject $= Nothing
  currentProgram        $= Nothing
  swapBuffers
  where
    uniformP = uniform program
