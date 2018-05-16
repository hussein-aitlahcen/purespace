-- GameWindow.hs ---

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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module PureSpace.Client.GameWindow
  (
    runApp
  )
  where

import           Codec.Picture
import           Data.List
import           Data.Vector.Storable
import           Graphics.UI.GLUT
import           PureSpace.Client.Shaders
import           PureSpace.Client.Sprites
import           PureSpace.Common.Lens
import           PureSpace.Common.Prelude


data AppConfig = AppConfig () deriving Show
data AppState = AppState () deriving Show
data AppError = AppAssetError AssetError
              | AppShaderError ShaderError
              deriving Show

newtype App a = App (ExceptT AppError (ReaderT AppConfig (StateT AppState IO)) a)
    deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadIO, MonadState AppState)

class AsAppError s where
  appError :: Prism' s AppError

instance AsAppError AppError where
  appError = id

instance AsAssetError AppError where
  assetError =
    let f = AppAssetError
        g = \case
          AppAssetError x -> Right x
          y               -> Left y
    in prism f g

instance AsShaderError AppError where
  shaderError =
    let f = AppShaderError
        g = \case
          AppShaderError x -> Right x
          y                -> Left y
    in prism f g

runApp :: IO ()
runApp = do
  result <- evalStateT (runReaderT (runExceptT createGameWindow) appConfig) appState
  case result of
    Left message -> print (message :: AppError)
    Right _      -> putStrLn "Unseen string"
  where
    appState  = AppState ()
    appConfig = AppConfig ()

{-
############################
Everything under this line is complete garbage atm
############################
-}

data DrawableSprite = DrawableSprite Sprite GLfloat GLfloat GLfloat GLfloat deriving Show

createGameWindow :: (MonadIO m, MonadError e m, AsAppError e, AsAssetError e, AsShaderError e) => m ()
createGameWindow = do
  atlas <- loadAtlas
  (_, _) <- getArgsAndInitialize
  window  <- createWindow "PureSpace"
  (text, sprite) <- initContext atlas
  liftIO $ print sprite
  displayCallback $= display text sprite
  reshapeCallback $= Just reshape
  idleCallback    $= Just (postRedisplay (Just window))
  mainLoop

reshape :: ReshapeCallback
reshape s@(Size width height) = do
  putStrLn "reshaped"
  viewport   $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  bool (width > height)
    (ortho2D (-visibleArea*aspectRatio) (visibleArea*aspectRatio) (-visibleArea)             visibleArea)
    (ortho2D (-visibleArea)             visibleArea               (-visibleArea/aspectRatio) (visibleArea/aspectRatio))
  where
    visibleArea = 600
    aspectRatio = fromIntegral width / fromIntegral height

initContext :: (MonadIO m, MonadError e m, AsAppError e, AsAssetError e, AsShaderError e) => SpriteAtlas -> m (TextureObject, DrawableSprite)
initContext (SpriteAtlas image sprites) = do
  liftIO $ putStrLn "initialize"
  initialDisplayMode          $= [DoubleBuffered]
  blend                       $= Enabled
  sampleAlphaToOne            $= Enabled
  sampleAlphaToCoverage       $= Enabled
  depthBounds                 $= Nothing
  depthFunc                   $= Nothing
  texture Texture2D           $= Enabled
  texObject <- genObjectName
  textureBinding  Texture2D   $= Just texObject
  textureWrapMode Texture2D S $= (Mirrored, ClampToBorder)
  textureWrapMode Texture2D T $= (Mirrored, ClampToBorder)
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  (Right (ImageRGBA8 (Image width height pixels))) <- liftIO $ readImage image
  liftIO $ unsafeWith pixels $ \ptr ->
    texImage2D
      Texture2D
      NoProxy
      0
      RGBA8
      (TextureSize2D (fromIntegral width) (fromIntegral height))
      0
      (PixelData RGBA UnsignedByte ptr)
  let (Just s@(Sprite _ x y w h)) = Data.List.find (\(Sprite name _ _ _ _) -> name == "playerShip1_blue.png") sprites
  pure (texObject, DrawableSprite s (fromIntegral x/fromIntegral width) (fromIntegral y/fromIntegral height) (fromIntegral w/fromIntegral width) (fromIntegral h/fromIntegral height))

display :: TextureObject -> DrawableSprite -> DisplayCallback
display text sprite = do
  clear [ColorBuffer, DepthBuffer]
  texture        Texture2D $= Enabled
  textureBinding Texture2D $= Just text
  time <- elapsedTime
  let
      py = (fromIntegral time / 1000) * 30
      texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
      vertex3f = vertex :: Vertex3 GLfloat -> IO ()
      renderSprite (DrawableSprite (Sprite _ ox oy ow oh) x y w h) p =
        renderPrimitive Quads $ do
          texCoord2f (TexCoord2 x y)
          vertex3f (Vertex3 (-fromIntegral ow/2) (fromIntegral oh/2 + p) 0)
          texCoord2f (TexCoord2 (x + w) y)
          vertex3f (Vertex3 (fromIntegral ow/2) (fromIntegral oh/2 + p) 0)
          texCoord2f (TexCoord2 (x + w) (y + h))
          vertex3f (Vertex3 (fromIntegral ow/2) (-fromIntegral oh/2 + p) 0)
          texCoord2f (TexCoord2 x (y + h))
          vertex3f (Vertex3 (-fromIntegral ow/2) (-fromIntegral oh/2 + p) 0)
  renderSprite sprite py
  texture Texture2D           $= Disabled
  swapBuffers
