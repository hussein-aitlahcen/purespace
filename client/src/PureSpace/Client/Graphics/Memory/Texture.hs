-- Texture.hs ---
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

module PureSpace.Client.Graphics.Memory.Texture
  ( module PureSpace.Client.Graphics.Error
  , SpriteTexture(..)
  , createTexture
  ) where

import Codec.Picture (DynamicImage(..), Image(..), readImage)
import Data.Vector.Storable (unsafeWith)
import Graphics.UI.GLUT
  ( Clamping(..)
  , DataType(..)
  , PixelData(..)
  , PixelFormat(..)
  , PixelInternalFormat(..)
  , Proxy(..)
  , Repetition(..)
  , TextureCoordName(..)
  , TextureFilter(..)
  , TextureObject
  , TextureSize2D(..)
  , TextureTarget2D(..)
  , ($=)
  , genObjectName
  , generateMipmap'
  , texImage2D
  , textureBinding
  , textureFilter
  , textureWrapMode
  )
import PureSpace.Client.Graphics.Error
import PureSpace.Common.Prelude
import PureSpace.Common.Lens (MonadError, MonadIO, liftIO, throwing)

data SpriteTexture =
  SpriteTexture Int
                Int
                TextureObject

createTexture ::
     (MonadIO m, MonadError e m, AsResourceError e, AsGraphicsError e)
  => FilePath
  -> m SpriteTexture
createTexture imagePath = do
  img <- liftIO $ readImage imagePath
  case img of
    (Right (ImageRGBA8 (Image w h pixels))) ->
      pure (SpriteTexture w h) <*> go w h pixels
    (Left s) -> liftIO (putStrLn $ "Failed to create texture: " <> s) *> throwing resourceFileNotFound imagePath
    _ -> throwing graphicsTextureError imagePath
  where
    go h w pixels = do
      text <- genObjectName
      textureBinding Texture2D $= Just text
      textureWrapMode Texture2D S $= (Mirrored, ClampToBorder)
      textureWrapMode Texture2D T $= (Mirrored, ClampToBorder)
      textureFilter Texture2D $= ((Linear', Nothing), Linear')
      liftIO $
        unsafeWith pixels $ \ptr ->
          texImage2D
            Texture2D
            NoProxy
            0
            RGBA8
            (TextureSize2D (fromIntegral w) (fromIntegral h))
            0
            (PixelData RGBA UnsignedByte ptr)
      liftIO $ generateMipmap' Texture2D
      textureBinding Texture2D $= Nothing
      pure text
