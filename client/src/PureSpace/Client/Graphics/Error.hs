-- Error.hs ---
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
{-# LANGUAGE LambdaCase #-}

module PureSpace.Client.Graphics.Error
  ( module PureSpace.Client.Graphics.Assets.Error
  , module PureSpace.Client.Graphics.Program.Error
  , GraphicsError(..)
  , AsGraphicsError(..)
  ) where

import PureSpace.Client.Graphics.Assets.Error
import PureSpace.Client.Graphics.Program.Error
import PureSpace.Common.Lens (Prism', prism)

data GraphicsError
  = GraphicsAssetError AssetError
  | GraphicsShaderProgramError ShaderProgramError
  | GraphicsTextureError FilePath
  deriving (Show)

class AsGraphicsError s where
  graphicsError :: Prism' s GraphicsError
  graphicsTextureError :: Prism' s FilePath
  graphicsTextureError = graphicsError . graphicsTextureError

instance AsGraphicsError GraphicsError where
  graphicsError = id
  graphicsTextureError =
    let f = GraphicsTextureError
        g =
          \case
            GraphicsTextureError x -> Right x
            x -> Left x
     in prism f g

instance AsAssetError GraphicsError where
  assetError =
    let f = GraphicsAssetError
        g =
          \case
            GraphicsAssetError x -> Right x
            x -> Left x
     in prism f g

instance AsShaderProgramError GraphicsError where
  shaderProgramError =
    let f = GraphicsShaderProgramError
        g =
          \case
            GraphicsShaderProgramError x -> Right x
            x -> Left x
     in prism f g

instance AsShaderError GraphicsError where
  shaderError = shaderProgramError . shaderError
