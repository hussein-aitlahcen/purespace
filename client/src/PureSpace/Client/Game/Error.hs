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

module PureSpace.Client.Game.Error
  (
    GameError (..),
    AsGameError (..),
    GraphicsError (..),
    AsGraphicsError (..),
    AssetError (..),
    AsAssetError (..),
    ShaderError (..),
    AsShaderError (..),
    ShaderProgramError (..),
    AsShaderProgramError (..)
  )
  where

import           PureSpace.Client.Assets         (AsAssetError (..),
                                                  AssetError (..))
import           PureSpace.Client.Graphics.Error (AsGraphicsError (..),
                                                  AsShaderError (..),
                                                  AsShaderProgramError (..),
                                                  GraphicsError (..),
                                                  ShaderError (..),
                                                  ShaderProgramError (..))
import           PureSpace.Common.Lens

data GameError = GameAssetError    AssetError
               | GameGraphicsError GraphicsError
               deriving Show

class AsGameError s where
  gameError :: Prism' s GameError

instance AsGameError GameError where
  gameError = id

instance AsAssetError GameError where
  assetError =
    let f = GameAssetError
        g = \case
          GameAssetError x -> Right x
          x                -> Left  x
    in prism f g

instance AsGraphicsError GameError where
  graphicsError =
    let f = GameGraphicsError
        g = \case
          GameGraphicsError x -> Right x
          x                   -> Left  x
    in prism f g

instance AsShaderError GameError where
  shaderError = graphicsError . shaderError

instance AsShaderProgramError GameError where
  shaderProgramError = graphicsError . shaderProgramError
