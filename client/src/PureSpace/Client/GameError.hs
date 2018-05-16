-- GameError.hs ---

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

module PureSpace.Client.GameError
  (
    GameError (..),
    AsGameError (..),
    AssetError (..),
    AsAssetError (..),
    ShaderError (..),
    AsShaderError (..),
    ShaderProgramError (..),
    AsShaderProgramError (..)
  )
  where

import           PureSpace.Client.Assets
import           PureSpace.Client.Shader
import           PureSpace.Client.ShaderProgram
import           PureSpace.Common.Lens

data GameError = GameAssetError  AssetError
               | GameShaderError ShaderError
               | GameShaderProgramError ShaderProgramError
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

instance AsShaderError GameError where
  shaderError =
    let f = GameShaderError
        g = \case
          GameShaderError x -> Right x
          x                 -> Left  x
    in prism f g

instance AsShaderProgramError GameError where
   shaderProgramError =
     let f = GameShaderProgramError
         g = \case
           GameShaderProgramError x -> Right x
           x                        -> Left  x
     in prism f g
