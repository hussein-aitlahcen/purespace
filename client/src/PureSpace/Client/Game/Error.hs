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
    module PureSpace.Common.Resource.Error,
    module PureSpace.Client.Graphics.Error,
    GameError (..),
    AsGameError (..),
  )
  where

import           PureSpace.Client.Graphics.Error
import           PureSpace.Common.Resource.Error
import           PureSpace.Common.Lens           (Prism', prism)

data GameError = GameGraphicsError GraphicsError
               | GameResourceError ResourceError
               deriving Show

class AsGameError s where
  gameError :: Prism' s GameError

instance AsGameError GameError where
  gameError = id

instance AsResourceError GameError where
  resourceError =
    let f = GameResourceError
        g = \case
          GameResourceError x -> Right x
          x                   -> Left  x
    in prism f g

instance AsGraphicsError GameError where
  graphicsError =
    let f = GameGraphicsError
        g = \case
          GameGraphicsError x -> Right x
          x                   -> Left  x
    in prism f g

instance AsAssetError GameError where
  assetError = graphicsError . assetError

instance AsShaderError GameError where
  shaderError = graphicsError . shaderError

instance AsShaderProgramError GameError where
  shaderProgramError = graphicsError . shaderProgramError
