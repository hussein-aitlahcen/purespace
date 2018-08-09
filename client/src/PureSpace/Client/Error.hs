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

module PureSpace.Client.Error
  ( module PureSpace.Common.Resource.Error
  , module PureSpace.Client.Graphics.Error
  , ClientError(..)
  , AsClientError(..)
  ) where

import PureSpace.Client.Graphics.Error
import PureSpace.Common.Lens (Prism', prism)
import PureSpace.Common.Resource.Error

data ClientError
  = ClientGraphicsError GraphicsError
  | ClientResourceError ResourceError
  deriving (Show)

class AsClientError s where
  gameError :: Prism' s ClientError

instance AsClientError ClientError where
  gameError = id

instance AsResourceError ClientError where
  resourceError =
    let f = ClientResourceError
        g =
          \case
            ClientResourceError x -> Right x
            x -> Left x
     in prism f g

instance AsGraphicsError ClientError where
  graphicsError =
    let f = ClientGraphicsError
        g =
          \case
            ClientGraphicsError x -> Right x
            x -> Left x
     in prism f g

instance AsAssetError ClientError where
  assetError = graphicsError . assetError

instance AsShaderError ClientError where
  shaderError = graphicsError . shaderError

instance AsShaderProgramError ClientError where
  shaderProgramError = graphicsError . shaderProgramError
