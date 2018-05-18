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

module PureSpace.Client.Graphics.Assets.Error
  (
    module PureSpace.Common.Resource.Error,
    AssetName,
    AssetEntry,
    AssetError (..),
    AsAssetError (..)
  )
  where

import           PureSpace.Common.Lens           (Prism', prism)
import           PureSpace.Common.Resource.Error

type AssetName  = String
type AssetEntry = (AssetName, FilePath)

newtype AssetError = AssetDecodingFailed (AssetEntry, String) deriving Show

class AsAssetError s where
  assetError           :: Prism' s AssetError
  assetDecodingFailure :: Prism' s (AssetEntry, String)
  assetDecodingFailure = assetError . assetDecodingFailure

instance AsAssetError AssetError where
  assetError = id
  assetDecodingFailure =
    let f = AssetDecodingFailed
        g = \case
          AssetDecodingFailed entry -> Right entry
          x                         -> Left x
    in prism f g
