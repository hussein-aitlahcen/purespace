-- Assets.hs ---

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

module PureSpace.Client.Assets
  (
    AssetError (..),
    AsAssetError (..),
    assetsPath,
    spritesPath,
    loadAsset,
    loadAssetJSON
  )
  where

import           Data.Aeson               (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy     as BS
import           PureSpace.Common.Prelude
import           PureSpace.Common.Lens
import           PureSpace.Common.Files

data AssetError = AssetNotFound       String
                | AssetDecodingFailed String
                deriving Show

class AsAssetError s where
  assetError           :: Prism' s AssetError
  assetNotFound        :: Prism' s String
  assetDecodingFailure :: Prism' s String
  assetNotFound        = assetError . assetNotFound
  assetDecodingFailure = assetError . assetDecodingFailure

instance AsAssetError AssetError where
  assetError = id
  assetNotFound =
    let f = AssetNotFound
        g = \case
          AssetNotFound message -> Right message
          x                     -> Left x
    in prism f g
  assetDecodingFailure =
    let f = AssetDecodingFailed
        g = \case
          AssetDecodingFailed message -> Right message
          x                           -> Left x
    in prism f g

assetsPath :: String
assetsPath = "./assets"

spritesPath :: String
spritesPath = assetsPath <> "/Spritesheet"

loadAsset :: (MonadIO m, MonadError e m, AsAssetError e) => (FilePath -> IO a) -> FilePath -> m a
loadAsset f asset = do
  fileExists <- liftIO $ doesFileExist asset
  bool fileExists
    (liftIO $ f asset)
    (throwing assetNotFound asset)

loadAssetJSON :: (MonadIO m, MonadError e m, AsAssetError e, FromJSON a) => FilePath -> m a
loadAssetJSON path = do
  content <- loadAsset BS.readFile path
  case eitherDecode content of
    Right asset  -> pure asset
    Left message -> throwing assetDecodingFailure message
