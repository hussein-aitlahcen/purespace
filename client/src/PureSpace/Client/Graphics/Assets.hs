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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module PureSpace.Client.Graphics.Assets
  (
    module PureSpace.Client.Graphics.Assets.Error,
    module PureSpace.Common.Resource,
    Sprite (..),
    SpriteAtlas (..),
    assetsPath,
    spritesPath,
    spriteSheetPath,
    loadAssetJSON
  )
  where

import           Data.Aeson                             (FromJSON, eitherDecode,
                                                         parseJSON, withObject,
                                                         (.:))
import qualified Data.ByteString.Lazy                   as BS
import           PureSpace.Client.Graphics.Assets.Error
import           PureSpace.Common.Lens                  (MonadError, MonadIO,
                                                         throwing)
import           PureSpace.Common.Prelude
import           PureSpace.Common.Resource

data Sprite = Sprite String Int Int Int Int deriving Show

data SpriteAtlas = SpriteAtlas String [Sprite] deriving Show

instance FromJSON Sprite where
  parseJSON = withObject "sprite" $ \v ->
    pure Sprite
    <*> v .: "name"
-- TODO: make literal numbers please
    <*> (read <$> v .: "x")
    <*> (read <$> v .: "y")
    <*> (read <$> v .: "width")
    <*> (read <$> v .: "height")

instance FromJSON SpriteAtlas where
  parseJSON = withObject "sprite-atlas" $ \v ->
    pure SpriteAtlas
    <*> (preprendPath <$> v .: "atlas")
    <*> v .: "sprites"
    where
      preprendPath = ((spritesPath <> "/") <>)

assetsPath :: String
assetsPath = "./assets"

spriteSheetPath :: FilePath
spriteSheetPath = spritesPath <> "/sheet.json"

spritesPath :: String
spritesPath = assetsPath <> "/Spritesheet"

loadAssetJSON :: (MonadIO m,
                  MonadError e m,
                  AsResourceError e,
                  AsAssetError e,
                  FromJSON a)
              => AssetEntry -> m a
loadAssetJSON entry@(_, path) = do
  content <- loadResource BS.readFile path
  case eitherDecode content of
    Right asset  -> pure asset
    Left message -> throwing assetDecodingFailure (entry, message)
