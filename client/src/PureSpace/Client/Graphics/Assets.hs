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
{-# LANGUAGE OverloadedStrings #-}

module PureSpace.Client.Graphics.Assets
  ( module PureSpace.Client.Graphics.Assets.Error
  , module PureSpace.Common.Resource
  , Sprite(..)
  , SpriteAtlas(..)
  , spritesAtlasPath
  , spritesAtlasDefinitionPath
  , loadSpritesAtlasDefinition
  , loadAssetJSON
  ) where

import Data.Aeson (FromJSON, (.:), eitherDecode, parseJSON, withObject)
import qualified Data.ByteString.Lazy as BS
import Paths_purespace
import PureSpace.Client.Graphics.Assets.Error
import PureSpace.Common.Lens (MonadError, MonadIO, throwing)
import PureSpace.Common.Monad
import PureSpace.Common.Prelude
import PureSpace.Common.Resource

data Sprite =
  Sprite String -- name
         Int -- x
         Int -- y
         Int -- width
         Int -- height
  deriving (Show)

newtype SpriteAtlas =
  SpriteAtlas [Sprite]
  deriving (Show)

instance FromJSON Sprite where
  parseJSON =
    withObject "sprite" $ \v ->
      pure Sprite <*> v .: "name" <*> (read <$> v .: "x") <*>
      (read <$> v .: "y") <*>
      (read <$> v .: "width") <*>
      (read <$> v .: "height")

-- TODO: make literal numbers please
instance FromJSON SpriteAtlas where
  parseJSON = withObject "sprite-atlas" $ \v -> SpriteAtlas <$> v .: "sprites"

assetsDirectory :: String
assetsDirectory = "assets"

spritesDirectory :: String
spritesDirectory = assetsDirectory <> "/Spritesheet"

spritesAtlasDefinitionPath :: MonadIO m => m FilePath
spritesAtlasDefinitionPath = liftIO $ getDataFileName $ spritesDirectory <> "/sheet.json"

spritesAtlasPath :: MonadIO m => m FilePath
spritesAtlasPath = liftIO $ getDataFileName $ spritesDirectory <> "/sheet.png"

loadAssetJSON ::
     (MonadIO m, MonadError e m, AsResourceError e, AsAssetError e, FromJSON a)
  => AssetEntry
  -> m a
loadAssetJSON entry@(_, path) = do
  content <- loadResource BS.readFile path
  case eitherDecode content of
    Right asset -> pure asset
    Left message -> throwing assetDecodingFailure (entry, message)

loadSpritesAtlasDefinition ::
     (MonadIO m, MonadError e m, AsResourceError e, AsAssetError e)
  => m SpriteAtlas
loadSpritesAtlasDefinition =
  loadAssetJSON =<< pure (,) <*> pure "Sprite Atlas" <*> spritesAtlasDefinitionPath
