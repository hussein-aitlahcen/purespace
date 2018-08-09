-- Resource.hs ---
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

module PureSpace.Common.Resource
  ( module PureSpace.Common.Resource.Error
  , loadResource
  ) where

import PureSpace.Common.Files (doesFileExist)
import PureSpace.Common.Lens (MonadError, MonadIO, liftIO, throwing)
import PureSpace.Common.Prelude
import PureSpace.Common.Resource.Error

loadResource ::
     (MonadIO m, MonadError e m, AsResourceError e)
  => (FilePath -> IO a)
  -> FilePath
  -> m a
loadResource f path = do
  fileExists <- liftIO $ doesFileExist path
  bool fileExists (liftIO $ f path) (throwing resourceFileNotFound path)
