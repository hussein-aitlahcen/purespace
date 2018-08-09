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
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module PureSpace.Common.Resource.Error
  ( ResourceError(..)
  , AsResourceError(..)
  ) where

import PureSpace.Common.Lens

newtype ResourceError =
  ResourceFileNotFound FilePath
  deriving (Show)

class AsResourceError s where
  resourceError :: Prism' s ResourceError
  resourceFileNotFound :: Prism' s FilePath
  resourceFileNotFound = resourceError . resourceFileNotFound

instance AsResourceError ResourceError where
  resourceError = id
  resourceFileNotFound =
    let f = ResourceFileNotFound
        g =
          \case
            ResourceFileNotFound x -> Right x
            x -> Left x
     in prism f g
