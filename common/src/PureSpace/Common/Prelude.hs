-- Prelude.hs ---

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

module PureSpace.Common.Prelude
  (
    module Data.Semigroup,
    safeHead,
    bool,
    traverse_,
    (&&&),
    (***),
    Kleisli (..),
    enumerate
  )
  where

import           Control.Arrow          (Kleisli (..), (&&&), (***))
import           Data.Foldable          (traverse_)
import           Data.Semigroup

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

bool :: Bool -> a -> a -> a
bool cond true false = if cond then true else false

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]
