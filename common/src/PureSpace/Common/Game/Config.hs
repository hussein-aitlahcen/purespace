-- Config.hs ---

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

module PureSpace.Common.Game.Config
  (
    GameConfig (..),
    HasGameConfig (..),
    HasGridSize (..),
    HasGridDivision (..)
  )
  where

import           PureSpace.Common.Game.Collision
import           PureSpace.Common.Lens

data GameConfig = GameConfig GridSize GridDivision

class HasGameConfig s where
  gameConfig :: Lens' s GameConfig

class HasGridSize s where
  gridSize :: Lens' s GridSize

class HasGridDivision s where
  gridDivision :: Lens' s GridDivision

instance HasGameConfig GameConfig where
  gameConfig = id

instance HasGridSize GameConfig where
  gridSize =
    let f (GameConfig a _)   = a
        g (GameConfig _ b) a = GameConfig a b
    in lens f g

instance HasGridDivision GameConfig where
  gridDivision =
    let f (GameConfig _ b)   = b
        g (GameConfig a _) b = GameConfig a b
    in lens f g
