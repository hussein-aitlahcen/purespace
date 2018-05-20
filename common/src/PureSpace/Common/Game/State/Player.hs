-- Player.hs ---

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

module PureSpace.Common.Game.State.Player
  (
    Cash
  , HasBases (..)
  , HasCash (..)
  , HasPlayer (..)
  , HasShips (..)
  , Player (..)
  )
where

import           PureSpace.Common.Game.Ship.Types
import           PureSpace.Common.Game.Base.Types
import           PureSpace.Common.Lens            (Lens', lens)

-- | Currency to be spent on upgrades and units
type Cash = Integer

data Player = Player [Base] [Ship] Cash deriving Show

class HasPlayer p where
  player :: Lens' p Player

instance HasPlayer Player where
  player = id

class HasBases b where
  bases :: Lens' b [Base]

class HasShips s where
  ships :: Lens' s [Ship]

class HasCash c where
  cash :: Lens' c Cash

instance HasBases Player where
  bases =
    let f (Player a _ _) = a
        g (Player _ b c) a = Player a b c
    in lens f g

instance HasShips Player where
  ships =
    let f (Player _ b _) = b
        g (Player a _ c) b = Player a b c
    in lens f g

instance HasCash Player where
  cash =
    let f (Player _ _ c) = c
        g (Player a b _) = Player a b
    in lens f g
  
