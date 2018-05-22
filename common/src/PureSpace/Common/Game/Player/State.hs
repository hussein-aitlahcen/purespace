-- State.hs ---

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

module PureSpace.Common.Game.Player.State
  (
    module PureSpace.Common.Game.Base,
    module PureSpace.Common.Game.Ship,
    PlayerState (..),
    Cash,
    HasPlayerState (..),
    HasTeam (..),
    HasBases (..),
    HasCash (..),
    HasShips (..),
  )
  where

import           PureSpace.Common.Game.Base
import           PureSpace.Common.Game.Ship
import           PureSpace.Common.Lens      (Lens', lens)

-- | Currency to be spent on upgrades and units
type Cash   = Integer

data PlayerState = PlayerState Team [Base] [Ship] Cash deriving Show

class HasPlayerState p where
  playerState :: Lens' p PlayerState

class HasBases b where
  bases :: Lens' b [Base]

class HasShips s where
  ships :: Lens' s [Ship]

class HasCash c where
  cash :: Lens' c Cash

instance HasPlayerState PlayerState where
  playerState = id

instance HasTeam PlayerState where
  team =
    let f (PlayerState a _ _ _)   = a
        g (PlayerState _ b c d) a = PlayerState a b c d
    in lens f g

instance HasBases PlayerState where
  bases =
    let f (PlayerState _ b _ _)   = b
        g (PlayerState a _ c d) b = PlayerState a b c d
    in lens f g

instance HasShips PlayerState where
  ships =
    let f (PlayerState _ _ c _)   = c
        g (PlayerState a b _ d) c = PlayerState a b c d
    in lens f g

instance HasCash PlayerState where
  cash =
    let f (PlayerState _ _ _ d)   = d
        g (PlayerState a b c _) d = PlayerState a b c d
    in lens f g
