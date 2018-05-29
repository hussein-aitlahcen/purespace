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
    HasCash (..),
  )
  where

import           PureSpace.Common.Game.Base
import           PureSpace.Common.Game.Ship
import           PureSpace.Common.Lens      (Lens', lens)

-- | Currency to be spent on upgrades and units
type Cash = Integer

data PlayerState = PlayerState PlayerId Team Cash deriving Show

class HasPlayerState p where
  player :: Lens' p PlayerState

class HasCash c where
  cash :: Lens' c Cash

instance HasPlayerState PlayerState where
  player = id

instance HasPlayerId PlayerState where
  playerId =
    let f (PlayerState a _ _)   = a
        g (PlayerState _ b c) a = PlayerState a b c
    in lens f g

instance HasTeam PlayerState where
  team =
    let f (PlayerState _ b _)  = b
        g (PlayerState a _ c) b = PlayerState a b c
    in lens f g

instance HasCash PlayerState where
  cash =
    let f (PlayerState _ _ c)   = c
        g (PlayerState a b _) c = PlayerState a b c
    in lens f g

