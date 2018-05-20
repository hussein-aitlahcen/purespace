-- GameState.hs ---

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

module PureSpace.Common.Game.State.GameState
  (
    HasPlayerOne(..)
  , HasPlayerTwo(..)
  , GameState(..)
  )
where

import           PureSpace.Common.Game.State.Player (Player)
import           PureSpace.Common.Lens              (Lens', lens)

-- | The state of the game, namely the current situation
-- of both players. Victory or loss condition can be inferred
-- from the situation of players at any given time.
data GameState = GameState Player Player

-- To ponder: maybe an `Ixed` would be better here ?
-- This might need to be changed. We will see what's more
-- convenient when writing the main loop.

class HasPlayerOne p where
  playerOne :: Lens' p Player

class HasPlayerTwo p where
  playerTwo :: Lens' p Player

instance HasPlayerOne GameState where
  playerOne =
    let f (GameState a _) = a
        g (GameState _ b) a = GameState a b
    in lens f g

instance HasPlayerTwo GameState where
  playerTwo =
    let f (GameState _ b) = b
        g (GameState a _) = GameState a
    in lens f g
