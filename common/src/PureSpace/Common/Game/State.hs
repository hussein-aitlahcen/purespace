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

module PureSpace.Common.Game.State
  (
    module PureSpace.Common.Game.Player.State,
    GameState (..),
    HasGameState (..),
    HasSpatialGrid (..),
    HasPlayers (..),
    initialGameState
  )
  where

import           PureSpace.Common.Game.Collision
import           PureSpace.Common.Game.Entity
import           PureSpace.Common.Game.Config
import           PureSpace.Common.Game.Player.State
import           PureSpace.Common.Lens              (Lens', lens)

-- | The state of the game, namely the current situation
-- of the players. Victory or loss condition can be inferred
-- from the situation of players at any given time.
data GameState = GameState (Grid Entity) [PlayerState]

initialGameState :: GameConfig -> GameState
initialGameState (GameConfig gs gd)= GameState (createSpatialGrid gs gd []) []

-- To ponder: maybe an `Ixed` would be better here ?
-- This might need to be changed. We will see what's more
-- convenient when writing the main loop.

class HasGameState g where
  gameState :: Lens' g GameState

instance HasGameState GameState where
  gameState = id

class HasSpatialGrid s where
  spatialGrid :: Lens' s (Grid Entity)

class HasPlayers p where
  players :: Lens' p [PlayerState]

instance HasPlayers GameState where
  players =
    let f (GameState _ b)   = b
        g (GameState a _) b = GameState a b
    in lens f g

instance HasSpatialGrid GameState where
  spatialGrid =
    let f (GameState a _)   = a
        g (GameState _ b) a = GameState a b
    in lens f g
