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
  ( module PureSpace.Common.Game.Player.State
  , GameState(..)
  , HasGameState(..)
  , HasSpatialGrid(..)
  , HasPlayers(..)
  , HasNextObjectId(..)
  , initialGameState
  ) where

import PureSpace.Common.Game.Collision
import PureSpace.Common.Game.Config
import PureSpace.Common.Game.Entity
import PureSpace.Common.Game.Player.State
import PureSpace.Common.Lens (Lens', lens)

-- | The state of the game, namely the current situation
-- of the players. Victory or loss condition can be inferred
-- from the situation of players at any given time.
data GameState =
  GameState (Grid Entity)
            [PlayerState]
            [Entity]
            ObjectId

initialGameState :: GameConfig -> GameState
initialGameState (GameConfig gs gd) =
  GameState (createSpatialGrid gs gd []) [] [] 0

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

class HasNextObjectId p where
  nextObjectId :: Lens' p ObjectId

instance HasPlayers GameState where
  players =
    let f (GameState _ b _ _) = b
        g (GameState a _ c d) b = GameState a b c d
     in lens f g

instance HasSpatialGrid GameState where
  spatialGrid =
    let f (GameState a _ _ _) = a
        g (GameState _ b c d) a = GameState a b c d
     in lens f g

instance HasEntities GameState where
  entities =
    let f (GameState _ _ c _) = c
        g (GameState a b _ d) c = GameState a b c d
     in lens f g

instance HasNextObjectId GameState where
  nextObjectId =
    let f (GameState _ _ _ d) = d
        g (GameState a b c _) d = GameState a b c d
     in lens f g
