-- Fleet.hs ---
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
module PureSpace.Common.Game.Fleet
  ( module PureSpace.Common.Game.Types
  , Fleet(..)
  , FleetCaracteristics(..)
  , FleetComposition(..)
  , HasFleets(..)
  , HasCost(..)
  , HasFleetCaracteristics(..)
  , HasFleetCompositions(..)
  , HasRespawnCooldown(..)
  , HasShipAmount(..)
  ) where

import PureSpace.Common.Game.Ship
  ( HasShipCaracteristics(..)
  , ShipCaracteristics
  )
import PureSpace.Common.Game.Types
import PureSpace.Common.Lens (Lens', lens)

-- | Maybe this would be expressed more elegantly using a Map ?
data FleetComposition =
  FleetComposition ShipCaracteristics
                   ShipAmount
  deriving (Eq, Ord, Show)

data FleetCaracteristics =
  FleetCaracteristics [FleetComposition]
                      RespawnCooldown
                      Cost
  deriving (Eq, Ord, Show)

data Fleet =
  Fleet FleetCaracteristics
        RespawnCooldown
  deriving (Eq, Ord, Show)

class HasFleets h where
  fleets :: Lens' h [Fleet]

class HasCost c where
  cost :: Lens' c Cost

class HasFleetCaracteristics f where
  fleetCaracteristics :: Lens' f FleetCaracteristics

class HasShipAmount s where
  shipAmount :: Lens' s ShipAmount

class HasFleetCompositions s where
  fleetComposition :: Lens' s [FleetComposition]

instance HasCost FleetCaracteristics where
  cost =
    let f (FleetCaracteristics _ _ c) = c
        g (FleetCaracteristics a b _) = FleetCaracteristics a b
     in lens f g

instance HasRespawnCooldown FleetCaracteristics where
  respawnCooldown =
    let f (FleetCaracteristics _ b _) = b
        g (FleetCaracteristics a _ c) b = FleetCaracteristics a b c
     in lens f g

instance HasFleetCaracteristics Fleet where
  fleetCaracteristics =
    let f (Fleet a _) = a
        g (Fleet _ b) a = Fleet a b
     in lens f g

instance HasRespawnCooldown Fleet where
  respawnCooldown =
    let f (Fleet _ b) = b
        g (Fleet a _) b = Fleet a b
     in lens f g

instance HasFleetCompositions FleetCaracteristics where
  fleetComposition =
    let f (FleetCaracteristics a _ _) = a
        g (FleetCaracteristics _ b c) a = FleetCaracteristics a b c
     in lens f g

instance HasShipAmount FleetComposition where
  shipAmount =
    let f (FleetComposition _ b) = b
        g (FleetComposition a _) = FleetComposition a
     in lens f g

instance HasShipCaracteristics FleetComposition where
  shipCaracteristics =
    let f (FleetComposition a _) = a
        g (FleetComposition _ b) a = FleetComposition a b
     in lens f g
