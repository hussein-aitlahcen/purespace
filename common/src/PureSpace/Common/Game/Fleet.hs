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
  (
    Fleet (..),
    FleetCaracteristics (..),
    FleetComposition (..),
    ShipAmount,
    RespawnCooldown,
    Cost,
    HasFleet (..),
    HasCost (..),
    HasFleetCaracteristics (..),
    HasRespawnCooldown (..),
    HasShipAmount (..)
  ) where

import           Numeric.Natural
import           PureSpace.Common.Game.Ship (ShipCaracteristics, HasShipCaracteristics(..))
import           PureSpace.Common.Lens      (Lens', lens)

type ShipAmount      = Natural
type RespawnCooldown = Natural
type Cost            = Natural

-- | Maybe this would be expressed more elegantly using a Map ?
data FleetComposition = FleetComposition ShipCaracteristics ShipAmount deriving (Eq, Ord, Show)

data FleetCaracteristics =
  FleetCaracteristics [FleetComposition] RespawnCooldown Cost
  deriving (Eq, Ord, Show)

newtype Fleet = Fleet [FleetCaracteristics] deriving (Eq, Ord, Show)

class HasFleet h where
  fleet :: Lens' h Fleet

class HasCost c where
  cost :: Lens' c Cost

class HasRespawnCooldown r where
  respawnCooldown :: Lens' r RespawnCooldown

class HasFleetCaracteristics f where
  fleetCaracteristics :: Lens' f [FleetCaracteristics]

class HasShipAmount s where
  shipAmount :: Lens' s ShipAmount

instance HasFleet Fleet where
  fleet = id

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
    let f (Fleet a) = a
        g _ = Fleet
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
