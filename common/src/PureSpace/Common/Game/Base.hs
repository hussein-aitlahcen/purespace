-- Base.hs ---
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
module PureSpace.Common.Game.Base
  ( module PureSpace.Common.Game.Types
  , module PureSpace.Common.Game.Fleet
  , Base(..)
  , BaseCaracteristics(..)
  , HasIncome(..)
  , HasIsHeadquarter(..)
  , HasBase(..)
  , HasBaseType(..)
  , Income
  , IsHeadquarter
  ) where

import PureSpace.Common.Game.Fleet
import PureSpace.Common.Game.Types
import PureSpace.Common.Lens (Lens', lens)

-- | An amount of cash received regularly
type Income = Integer

-- | Headquarters are vital targets; if no headerquarter remains,
-- a player has lost.
type IsHeadquarter = Bool

-- | Base of a player, providers of income and potentially
-- main target of an opponent
data Base =
  Base BaseCaracteristics
       Team
       Income
       Health
       Position
       Angle
       Fleet
       ObjectId
       PlayerId
       RespawnCooldown
  deriving (Eq, Ord, Show)

data BaseCaracteristics =
  BaseCaracteristics MaxHealth
                     IsHeadquarter
                     Width
                     Height
  deriving (Eq, Ord, Show)

class HasBase b where
  base :: Lens' b Base

class HasBaseType b where
  baseType :: Lens' b BaseCaracteristics

class HasIncome i where
  income :: Lens' i Income

class HasIsHeadquarter i where
  isHeadquarter :: Lens' i IsHeadquarter

instance HasBase Base where
  base = id

instance HasBaseType Base where
  baseType =
    let f (Base a _ _ _ _ _ _ _ _ _) = a
        g (Base _ b c d e k l m o p) a = Base a b c d e k l m o p
     in lens f g

instance HasTeam Base where
  team =
    let f (Base _ b _ _ _ _ _ _ _ _) = b
        g (Base a _ c d e k l m o p) b = Base a b c d e k l m o p
     in lens f g

instance HasIncome Base where
  income =
    let f (Base _ _ c _ _ _ _ _ _ _) = c
        g (Base a b _ d e k l m o p) c = Base a b c d e k l m o p
     in lens f g

instance HasHealth Base where
  health =
    let f (Base _ _ _ d _ _ _ _ _ _) = d
        g (Base a b c _ e k l m o p) d = Base a b c d e k l m o p
     in lens f g

instance HasPosition Base where
  position =
    let f (Base _ _ _ _ e _ _ _ _ _) = e
        g (Base a b c d _ k l m o p) e = Base a b c d e k l m o p
     in lens f g

instance HasAngle Base where
  angle =
    let f (Base _ _ _ _ _ k _ _ _ _) = k
        g (Base a b c d e _ l m o p) k = Base a b c d e k l m o p
     in lens f g

instance HasFleet Base where
  fleet =
    let f (Base _ _ _ _ _ _ l _ _ _) = l
        g (Base a b c d e k _ m o p) l = Base a b c d e k l m o p
     in lens f g

instance HasObjectId Base where
  objectId =
    let f (Base _ _ _ _ _ _ _ m _ _) = m
        g (Base a b c d e k l _ o p) m = Base a b c d e k l m o p
     in lens f g

instance HasPlayerId Base where
  playerId =
    let f (Base _ _ _ _ _ _ _ _ o _) = o
        g (Base a b c d e k l m _ p) o = Base a b c d e k l m o p
     in lens f g

instance HasRespawnCooldown Base where
  respawnCooldown =
    let f (Base _ _ _ _ _ _ _ _ _ p) = p
        g (Base a b c d e k l m o _) p = Base a b c d e k l m o p
     in lens f g

instance HasMaxHealth Base where
  maxHealth = baseType . maxHealth

instance HasIsHeadquarter Base where
  isHeadquarter = baseType . isHeadquarter

instance HasWidth Base where
  width = baseType . width

instance HasHeight Base where
  height = baseType . height

instance HasBaseType BaseCaracteristics where
  baseType = id

instance HasMaxHealth BaseCaracteristics where
  maxHealth =
    let f (BaseCaracteristics a _ _ _) = a
        g (BaseCaracteristics _ b c d) a = BaseCaracteristics a b c d
     in lens f g

instance HasIsHeadquarter BaseCaracteristics where
  isHeadquarter =
    let f (BaseCaracteristics _ b _ _) = b
        g (BaseCaracteristics a _ c d) b = BaseCaracteristics a b c d
     in lens f g

instance HasWidth BaseCaracteristics where
  width =
    let f (BaseCaracteristics _ _ c _) = c
        g (BaseCaracteristics a b _ d) c = BaseCaracteristics a b c d
     in lens f g

instance HasHeight BaseCaracteristics where
  height =
    let f (BaseCaracteristics _ _ _ d) = d
        g (BaseCaracteristics a b c _) d = BaseCaracteristics a b c d
     in lens f g
