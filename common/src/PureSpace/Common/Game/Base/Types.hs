-- Types.hs ---

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

module PureSpace.Common.Game.Base.Types
  (
    Base (..)
  , BaseType (..)
  , HasIncome (..)
  , HasIsHeadquarter (..)
  , HasBase (..)
  , HasBaseType (..)
  , Income
  , IsHeadquarter
  )
where


import           PureSpace.Common.Game.Ship.Types
import           PureSpace.Common.Game.Types      (HasHealth, HasMaxHealth)
import           PureSpace.Common.Lens            (Lens', lens)

-- | An amount of cash received regularly
type Income = Integer
-- | Headquarters are vital targets; if no headerquarter remains,
-- a player has lost.
type IsHeadquarter = Bool

data BaseType = BaseType Income MaxHealth IsHeadquarter deriving Show

-- | Base of a player, providers of income and potentially
-- main target of an opponent
data Base = Base BaseType Health Position deriving Show

class HasBase b where
  base :: Lens' b Base

class HasBaseType b where
  baseType :: Lens' b BaseType

class HasIncome i where
  income :: Lens' i Income

class HasIsHeadquarter i where
  isHeadquarter :: Lens' i IsHeadquarter

instance HasBase Base where
  base = id

instance HasBaseType BaseType where
  baseType = id

instance HasBaseType Base where
 baseType =
    let f (Base a _ _) = a
        g (Base _ b c) a = Base a b c
    in lens f g

instance HasHealth Base where
  health =
    let f (Base _ b _) = b
        g (Base a _ c) b = Base a b c
    in lens f g

instance HasMaxHealth BaseType where
  maxHealth =
    let f (BaseType _ b _) = b
        g (BaseType a _ c) b = BaseType a b c
    in lens f g

instance HasIsHeadquarter BaseType where
  isHeadquarter =
    let f (BaseType _ _ c) = c
        g (BaseType a b _) = BaseType a b
    in lens f g
