-- Action.hs ---
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
{-# LANGUAGE GADTs #-}

module PureSpace.Common.Game.Action
  ( GameAction(..)
  ) where

import PureSpace.Common.Game.Player.State

data GameAction where
  ShotTarget
    :: ( HasPosition t
       , HasTeam t
       , HasPlayerId t
       , HasProjectileCaracteristics t
       , HasPosition u
       )
    => t
    -> u
    -> GameAction
  SpawnShip
    :: ( HasPosition t
       , HasTeam t
       , HasPlayerId t
       , HasShipCaracteristics u
       )
    => t
    -> u
    -> GameAction
  NoOperation :: GameAction
