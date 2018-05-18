-- Input.hs ---

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

module PureSpace.Client.Graphics.Device.Input
  (
    InputEvent (..),
    TChan,
    InputStream,
    Key (..),
    KeyState (..),
    Modifiers (..),
    Position,
    inputStream,
  )
  where

import           Graphics.UI.GLUT            (Key (..), KeyState (..),
                                              KeyboardMouseCallback,
                                              Modifiers (..), Position (..))
import           PureSpace.Common.Concurrent

type InputStream = TChan InputEvent

data InputEvent = InputEvent Key KeyState Modifiers Position deriving Show

inputStream :: TChan InputEvent -> KeyboardMouseCallback
inputStream c k ks m p = atomically $ writeTChan c $ InputEvent k ks m p
