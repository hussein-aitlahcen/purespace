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
module PureSpace.Client.State
  ( module PureSpace.Client.Graphics.State
  , module PureSpace.Common.Game.State
  , module PureSpace.Common.Game.Config
  , ClientState(..)
  , initialClientState
  ) where

import PureSpace.Client.Graphics.State
import PureSpace.Common.Game.Config
import PureSpace.Common.Game.State
import PureSpace.Common.Lens (lens)

data ClientState =
  ClientState GameState
              GraphicsState

instance HasGameState ClientState where
  gameState =
    let f (ClientState x _) = x
        g (ClientState _ y) x = ClientState x y
     in lens f g

instance HasGraphicsState ClientState where
  graphicsState =
    let f (ClientState _ y) = y
        g (ClientState x _) y = ClientState x y
     in lens f g

instance HasShaderState ClientState where
  shaderState = graphicsState . shaderState

instance HasShaderProgramState ClientState where
  shaderProgramState = graphicsState . shaderProgramState

instance HasDeviceState ClientState where
  deviceState = graphicsState . deviceState

initialClientState :: GameConfig -> ClientState
initialClientState gameConf =
  ClientState (initialGameState gameConf) initialGraphicsState
