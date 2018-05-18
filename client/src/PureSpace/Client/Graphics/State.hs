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

module PureSpace.Client.Graphics.State
  (
    module PureSpace.Client.Graphics.Device.State,
    module PureSpace.Client.Graphics.Program.State,
    GraphicsState (..),
    HasGraphicsState (..),
    initialGraphicsState
  )
  where

import           PureSpace.Client.Graphics.Device.State
import           PureSpace.Client.Graphics.Program.State
import           PureSpace.Common.Lens

data GraphicsState = GraphicsState ShaderProgramState DeviceState

class HasGraphicsState s where
  graphicsState :: Lens' s GraphicsState

instance HasGraphicsState GraphicsState where
  graphicsState = id

instance HasShaderProgramState GraphicsState where
  shaderProgramState =
    let f (GraphicsState x _)   = x
        g (GraphicsState _ y) x = GraphicsState x y
    in lens f g

instance HasShaderState GraphicsState where
  shaderState = shaderProgramState . shaderState

instance HasDeviceState GraphicsState where
  deviceState =
    let f (GraphicsState _ y)   = y
        g (GraphicsState x _) y = GraphicsState x y
    in lens f g

initialGraphicsState :: GraphicsState
initialGraphicsState = GraphicsState initialShaderProgramState initialDeviceState
