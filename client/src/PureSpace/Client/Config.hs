-- Config.hs ---

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

module PureSpace.Client.Config
  (
    module PureSpace.Common.Game.Config,
    ClientConfig (..),
    defaultClientConfig
  )
  where

import           PureSpace.Common.Game.Config
import           PureSpace.Common.Lens

data ClientConfig = ClientConfig GameConfig deriving Show

defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig defaultGameConfig

instance HasGameConfig ClientConfig where
  gameConfig =
    let f (ClientConfig a) = a
        g _                = ClientConfig
    in lens f g

instance HasGridSize ClientConfig where
  gridSize = gameConfig . gridSize

instance HasGridDivision ClientConfig where
  gridDivision = gameConfig . gridDivision
