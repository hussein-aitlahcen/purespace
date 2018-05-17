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

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module PureSpace.Client.Graphics.State
  (
    GraphicsState (..),
    HasGraphicsState (..),
    ShaderProgramState (..),
    HasShaderProgramState (..),
    ShaderState (..),
    HasShaderState (..)
  )
  where

import           PureSpace.Client.Graphics.Shader.Program.State
import           PureSpace.Client.Graphics.Shader.State
import           PureSpace.Common.Lens

data GraphicsState = GraphicsState ShaderProgramState ShaderState deriving Show

class HasGraphicsState s where
  graphicsState :: Lens' s GraphicsState

instance HasGraphicsState GraphicsState where
  graphicsState = id

instance HasShaderState GraphicsState where
  shaderState =
    let f (GraphicsState _ y)   = y
        g (GraphicsState x _) y = GraphicsState x y
    in graphicsState . lens f g

instance HasShaderProgramState GraphicsState where
  shaderProgramState =
    let f (GraphicsState x _)   = x
        g (GraphicsState _ y) x = GraphicsState x y
    in lens f g
