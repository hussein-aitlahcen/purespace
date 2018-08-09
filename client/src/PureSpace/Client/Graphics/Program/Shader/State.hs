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
module PureSpace.Client.Graphics.Program.Shader.State
  ( Shader
  , ShaderType
  , ShaderState(..)
  , HasShaderState(..)
  , initialShaderState
  ) where

import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects (Shader, ShaderType)
import PureSpace.Common.Lens (Lens', lens)

type ShaderEntry = (ShaderType, Shader)

type Shaders = [ShaderEntry]

newtype ShaderState =
  ShaderState Shaders
  deriving (Show)

class HasShaderState s where
  shaderState :: Lens' s ShaderState
  shaderListState :: Lens' s Shaders
  shaderListState = shaderState . shaderListState

instance HasShaderState ShaderState where
  shaderState = id
  shaderListState =
    let f (ShaderState x) = x
        g (ShaderState _) x = ShaderState x
     in lens f g

initialShaderState :: ShaderState
initialShaderState = ShaderState []
