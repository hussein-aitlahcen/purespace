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
module PureSpace.Client.Graphics.Program.State
  ( module PureSpace.Client.Graphics.Program.Shader.State
  , Program
  , ShaderProgramState(..)
  , HasShaderProgramState(..)
  , initialShaderProgramState
  ) where

import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects (Program)
import PureSpace.Client.Graphics.Program.Shader.State
import PureSpace.Common.Lens (Lens', lens)

data ShaderProgramState =
  ShaderProgramState ShaderState
                     (Maybe Program)
  deriving (Show)

class HasShaderProgramState s where
  shaderProgramState :: Lens' s ShaderProgramState
  shaderProgram :: Lens' s (Maybe Program)
  shaderProgram = shaderProgramState . shaderProgram

instance HasShaderProgramState ShaderProgramState where
  shaderProgramState = id
  shaderProgram =
    let f (ShaderProgramState _ y) = y
        g (ShaderProgramState x _) y = ShaderProgramState x y
     in lens f g

instance HasShaderState ShaderProgramState where
  shaderState =
    let f (ShaderProgramState x _) = x
        g (ShaderProgramState _ y) x = ShaderProgramState x y
     in lens f g

initialShaderProgramState :: ShaderProgramState
initialShaderProgramState = ShaderProgramState initialShaderState Nothing
