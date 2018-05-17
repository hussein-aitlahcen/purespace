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

module PureSpace.Client.Graphics.Shader.Program.State
  (
    Program,
    ShaderProgramState (..),
    HasShaderProgramState (..),
  )
  where

import           Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects (Program)
import           PureSpace.Common.Lens                               (Lens',
                                                                      lens)

newtype ShaderProgramState = ShaderProgramState (Maybe Program) deriving Show

class HasShaderProgramState s where
  shaderProgramState   :: Lens' s ShaderProgramState
  shaderProgram        :: Lens' s (Maybe Program)
  shaderProgram = shaderProgramState . shaderProgram

instance HasShaderProgramState ShaderProgramState where
  shaderProgramState = id
  shaderProgram =
    let f (ShaderProgramState x)   = x
        g (ShaderProgramState _) x = ShaderProgramState x
    in lens f g
