-- ShaderProgramError.hs ---

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

{-# LANGUAGE LambdaCase #-}

module PureSpace.Client.Graphics.Shader.Program.Error
  (
    ShaderProgramError (..),
    AsShaderProgramError (..),
  )
  where

import           PureSpace.Common.Lens (Prism', prism)

newtype ShaderProgramError = ShaderProgramValidationFailure String deriving Show

class AsShaderProgramError s where
  shaderProgramError             :: Prism' s ShaderProgramError
  shaderProgramValidationFailure :: Prism' s String
  shaderProgramValidationFailure = shaderProgramError . shaderProgramValidationFailure

instance AsShaderProgramError ShaderProgramError where
  shaderProgramError = id
  shaderProgramValidationFailure =
    let f = ShaderProgramValidationFailure
        g = \case
          ShaderProgramValidationFailure x -> Right x
          x                                -> Left  x
    in prism f g
