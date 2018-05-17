-- Error.hs ---

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

module PureSpace.Client.Graphics.Shader.Error
  (
    ShaderError (..),
    AsShaderError (..),
  )
  where

import           Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects (ShaderType (..))
import           PureSpace.Common.Lens                              (Prism',
                                                                     prism)
data ShaderError = ShaderFileNotFound       FilePath
                 | ShaderNotSupported       ShaderType
                 | ShaderCompilationFailure (ShaderType, String)
                 deriving Show

class AsShaderError s where
  shaderError             :: Prism' s ShaderError
  shaderFileNotFound      :: Prism' s FilePath
  shaderCompilationFailed :: Prism' s (ShaderType, String)
  shaderNotSupported      :: Prism' s ShaderType
  shaderFileNotFound      = shaderError . shaderFileNotFound
  shaderCompilationFailed = shaderError . shaderCompilationFailed
  shaderNotSupported      = shaderError . shaderNotSupported

instance AsShaderError ShaderError where
  shaderError = id
  shaderFileNotFound =
    let f = ShaderFileNotFound
        g = \case
          ShaderFileNotFound x -> Right x
          x                    -> Left x
    in prism f g
  shaderCompilationFailed =
    let f = ShaderCompilationFailure
        g = \case
          ShaderCompilationFailure x -> Right x
          x                          -> Left x
    in prism f g
  shaderNotSupported =
    let
      f = ShaderNotSupported
      g = \case
        ShaderNotSupported x -> Right x
        x                    -> Left x
    in prism f g
