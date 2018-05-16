-- ShaderProgram.hs ---

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

{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PureSpace.Client.ShaderProgram
  (
    Program,
    ShaderProgramError (..),
    AsShaderProgramError (..),
    Shader,
    ShaderType (..),
    ShaderError (..),
    AsShaderError (..),
    loadGameShaderProgram
  )
  where

import           Graphics.Rendering.OpenGL.GL                        (deleteObjectName)
import           Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects (Program, attachShader,
                                                                      createProgram,
                                                                      linkProgram,
                                                                      programInfoLog,
                                                                      validateProgram,
                                                                      validateStatus)
import           PureSpace.Client.Shader                             (AsShaderError (..),
                                                                      Shader,
                                                                      ShaderError (..),
                                                                      ShaderType (..),
                                                                      loadGameShaders)
import           PureSpace.Common.Lens                               (MonadError,
                                                                      MonadIO,
                                                                      Prism',
                                                                      liftIO,
                                                                      prism,
                                                                      throwing)
import           PureSpace.Common.Prelude

newtype ShaderProgramError = ShaderProgramValidationFailure String deriving Show

class AsShaderProgramError s where
  shaderProgramError              :: Prism' s ShaderProgramError
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

loadGameShaderProgram :: (MonadIO m, MonadError e m, AsShaderError e, AsShaderProgramError e) => m Program
loadGameShaderProgram = do
  shaders <- loadGameShaders [VertexShader, FragmentShader]
  program <- liftIO createProgram
  traverse_ (liftIO . attachShader program) shaders
  liftIO $ linkProgram program
  liftIO $ validateProgram program
  isValid <- liftIO $ validateStatus program
  bool isValid
    (pure program)
    (do
        validationLog <- liftIO $ programInfoLog program
        deleteObjectName program
        throwing shaderProgramValidationFailure validationLog)

