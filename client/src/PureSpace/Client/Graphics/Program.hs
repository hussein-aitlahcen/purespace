-- Program.hs ---

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

{-# LANGUAGE MultiParamTypeClasses #-}

module PureSpace.Client.Graphics.Program
  (
    module PureSpace.Client.Graphics.Program.Shader,
    module PureSpace.Client.Graphics.Program.Uniform,
    Program,
    ShaderProgramState (..),
    HasShaderProgramState (..),
    ShaderProgramError (..),
    AsShaderProgramError (..),
    loadGameShaderProgram,
  )
  where

import           Graphics.Rendering.OpenGL.GL                        (deleteObjectName)
import           Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects (Program, attachShader,
                                                                      createProgram,
                                                                      linkProgram,
                                                                      programInfoLog,
                                                                      validateProgram,
                                                                      validateStatus)
import           PureSpace.Client.Graphics.Program.Error
import           PureSpace.Client.Graphics.Program.Shader
import           PureSpace.Client.Graphics.Program.State
import           PureSpace.Client.Graphics.Program.Uniform
import           PureSpace.Common.Lens                               (MonadError,
                                                                      MonadIO,
                                                                      MonadState,
                                                                      liftIO,
                                                                      throwing,
                                                                      use)
import           PureSpace.Common.Prelude

loadGameShaderProgram :: (MonadIO m,
                          MonadError e m,
                          MonadState s m,
                          AsResourceError e,
                          HasShaderState s,
                          HasShaderProgramState s,
                          AsShaderError e,
                          AsShaderProgramError e)
                      => [(ShaderType, FilePath)] -> m Program
loadGameShaderProgram s = do
  loadGameShaders s
  shaders <- pure . fmap snd =<< use shaderListState
  program <- liftIO createProgram
  traverse_ (liftIO . attachShader program) shaders
  liftIO $ linkProgram program
  traverse_ deleteObjectName shaders
  liftIO $ validateProgram program
  isValid <- liftIO $ validateStatus program
  bool isValid
    (pure program)
    (do
        validationLog <- liftIO $ programInfoLog program
        deleteObjectName program
        throwing shaderProgramValidationFailure validationLog)

