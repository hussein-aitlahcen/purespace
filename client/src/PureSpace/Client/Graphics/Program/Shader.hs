-- Shader.hs ---
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

module PureSpace.Client.Graphics.Program.Shader
  ( module PureSpace.Common.Resource
  , module PureSpace.Client.Graphics.Program.Shader.Error
  , module PureSpace.Client.Graphics.Program.Shader.State
  , shaders
  , loadGameShaders
  ) where

import qualified Data.ByteString as BS
import Graphics.Rendering.OpenGL.GL (($=), deleteObjectName)
import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
  ( Shader
  , ShaderType(..)
  , compileShader
  , compileStatus
  , createShader
  , releaseShaderCompiler
  , shaderInfoLog
  , shaderSourceBS
  )
import Paths_purespace
import PureSpace.Client.Graphics.Program.Shader.Error
import PureSpace.Client.Graphics.Program.Shader.State
import PureSpace.Common.Lens
  ( MonadError
  , MonadIO
  , MonadState
  , (.=)
  , liftIO
  , throwing
  )
import PureSpace.Common.Prelude
import PureSpace.Common.Resource

shadersPath :: String
shadersPath = "shaders"

shaders :: MonadIO m => m [(ShaderType, FilePath)]
shaders =
  liftIO $
  traverse
    (bitraverse pure (getDataFileName . (shadersPath ++)))
    [(VertexShader, "/sprite.vert"), (FragmentShader, "/sprite.frag")]

createAndCompileShader ::
     (MonadIO m, MonadError e m, AsShaderError e)
  => ShaderType
  -> BS.ByteString
  -> m (ShaderType, Shader)
createAndCompileShader st src = do
  shader <- liftIO $ createShader st
  shaderSourceBS shader $= src
  liftIO $ compileShader shader
  success <- liftIO $ compileStatus shader
  liftIO releaseShaderCompiler
  bool
    success
    (pure (st, shader))
    (do compilationLog <- liftIO $ shaderInfoLog shader
        deleteObjectName shader
        throwing
          shaderCompilationFailed
          (st, "Shader compilation failed, log: " <> compilationLog))

loadGameShaders ::
     ( MonadIO m
     , MonadError e m
     , MonadState s m
     , AsResourceError e
     , HasShaderState s
     , AsShaderError e
     )
  => [(ShaderType, FilePath)]
  -> m ()
loadGameShaders shaders = do
  sources <- (traverse . traverse) (loadResource BS.readFile) shaders
  compiledShaders <- traverse (uncurry createAndCompileShader) sources
  shaderListState .= compiledShaders
