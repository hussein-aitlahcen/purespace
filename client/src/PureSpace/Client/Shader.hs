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

{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PureSpace.Client.Shader
  (
    Shader,
    ShaderType (..),
    ShaderError (..),
    AsShaderError (..),
    loadGameShaders
  )
  where

import qualified Data.ByteString                                    as BS
import           Graphics.Rendering.OpenGL.GL                       (deleteObjectName,
                                                                     ($=))
import           Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects (Shader, ShaderType (..),
                                                                     compileShader,
                                                                     compileStatus,
                                                                     createShader,
                                                                     releaseShaderCompiler,
                                                                     shaderInfoLog,
                                                                     shaderSourceBS)
import           PureSpace.Common.Files                             (doesFileExist)
import           PureSpace.Common.Lens                              (MonadError,
                                                                     MonadIO,
                                                                     Prism',
                                                                     liftIO,
                                                                     prism,
                                                                     throwing)
import           PureSpace.Common.Prelude

data ShaderError = ShaderFileNotFound       (ShaderType, String)
                 | ShaderNotSupported       ShaderType
                 | ShaderCompilationFailure (ShaderType, String)
                 deriving Show

class AsShaderError s where
  shaderError             :: Prism' s ShaderError
  shaderFileNotFound      :: Prism' s (ShaderType, String)
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

shadersPath :: String
shadersPath = "./shaders"

shaderTypePath :: (MonadError e m, AsShaderError e) => ShaderType -> m String
shaderTypePath FragmentShader = pure $ shadersPath <> "/sprite.frag"
shaderTypePath VertexShader   = pure $ shadersPath <> "/sprite.vert"
shaderTypePath st             = throwing shaderNotSupported st

-- TODO: refactor and extract this sort of thing to the common module
loadShaderSource :: (MonadIO m, MonadError e m, AsShaderError e) => ShaderType -> m BS.ByteString
loadShaderSource st = do
  shaderPath <- shaderTypePath st
  fileExists <- liftIO $ doesFileExist shaderPath
  bool fileExists
    (liftIO $ BS.readFile shaderPath)
    (throwing shaderFileNotFound (st, shaderPath))

-- NOTTODO: liftIO's are boring
createAndCompileShader :: (MonadIO m, MonadError e m, AsShaderError e) => ShaderType -> BS.ByteString -> m Shader
createAndCompileShader st src = do
  shader <- liftIO $ createShader st
  shaderSourceBS shader $= src
  liftIO $ compileShader shader
  success <- liftIO $ compileStatus shader
  liftIO releaseShaderCompiler
  bool success
    (pure shader)
    (do
        compilationLog <- liftIO $ shaderInfoLog shader
        deleteObjectName shader
        throwing shaderCompilationFailed (st, "Shader compilation failed, log: " <> compilationLog))

loadGameShaders :: (MonadIO m, MonadError e m, AsShaderError e) => [ShaderType] -> m [Shader]
loadGameShaders shaderTypes = do
  sources <- zip shaderTypes <$> traverse loadShaderSource shaderTypes
  traverse (uncurry createAndCompileShader) sources
