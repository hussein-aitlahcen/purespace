-- Shaders.hs ---

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

module PureSpace.Client.Shaders
  (
    GameShaders (..),
    ShaderError (..),
    HasGameShaders (..),
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
import           PureSpace.Common.Files
import           PureSpace.Common.Lens
import           PureSpace.Common.Prelude

newtype GameShaders = GameShaders [Shader]

data ShaderError = FileNotFound      (ShaderType, String)
                 | NotSupported      ShaderType
                 | CompilationFailed (ShaderType, String)
                 deriving Show

class HasGameShaders s where
  gameShaders    :: Lens' s GameShaders
  fragmentShader :: Lens' s [Shader]

instance HasGameShaders GameShaders where
  gameShaders    = id
  fragmentShader = lens f g
    where
      f (GameShaders shaders) = shaders
      g _                     = GameShaders

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
    let f = FileNotFound
        g = \case
          FileNotFound x -> Right x
          x              -> Left x
    in prism f g
  shaderCompilationFailed =
    let f = CompilationFailed
        g = \case
          CompilationFailed x -> Right x
          x                   -> Left x
    in prism f g
  shaderNotSupported =
    let
      f = NotSupported
      g = \case
        NotSupported x -> Right x
        x              -> Left x
    in prism f g

shadersPath :: String
shadersPath = "./shaders"

shaderTypePath :: (MonadError e m, AsShaderError e) => ShaderType -> m String
shaderTypePath FragmentShader = pure $ shadersPath <> "/fragment.shader"
-- TODO: support more shaders ?
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
    (let releaseShader   = deleteObjectName shader
         loadFailureLogs = liftIO (shaderInfoLog shader)
     in releaseShader *> loadFailureLogs >>= throwing shaderCompilationFailed . (,) st)

loadGameShaders :: (MonadIO m, MonadError e m, AsShaderError e) => [ShaderType] -> m GameShaders
loadGameShaders shaderTypes = do
  sources <- zip shaderTypes <$> traverse loadShaderSource shaderTypes
  GameShaders <$> traverse (uncurry createAndCompileShader) sources

