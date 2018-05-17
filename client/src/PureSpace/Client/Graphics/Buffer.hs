-- Buffer.hs ---

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

module PureSpace.Client.Graphics.Buffer
  (
    createVBO,
    createVAO,
  )
  where

import           Data.Vector.Storable   (fromList, unsafeWith)
import           Foreign.Storable       (Storable (..), sizeOf)
import           Graphics.GLUtil        (offset0)
import           Graphics.UI.GLUT       (AttribLocation (..), BufferObject,
                                         BufferTarget (..), BufferUsage (..),
                                         Capability (..), DataType (..),
                                         GLfloat, IntegerHandling (..),
                                         NumComponents,
                                         VertexArrayDescriptor (..),
                                         VertexArrayObject, bindBuffer,
                                         bindVertexArrayObject, bufferData,
                                         genObjectName, vertexAttribArray,
                                         vertexAttribPointer, ($=))
import           PureSpace.Common.Monad (MonadIO, liftIO)

vaoComponents :: NumComponents
vaoComponents = 4

createVBO :: MonadIO m => [GLfloat] -> m (BufferObject, VertexArrayObject)
createVBO vertices  = do
  vertexBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just vertexBuffer
  let vector = fromList vertices
  liftIO $ unsafeWith vector $ \ptr ->
    bufferData ArrayBuffer $= (bufferSize, ptr, StaticDraw)
  vertexArray <- createVAO vaoComponents
  bindBuffer ArrayBuffer                 $= Nothing
  pure (vertexBuffer, vertexArray)
  where
    bufferSize = toEnum $ length vertices * sizeOf (head vertices)

createVAO :: MonadIO m => NumComponents -> m VertexArrayObject
createVAO numComponents = do
  vertexArray  <- genObjectName
  bindVertexArrayObject                  $= Just vertexArray
  vertexAttribArray   (AttribLocation 0) $= Enabled
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor numComponents Float 0 offset0)
  bindVertexArrayObject                  $= Nothing
  pure vertexArray
