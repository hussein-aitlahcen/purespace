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
    spriteVAO
  )
  where

import           Graphics.GLUtil               (offset0, makeBuffer, makeVAO)
import           Graphics.UI.GLUT              (AttribLocation (..),
                                                BufferTarget (..),
                                                Capability (..), DataType (..),
                                                GLfloat, IntegerHandling (..),
                                                NumComponents,
                                                VertexArrayDescriptor (..),
                                                VertexArrayObject, bindBuffer,
                                                vertexAttribArray,
                                                vertexAttribPointer, ($=))
import           PureSpace.Common.Monad        (MonadIO, liftIO)

vaoComponents :: NumComponents
vaoComponents = 4

spriteVAO :: MonadIO m => [GLfloat] -> m VertexArrayObject
spriteVAO vertices = liftIO $ makeVAO $
    do buffer <- makeBuffer ArrayBuffer vertices
       bindBuffer ArrayBuffer $= Just buffer
       let attrib0 = AttribLocation 0
       vertexAttribArray  attrib0 $= Enabled
       vertexAttribPointer attrib0 $= (ToFloat, VertexArrayDescriptor vaoComponents Float 0 offset0)
       bindBuffer ArrayBuffer $= Nothing
