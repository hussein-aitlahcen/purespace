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
module PureSpace.Client.Graphics.Memory.Buffer
  ( spriteVAO
  , spriteVertices
  , spriteDraw
  ) where

import Foreign.Storable (sizeOf)
import Graphics.GLUtil (makeVAO, offsetPtr)
import Graphics.UI.GLUT
  ( AttribLocation(..)
  , BufferObject
  , BufferTarget(..)
  , Capability(..)
  , DataType(..)
  , GLfloat
  , IntegerHandling(..)
  , NumArrayIndices
  , NumComponents
  , PrimitiveMode(..)
  , VertexArrayDescriptor(..)
  , VertexArrayObject
  , ($=)
  , bindBuffer
  , drawArrays
  , vertexAttribArray
  , vertexAttribPointer
  )
import PureSpace.Common.Monad (MonadIO, liftIO)

-- vec4, defined by spriteVertices
spriteComponents :: NumComponents
spriteComponents = 4

-- 2 triangles => 6 vertices
spriteVerticeNumber :: NumArrayIndices
spriteVerticeNumber = 6

spriteVertices :: Integral a => a -> a -> a -> a -> a -> a -> [GLfloat]
spriteVertices x y w h textW textH = triangles
    -- UV coords (normalized with texture dimension)
  where
    norm a b = fromIntegral a / fromIntegral b
    nX = norm x textW
    nY = norm y textH
    nW = norm w textW
    nH = norm h textH
    hw = norm w 2
    hh = norm h 2
    triangles
        -- vertexX, vertexY, textureX, textureY,
        -- interpreted as vec4 by the vertex shader
        -- bottom left triangle
        --
     =
      [ -hw
      , hh
      , nX
      , nY
      , hw
      , hh
      , nX + nW
      , nY
      , hw
      , -hh
      , nX + nW
      , nY + nH
        -- top right triangle
      , -hw
      , hh
      , nX
      , nY
      , hw
      , -hh
      , nX + nW
      , nY + nH
      , -hw
      , -hh
      , nX
      , nY + nH
      ]

spriteDraw :: MonadIO m => m ()
spriteDraw = liftIO $ drawArrays Triangles 0 spriteVerticeNumber

spriteVAO :: MonadIO m => BufferObject -> Int -> m VertexArrayObject
spriteVAO buffer = createVAO buffer spriteComponents spriteVerticeNumber

createVAO ::
     MonadIO m
  => BufferObject
  -> NumComponents
  -> NumArrayIndices
  -> Int
  -> m VertexArrayObject
createVAO buffer components nbOfVertices index =
  liftIO $
  makeVAO $ do
    bindBuffer ArrayBuffer $= Just buffer
    vertexAttribArray attrib0 $= Enabled
    vertexAttribPointer attrib0 $=
      (ToFloat, VertexArrayDescriptor components Float stride offset)
    bindBuffer ArrayBuffer $= Nothing
  where
    stride = 0
    attrib0 = AttribLocation 0
    offset =
      let verticeSize = components * fromIntegral componentSize
       in offsetPtr $
          fromIntegral (nbOfVertices * verticeSize * fromIntegral index)
      -- ugly but required
    componentSize = sizeOf (undefined :: GLfloat)
