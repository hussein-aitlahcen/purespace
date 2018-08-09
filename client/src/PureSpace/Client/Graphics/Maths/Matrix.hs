-- Matrix.hs ---
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
module PureSpace.Client.Graphics.Maths.Matrix
  ( Matrix
  , PureSpace.Client.Graphics.Maths.Matrix.identity
  , rotate2D
  , translate2D
  , scale2D
  , ortho2D
  ) where

import Graphics.Rendering.OpenGL.GL (GLfloat)
import Linear
  ( M44
  , V2(..)
  , V3(..)
  , V4(..)
  , (!*!)
  , axisAngle
  , fromQuaternion
  , identity
  , m33_to_m44
  , ortho
  , scaled
  , translation
  )
import PureSpace.Common.Lens ((&), (+~))

-- 2D transformations
type Matrix = M44 GLfloat

identity :: Matrix
identity = Linear.identity

translate2D :: V2 Float -> Matrix -> Matrix
translate2D (V2 x y) m = m & translation +~ V3 x y 0

scale2D :: V2 Float -> Matrix -> Matrix
scale2D (V2 x y) = (!*!) (scaled (V4 x y 1 1))

rotate2D :: Float -> Matrix -> Matrix
rotate2D radians matrix =
  let rotationMatrix =
        m33_to_m44 $ fromQuaternion $ axisAngle (V3 0 0 1) radians
   in matrix !*! rotationMatrix

ortho2D :: Float -> Float -> Float -> Matrix
ortho2D va w h
  | w > h =
    ortho
      (-visibleArea * aspectRatio)
      (visibleArea * aspectRatio)
      (-visibleArea)
      visibleArea
      near
      far
  | otherwise =
    ortho
      (-visibleArea)
      visibleArea
      (-visibleArea / aspectRatio)
      (visibleArea / aspectRatio)
      near
      far
  where
    visibleArea = va
    aspectRatio = w / h
    -- near/far fixed for ortho
    near = -1
    far = 1
