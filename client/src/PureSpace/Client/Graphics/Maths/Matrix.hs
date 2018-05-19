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
  (
    Matrix,
    PureSpace.Client.Graphics.Maths.Matrix.identity,
    rotate2D,
    translate2D,
    scale2D,
    ortho2D
  )
  where

import           Graphics.Rendering.OpenGL.GL (GLfloat)
import           Linear                       (M44, V3 (..), V4 (..), axisAngle,
                                               fromQuaternion, identity,
                                               m33_to_m44, ortho, scaled,
                                               translation, (!*!))
import           PureSpace.Common.Lens        ((&), (+~))

-- 2D transformations

type Matrix = M44 GLfloat

identity :: Matrix
identity = Linear.identity

translate2D :: GLfloat -> GLfloat -> Matrix -> Matrix
translate2D x y m = m & translation +~ V3 x y 0

scale2D :: GLfloat -> Matrix -> Matrix
scale2D s = (!*!) (scaled (V4 s s 1 1))

rotate2D :: GLfloat -> Matrix -> Matrix
rotate2D angle matrix = matrix !*! rotationMatrix
  where
    rotationMatrix = m33_to_m44
                     $ fromQuaternion
                     $ axisAngle (V3 0 0 1) angle

ortho2D :: Integral a => a -> a -> a -> Matrix
ortho2D va w h
  | w > h     = ortho (-visibleArea*aspectRatio) (visibleArea*aspectRatio) (-visibleArea)             visibleArea               near far
  | otherwise = ortho (-visibleArea)             visibleArea               (-visibleArea/aspectRatio) (visibleArea/aspectRatio) near far
  where
    visibleArea = fromIntegral va
    aspectRatio = fromIntegral w / fromIntegral h
-- near/far fixed for ortho
    near        = -1
    far         = 1

