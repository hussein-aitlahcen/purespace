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

module PureSpace.Client.Graphics.Matrix
  (
    Matrix,
    identity,
    ortho2D
  )
  where

import           Graphics.Rendering.OpenGL.GL (GLfloat)
import qualified Linear                       as L

type Matrix = L.M44 GLfloat

identity :: Matrix
identity = L.identity

ortho2D :: Integral a => a -> a -> a -> Matrix
ortho2D va w h
  | w > h     = L.ortho (-visibleArea*aspectRatio) (visibleArea*aspectRatio) (-visibleArea)             visibleArea               (-1) 1
  | otherwise = L.ortho (-visibleArea)             visibleArea               (-visibleArea/aspectRatio) (visibleArea/aspectRatio) (-1) 1
  where
    visibleArea = fromIntegral va
    aspectRatio = fromIntegral w / fromIntegral h

