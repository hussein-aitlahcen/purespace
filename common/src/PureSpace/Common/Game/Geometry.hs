-- Geometry.hs ---

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

module PureSpace.Common.Game.Geometry
  (
    V2 (..),
    Position,
    Direction,
    Rectangle,
    bounds,
    overlaps,
    pointInRectangle,
    pointInCircle,
    direction
  )
  where

import           Linear                      (V2 (..))
import           PureSpace.Common.Game.Types (Direction, Position)

-- top left, bot right
type Rectangle = (Position, Position)

bounds :: Position -> Float -> Float -> Rectangle
bounds p w h =
  let q = (/ 2) <$> V2 w h
      pq = (+ p) . (* q)
  in (pq $ V2 (-1)  (-1),
      pq $ V2   1     1)

pointInCircle :: Position -> Float -> Position -> Bool
pointInCircle (V2 cx cy) r (V2 x y) =
  let dx = abs (x - cx)
      dy = abs (y - cy)
  in dx*dx + dy*dy < r*r

pointInRectangle :: Rectangle -> Position -> Bool
pointInRectangle (V2 a b, V2 c d) (V2 x y) =
  let xIsInside = x >= a && x <= c
      yIsInside = y >= b && y <= d
  in xIsInside && yIsInside

overlaps :: Rectangle -> Rectangle -> Bool
overlaps p@(aa, ab) q@(ba, bb) =
  let pContains = pointInRectangle p
      qContains = pointInRectangle q
  in qContains aa ||
     qContains ba ||
     pContains ab ||
     pContains bb

direction :: Position -> Position -> Direction
direction a b = b - a
