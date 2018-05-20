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
    Corners,
    corners,
    overlaps
  )
  where

import           PureSpace.Common.Game.Types (HasHeight (..), HasPosition (..),
                                              HasWidth (..), Position, V2 (..))
import           PureSpace.Common.Lens       ((^.))

-- top left, bot right
type Corners = (Position, Position)

corners :: (HasPosition s,
            HasWidth s,
            HasHeight s)
        => s
        -> Corners
corners x =
  let p = x ^. position
      w = x ^. width
      h = x ^. height
      q = (/ 2) $ fromIntegral <$> V2 w h
      pq = (+ p) . (* q)
  in (pq $ V2 (-1)  (-1),
      pq $ V2   1     1)

overlaps :: (HasPosition s,
             HasWidth s,
             HasHeight s)
         => s
         -> s
         -> Bool
overlaps sa sb =
  let (aa, ba) = corners sa
      (ab, bb) = corners sb
      overlap (V2 x y) (V2 a b) (V2 c d) = x >= a && x <= c && y >= b && y <= d
  in overlap aa ab bb ||
     overlap ba ab bb ||
     overlap ab aa ba ||
     overlap bb aa ba
