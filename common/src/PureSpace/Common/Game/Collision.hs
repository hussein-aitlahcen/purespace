-- Collision.hs ---

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

module PureSpace.Common.Game.Collision
  (
    V2 (..),
    HasPosition (..),
    HasWidth (..),
    HasHeight (..),
    GridSize,
    GridDivision,
    BucketSize,
    BucketId,
    Bucket (..),
    Grid (..),
    Collision,
    createSpatialGrid,
    computeCollisions,
  )
  where

import           Data.Bits                      (shiftL, (.&.), (.|.))
import qualified Data.IntMap.Strict             as M
import qualified Data.Vector                    as V
import           Linear
import           PureSpace.Common.Game.Geometry
import           PureSpace.Common.Prelude

type Collision a  = (a, a)
type GridSize     = Float
type GridDivision = Float
type BucketSize   = Float
type BucketId     = V2 Int
data Bucket     a = Bucket BucketId (V.Vector a)                                deriving Show
data Grid       a = Grid GridSize GridDivision BucketSize (M.IntMap (Bucket a)) deriving Show

createSpatialGrid :: (HasPosition s,
                        HasWidth s,
                        HasHeight s)
                    => GridSize
                    -> GridDivision
                    -> [s]
                    -> Grid s
createSpatialGrid gs gd units =
  let bs              = gs / gd
      rc              = rectangleBuckets bs
      -- (unit, (tl, br))
      unitsCorners    = zip units $ corners <$> units
      -- (unit, [bucket_hash])
      unitsBuckets    = second (fmap bucketHashId . rc) <$> unitsCorners
      unitsInBucket h = fst <$> filter (elem h . snd) unitsBuckets
      buckets         = M.fromList [(h, Bucket p $ V.fromList $ unitsInBucket h)
                                | x <- [0..round gd]
                                , y <- [0..round gd]
                                , let p = V2 x y
                                , let h = bucketHashId p]
  in Grid gs gd bs buckets

rectangleBuckets :: BucketSize -> Corners -> [BucketId]
rectangleBuckets bs (a, b) =
  let pb = positionBucket bs
      (V2 xa ya) = pb a
      (V2 xb yb) = pb b
  in [V2 x y | x <- [xa..xb], y <- [ya..yb]]

positionBucket :: BucketSize -> Position -> BucketId
positionBucket bs = fmap round . (^/ bs)

bucketHashId :: BucketId -> Int
bucketHashId (V2 x y) = 32 `shiftL` x .|. y .&. 0xFFFFFFFF

computeCollisions :: (HasPosition s,
                      HasWidth s,
                      HasHeight s,
                      Eq s)
                  => Grid s
                  -> V.Vector (Collision s)
computeCollisions (Grid _ _ _ buckets) = M.foldr step V.empty buckets
  where
    step (Bucket _ objs) acc = go $ V.length objs
      where
        go 0 = acc
        go 1 = acc
        go l =
          let exists x = V.elem x acc
          in acc V.++ V.fromList (catMaybes [bool
                                              (overlaps a b && (not . exists) (a, b))
                                              (Just (a, b))
                                              Nothing |
                                             j <- [0..l-2],
                                             k <- [j+1..l-1],
                                             let a = objs V.! j,
                                             let b = objs V.! k])


-- TODO: circle to buckets
circleRangeBuckets :: (HasPosition s,
                       HasWidth s,
                       HasHeight s)
                   => Grid s
                   -> Position
                   -> Float
                   -> [Bucket s]
circleRangeBuckets (Grid _ _ _ bucks) pos range =
  let points = [pos + V2 0 range,
                pos + V2 range 0,
                pos + V2 (-range) 0,
                pos + V2 0 (-range)]
  in []
