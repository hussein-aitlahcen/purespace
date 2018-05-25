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
    RangeType (..),
    HasPosition (..),
    HasWidth (..),
    HasHeight (..),
    BucketSize,
    BucketId,
    Bucket (..),
    Grid (..),
    GridSize,
    GridDivision,
    Collision,
    createSpatialGrid,
    eliminateSpatialGrid,
    computeCollisions,
    computeRange,
  )
  where

import           Data.Bits                      (shiftL, (.&.), (.|.))
import qualified Data.IntMap.Strict             as M
import qualified Data.PQueue.Prio.Min           as PQ
import qualified Data.Set                       as S
import qualified Data.Vector                    as V
import           Linear
import           PureSpace.Common.Game.Geometry
import           PureSpace.Common.Lens
import           PureSpace.Common.Prelude

data RangeType    = InfiniteRange | FiniteRange Float
type Collision a  = (a, a)
type GridSize     = V2 Float
type GridDivision = V2 Float
type BucketSize   = V2 Float
type BucketId     = V2 Int
data Bucket     a = Bucket BucketId (V.Vector a)                                deriving Show
data Grid       a = Grid GridSize GridDivision BucketSize (M.IntMap (Bucket a)) deriving Show

instance Foldable Bucket where
  foldr f x (Bucket _ y) = V.foldr' f x y

instance Foldable Grid where
  foldr f x (Grid _ _ _ y) = M.foldr' (flip (foldr f)) x y

unitBounds :: (HasPosition s,
               HasWidth s,
               HasHeight s)
           => s
           -> Rectangle
unitBounds u = bounds (u ^. position) (fromIntegral $ u ^. width) (fromIntegral $ u ^. height)

bucketsByRectangle :: BucketSize -> Rectangle -> [BucketId]
bucketsByRectangle bs (a, b) =
  let pb = bucketByPosition bs
      (V2 xa ya) = pb a
      (V2 xb yb) = pb b
  in [V2 x y | x <- [xa..xb], y <- [ya..yb]]

bucketByPosition :: BucketSize -> Position -> BucketId
bucketByPosition bs = fmap round . (/ bs)

bucketHashId :: BucketId -> Int
bucketHashId (V2 x y) = 32 `shiftL` x .|. y .&. 0xFFFFFFFF

eliminateSpatialGrid :: Ord a => Grid a -> S.Set a
eliminateSpatialGrid = foldr S.insert S.empty

createSpatialGrid :: (HasPosition s,
                      HasWidth s,
                      HasHeight s)
                  => GridSize
                  -> GridDivision
                  -> [s]
                  -> Grid s
createSpatialGrid gs gd@(V2 gdw gdh) units =
  let bs               = gs / gd
      rc               = bucketsByRectangle bs
      -- (unit, (tl, br))
      unitsWithCorners = zip units $ unitBounds <$> units
      -- (unit, [bucket_hash])
      unitsBuckets     = second (fmap bucketHashId . rc) <$> unitsWithCorners
      unitsInBucket h  = fst <$> filter (elem h . snd) unitsBuckets
      buckets          = M.fromList [(h, Bucket p $ V.fromList $ unitsInBucket h)
                                    | x <- [0..round gdw]
                                    , y <- [0..round gdh]
                                    , let p = V2 x y
                                    , let h = bucketHashId p]
  in Grid gs gd bs buckets

computeCollisions :: (HasPosition s,
                      HasWidth s,
                      HasHeight s,
                      Ord s)
                  => Grid s
                  -> S.Set (Collision s)
computeCollisions (Grid _ _ _ buckets) = M.foldr' step S.empty buckets
  where
    step (Bucket _ objs) acc = go $ V.length objs
      where
        go 0 = acc
        go 1 = acc
        go l = S.union acc $ S.fromList (catMaybes [bool
                                                     (overlaps (unitBounds a) (unitBounds b))
                                                     (Just (a, b))
                                                     Nothing
                                                   | j <- [0..l-2]
                                                   , k <- [j+1..l-1]
                                                   , let a = objs V.! j
                                                   , let b = objs V.! k])

computeRange :: (HasPosition a, HasPosition s)
             => Grid s
             -> a
             -> RangeType
             -> PQ.MinPQueue Distance s
computeRange (Grid _ _ bs buckets) x (FiniteRange r) =
  let p               = x ^. position
      rangeRect       = bounds p r r
      possibleBuckets = bucketsByRectangle bs rangeRect
      targetBuckets   = catMaybes $ flip M.lookup buckets . bucketHashId <$> possibleBuckets
      d y             = distance (y ^. position) (x ^. position)
      step y
        | inRange y = PQ.insert (d y) y
        | otherwise = id
        where
          inRange = pointInCircle p r . (^. position)
  in foldr (flip $ foldr step) PQ.empty targetBuckets

computeRange g x InfiniteRange =
  let d y    = distance (y ^. position) (x ^. position)
      step y = PQ.insert (d y) y
  in foldr step PQ.empty g
