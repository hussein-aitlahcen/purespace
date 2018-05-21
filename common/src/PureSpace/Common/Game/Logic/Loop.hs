--Loop.hs ---

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


module PureSpace.Common.Game.Logic.Loop
  (
    logicLoop,
    Timer (..)
  )
  where

import System.Clock
import           PureSpace.Common.Concurrent
import           PureSpace.Common.Lens
import Data.Int (Int64)

newtype Timer = Timer Int64

class HasTimer t where
  timer :: Lens' t Timer

instance HasTimer Timer where
  timer = id

class HasTime t where
  time :: Lens' t Int64

instance HasTime Timer where
  time =
    let f (Timer u) = u
        g _ = Timer
    in lens f g

logicLoop :: (MonadIO m,
        MonadState s m,
        HasTime s,
        HasTimer s)
       => m ()
logicLoop = tick *> liftIO (threadDelay . fromIntegral . milliToMicro $ 50) *> logicLoop

type Milliseconds = Int64
type Microseconds = Int64
type Nanoseconds = Int64

nanoToMilli :: Nanoseconds -> Milliseconds
nanoToMilli = (`div` 1000000)

milliToMicro :: Milliseconds -> Microseconds
milliToMicro = (* 1000)

tick :: (MonadIO m,
         MonadState s m,
         HasTime s,
         HasTimer s)
        => m ()
tick = do
  previous <- get
  now <- liftIO $ nsec <$> getTime MonotonicRaw
  let delta = nanoToMilli $ now - (previous ^. time)
  modify $ timer .~ Timer now
  liftIO $ putStrLn $ "Tick with delta " ++ show delta ++ " ms"
  
