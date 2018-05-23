-- Client.hs ---

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

module PureSpace.Client
  (
    ClientConfig (..),
    ClientState (..),
    ClientError (..),
    runClient
  )
  where

import           PureSpace.Client.Config
import           PureSpace.Client.Error
import           PureSpace.Client.Graphics.Window
import           PureSpace.Client.State
import           PureSpace.Common.Concurrent
import           PureSpace.Common.Game.Logic.Loop
import           PureSpace.Common.Lens
import           System.Clock

runGraphics :: IO ()
runGraphics =
  let config = ClientConfig
      go = evalStateT (runReaderT (runExceptT createGameWindow) config) initialClientState
      displayResult res = case res of
        Left message -> print (message :: ClientError)
        Right _      -> putStrLn "Unseen string"
  in go >>= displayResult

runLogic :: IO ()
runLogic =
  do initial <- liftIO $ toNanoSecs <$> getTime MonotonicRaw
     evalStateT logicLoop (Timer initial)

-- | Makes every thread return an empty MVar,
-- so the main thread can wait for them.
forkLoop :: IO () -> IO (MVar ())
forkLoop thread = do
    handle <- newEmptyMVar
    _ <- forkFinally thread (\_ -> putMVar handle ())
    return handle

runClient :: IO ()
runClient = do
  graphicThread <- forkLoop $ void runGraphics
  logicThread   <- forkLoop runLogic
  -- Waiting for all threads to finish
  mapM_ takeMVar [graphicThread, logicThread]
  putStrLn "Finished"
