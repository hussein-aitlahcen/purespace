-- Game.hs ---

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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PureSpace.Client.Game
  (
    GameApp (..),
    GameConfig (..),
    GameState (..),
    GameError (..),
    runGame
  )
  where

import           PureSpace.Client.Game.Config     (GameConfig (..))
import           PureSpace.Client.Game.Error      (GameError (..))
import           PureSpace.Client.Game.State      (GameState (..))
import           PureSpace.Client.Graphics
import           PureSpace.Client.Graphics.Window (createGameWindow)
import           PureSpace.Common.Lens            (ExceptT, MonadIO,
                                                   MonadReader, MonadState,
                                                   ReaderT, StateT, evalStateT,
                                                   runExceptT, runReaderT)

newtype GameApp a = GameApp { unGame :: ExceptT GameError (ReaderT GameConfig (StateT GameState IO)) a }
    deriving (Functor, Applicative, Monad, MonadReader GameConfig, MonadIO, MonadState GameState)

entryPoint :: GameApp ()
entryPoint = GameApp createGameWindow

runGame :: GameConfig -> IO (Either GameError ())
runGame config = evalStateT (runReaderT (runExceptT $ unGame entryPoint) config) initialState
  where
    initialState  = GameState (GraphicsState (ShaderProgramState Nothing) (ShaderState []))
