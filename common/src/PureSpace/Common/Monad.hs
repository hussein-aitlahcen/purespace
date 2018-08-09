-- Monad.hs ---
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
module PureSpace.Common.Monad
  ( module Control.Applicative
  , module Control.Monad.Error.Class
  , module Control.Monad.IO.Class
  , module Control.Monad.Except
  , module Control.Monad.Reader
  , module Control.Monad.Writer.Strict
  , module Control.Monad.State.Strict
  , module Control.Monad
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict hiding ((<>))
