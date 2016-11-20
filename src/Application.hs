{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- This module defines our application's state type and an alias for its
-- handler monad.
module Application where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.Persistent

data App = App
    { _heist   :: Snaplet (Heist App)
    , _db      :: Snaplet PersistState
    , _sess    :: Snaplet SessionManager
    , _auth    :: Snaplet (AuthManager App)
    }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasPersistPool (Handler b App) where
  getPersistPool = with db getPersistPool

instance HasPersistPool (Handler App (AuthManager App)) where
    getPersistPool = withTop db getPersistPool
