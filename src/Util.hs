module Util where

import Control.Conditional
import Snap.Snaplet.Auth
import Snap.Core

requireLogin action = ifM isLoggedIn action (redirect "/")
