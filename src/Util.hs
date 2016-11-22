module Util where

import Application
import Control.Conditional
import Snap.Snaplet.Auth
import Snap
import Heist
import Snap.Core
import Snap.Snaplet.Heist
import qualified Heist.Interpreted as I
import qualified Data.ByteString as BS
import qualified Data.Text as T

requireLogin :: Handler App (AuthManager App) () -> Handler App (AuthManager App) ()
requireLogin action = ifM isLoggedIn action (redirect "/")

renderWithError :: BS.ByteString -> Maybe T.Text -> Handler App (AuthManager App) ()
renderWithError url err = heistLocal (I.bindSplices splices) (render url)
  where
    splices = maybe mempty splice err
    splice e = "error" ## I.textSplice e


