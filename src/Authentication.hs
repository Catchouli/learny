{-# LANGUAGE OverloadedStrings #-}

module Authentication
  ( handleLogin
  , handleLogout
  , handleNewUser
  )
where

import Control.Applicative
import Control.Conditional
import qualified Data.Text as T
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Heist
import Heist
import qualified Heist.Interpreted as I
import Application


-- Render login form
renderLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
renderLogin authError = ifM isLoggedIn (redirect "/") loginHandler
  where
    loginHandler = heistLocal (I.bindSplices errs) $ render "login"
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err


-- Handle rendering the login form and login submissions
handleLogin :: Handler App (AuthManager App) ()
handleLogin = method GET (renderLogin Nothing) <|> method POST withErr
  where
    withErr = loginUser "login" "password" Nothing
                        (\_ -> renderLogin err) (redirect "/")
    err = Just "Unknown user or password"


-- Handle log outs
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


-- Handle rendering the sign up page and new user submissions
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"
