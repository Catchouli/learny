{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Exception (SomeException, try)
import qualified Data.Text as T
import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Config
import           Snap.Core
import           System.IO
import           Site

#ifdef DEVELOPMENT
import           Snap.Loader.Dynamic
#else
import           Snap.Loader.Static
#endif


-- This is the entry point for this web server application. It supports easily
-- switching between interpreting source and running statically compiled code
main :: IO ()
main = do
    -- Depending on the version of loadSnapTH in scope, this either enables
    -- dynamic reloading, or compiles it without. The last argument to
    -- loadSnapTH is a list of additional directories to watch for changes to
    -- trigger reloads in development mode. It doesn't need to include source
    -- directories, those are picked up automatically by the splice.
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                          'getActions
                                          ["snaplets/heist/templates"])

    _ <- try $ httpServe conf site :: IO (Either SomeException ())
    cleanup


-- The snap config
-- This action is only run once, regardless of whether development or
-- production mode is in use.
getConf :: IO (Config Snap AppConfig)
getConf = commandLineAppConfig $ id
            . setSSLPort 8080
            . setSSLCert "/etc/letsencrypt/live/dev.learny.click/fullchain.pem"
            . setSSLKey "/etc/letsencrypt/live/dev.learny.click/privkey.pem"
            . setSSLChainCert True
            $ mempty


-- This function generates the the site handler and cleanup action from the
-- configuration. In production mode, this action is only run once. In
-- development mode, this action is run whenever the application is reloaded.
--
-- Development mode also makes sure that the cleanup actions are run
-- appropriately before shutdown. The cleanup action returned from loadSnapTH
-- should still be used after the server has stopped handling requests, as the
-- cleanup actions are only automatically run when a reload is triggered.
--
-- This sample doesn't actually use the config passed in, but more
-- sophisticated code might.
getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
    (msgs, site, cleanup) <- runSnaplet
        (appEnvironment =<< getOther conf) app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)
