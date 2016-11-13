{-# LANGUAGE OverloadedStrings #-}

module Config where

-- Authentication expiry in seconds
authExpiry :: Num a => a
authExpiry = 3600

-- Site key path
siteKeyPath :: FilePath
siteKeyPath = "private/site_key.txt"
