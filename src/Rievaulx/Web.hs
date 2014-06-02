{-# LANGUAGE OverloadedStrings #-}

module Rievaulx.Web
       ( application
       ) where

import Network.Wai
import Network.HTTP.Types (status200)

application _ = return $
  responseLBS status200 [("Content-Type", "text/plain")] "Hello World"
