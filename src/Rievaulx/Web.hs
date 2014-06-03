{-# LANGUAGE OverloadedStrings #-}

module Rievaulx.Web
       ( runServer
       ) where

import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static (staticPolicy, addBase)

complete req = responseLBS status200 [("Content-Type", "text/html")]
               "Hello, world!"

notFound = responseLBS status404 [("Content-Type", "text/html")]
           "<h1>404 Not Found</h1>"

application :: Application
application req = return $ case rawPathInfo req of
  "/complete" -> complete req
  _ -> notFound

staticMiddleware :: Middleware
staticMiddleware = staticPolicy $ addBase "./public"

runServer :: IO ()
runServer = do
  putStrLn $ "Rievaulx is starting on http://localhost:3000/ ..."
  run 3000 $ staticMiddleware $ application
