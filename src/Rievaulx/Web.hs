{-# LANGUAGE OverloadedStrings #-}

module Rievaulx.Web
       ( runServer
       ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import           Network.HTTP.Types (status200, status404)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Static (staticPolicy, addBase)
import           Rievaulx (TernaryTree, insertMany, prefixTerms)
import           Rievaulx.Sources (getRandomizedWords)

notFound :: Response
notFound = responseLBS status404 [("Content-Type", "text/html")]
           "<h1>404 Not Found</h1>"

createApp :: TernaryTree Char -> Application
createApp tree = application
  where application req = return $ case pathInfo req of
          (_:word:[]) -> responseLBS status200 [("Content-Type", "application/json")]
                         (Aeson.encode . prefixTerms tree $ T.unpack word)
          _ -> notFound

staticMiddleware :: Middleware
staticMiddleware = staticPolicy $ addBase "./public"

runServer :: IO ()
runServer = do
  words <- getRandomizedWords "/usr/share/dict/words"
  let completionCandidates = insertMany words
  putStrLn $ "Rievaulx is starting on http://localhost:3000/ ..."
  run 3000 $ staticMiddleware $ createApp completionCandidates
