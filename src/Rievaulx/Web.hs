{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

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
import           System.Console.CmdArgs (Data, Typeable, (&=), typ, help, program, cmdArgs)

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

port :: Int
port = 3000

runServer :: IO ()
runServer = do
  opts <- cmdArgs options
  let filename = wordsFile opts
  words <- getRandomizedWords filename
  let completionCandidates = insertMany words
  putStrLn $ "Rievaulx is serving \"" ++ filename ++ "\" on http://localhost:" ++ show port ++ "/ ..."
  run port $ staticMiddleware $ createApp completionCandidates

data Options = Options
               { wordsFile :: String
               } deriving (Show, Data, Typeable)

options :: Options
options = Options { wordsFile = "/usr/share/dict/words"
                             &= typ "FILE"
                             &= help "A file containing a list of words to use"
                  }
          &= program "rievaulx"
