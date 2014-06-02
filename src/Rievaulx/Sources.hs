module Rievaulx.Sources
       ( shuffle
       , getWords
       , getRandomizedWords
       ) where

import Control.Monad
import Data.Array.IO
import System.Random

-- | Randomly shuffle a list
--   /O(N)/
-- from: http://www.haskell.org/haskellwiki/Random_shuffle

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1..n] $ \i -> do
    j <- randomRIO (i,n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

--
-- Load words from file source
--

getWords :: FilePath -> IO [String]
getWords fp = readFile fp >>= \c -> return . lines $ c

getRandomizedWords :: FilePath -> IO [String]
getRandomizedWords = shuffle <=< getWords
