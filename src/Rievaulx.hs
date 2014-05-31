module Rievaulx where

import           Data.List (foldl')
import qualified Data.Tree
import qualified Data.Tree.Pretty

data TernaryTree a = EmptyNode
                   | Node { nodeVal :: a
                          , nodeLeft :: TernaryTree a
                          , nodeCenter :: TernaryTree a
                          , nodeRight :: TernaryTree a
                          , nodeEnd :: Bool
                          }
                   deriving (Show, Eq)

--
-- Tree manipulation
--

leaf :: a -> Bool -> TernaryTree a
leaf x e = Node x EmptyNode EmptyNode EmptyNode e

insert :: (Ord a) => [a] => TernaryTree a => TernaryTree a
insert [] t = t
insert (x:[]) EmptyNode = leaf x True
insert (x:xs) EmptyNode = Node x EmptyNode (insert xs EmptyNode) EmptyNode False
insert s@(x:xs) (Node v lt ct rt e)
  | x < v = Node v (insert s lt) ct rt e
  | x > v = Node v lt ct (insert s rt) e
  | otherwise = Node v lt (insert xs ct) rt e

insertMany :: (Ord a) => [[a]] -> TernaryTree a
insertMany = foldl' (flip insert) EmptyNode

--
-- Lookups
--

contains :: (Ord a) => TernaryTree a -> [a] -> Bool
contains EmptyNode _ = False
contains (Node v _ _ _ e) (x:[]) = e && v == x
contains (Node v lt ct rt _) s@(x:xs)
  | x < v = contains lt s
  | x > v = contains rt s
  | otherwise = contains ct xs

--
-- Printing
--

drawTernaryTree :: (Show a) => TernaryTree a => IO ()
drawTernaryTree = putStrLn . Data.Tree.drawTree . toDataTree

drawVerticalTernaryTree :: (Show a) => TernaryTree a => IO ()
drawVerticalTernaryTree = putStrLn . Data.Tree.Pretty.drawVerticalTree . toDataTree

nodeLabel :: (Show a) => a -> Bool -> String
nodeLabel x b = show x ++ " " ++ if b then "T" else "F"

toDataTree :: (Show a) => TernaryTree a -> Data.Tree.Tree String
toDataTree EmptyNode = Data.Tree.Node "#" []
toDataTree (Node v EmptyNode EmptyNode EmptyNode e) =
  Data.Tree.Node (nodeLabel v e) []
toDataTree (Node v lt ct rt e) =
  Data.Tree.Node (nodeLabel v e) [toDataTree lt, toDataTree ct , toDataTree rt]
