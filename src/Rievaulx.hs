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
leaf x = Node x EmptyNode EmptyNode EmptyNode

insert :: (Ord a) => [a] -> TernaryTree a -> TernaryTree a
insert [] t = t
insert (x:[]) EmptyNode = leaf x True
insert (x:xs) EmptyNode = Node x EmptyNode (insert xs EmptyNode) EmptyNode False
insert s@(x:xs) (Node v lt ct rt e) = case compare x v of
  LT -> Node v (insert s lt) ct rt e
  GT -> Node v lt ct (insert s rt) e
  _  -> Node v lt (insert xs ct) rt e

insertMany :: (Ord a) => [[a]] -> TernaryTree a
insertMany = foldl' (flip insert) EmptyNode

--
-- Lookups
--

contains :: (Ord a) => TernaryTree a -> [a] -> Bool
contains EmptyNode _ = False
contains (Node v _ _ _ e) (x:[]) = e && v == x
contains (Node v lt ct rt _) s@(x:xs) = case compare x v of
  LT -> contains lt s
  GT -> contains rt s
  _  -> contains ct xs

getPrefixTree :: (Ord a) => TernaryTree a -> [a] -> Maybe (TernaryTree a)
getPrefixTree EmptyNode _ = Nothing
getPrefixTree _ [] = Nothing
getPrefixTree (Node v lt ct rt _) s@(x:xs) = case compare x v of
  LT -> getPrefixTree lt s
  GT -> getPrefixTree rt s
  _  -> if length xs == 0
        then Just ct
        else getPrefixTree ct xs

collect :: (Ord a) => TernaryTree a -> [a] -> [[a]]
collect EmptyNode _ = []
collect (Node v lt ct rt e) run =
  (collect lt run) ++ termRun ++ (collect ct currRun) ++ (collect rt run)
  where currRun = run ++ [v]
        termRun = if e then [currRun] else []

prefixTerms :: (Ord a) => TernaryTree a -> [a] -> [[a]]
prefixTerms _ [] = []
prefixTerms t p = case getPrefixTree t p of
  Nothing -> []
  Just prefixTree -> collect prefixTree p

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
