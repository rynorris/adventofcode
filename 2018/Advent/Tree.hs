module Advent.Tree where

data Tree a = Tree { root :: Node a } deriving (Show, Eq)

data Node a = Node { children :: [Node a], metadata :: a } deriving (Show, Eq)

nodeFold :: (a -> Node b -> a) -> a -> Node b -> a
nodeFold f x n | null $ children n = f x n
               | otherwise = f (foldl (nodeFold f) x $ children n) n
