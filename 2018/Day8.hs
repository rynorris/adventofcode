module Day8 where

data Tree = Tree { root :: Node } deriving (Show, Eq)

data Node = Node { children :: [Node], metadata :: [Int] } deriving (Show, Eq)

takeNode :: [Int] -> (Node, [Int])
takeNode (c:m:xs) = (Node cs ms, drop m rem) where
    ms = take m rem
    (cs, rem) = takeNodes c xs

takeNodes :: Int -> [Int] -> ([Node], [Int])
takeNodes n xs | n == 0 = ([], xs)
               | otherwise = let (node, rem) = takeNode xs in (\(ns, re) -> (node:ns, re)) $ takeNodes (n-1) rem

parse :: String -> Tree
parse = Tree . fst . takeNode . map read . words

