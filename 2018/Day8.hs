module Day8 where

import Advent.Tree

type Metadata = [Int]

takeNode :: [Int] -> (Node Metadata, [Int])
takeNode (c:m:xs) = (Node cs ms, drop m rem) where
    ms = take m rem
    (cs, rem) = takeNodes c xs

takeNodes :: Int -> [Int] -> ([Node Metadata], [Int])
takeNodes n xs | n == 0 = ([], xs)
               | otherwise = let (node, rem) = takeNode xs in (\(ns, re) -> (node:ns, re)) $ takeNodes (n-1) rem

parse :: String -> Tree Metadata
parse = Tree . fst . takeNode . map read . words

