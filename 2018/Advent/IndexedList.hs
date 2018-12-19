module Advent.IndexedList where

import qualified Data.IntMap.Strict as IntMap

data IndexedList a = IndexedList (IntMap.IntMap a) Int deriving (Show, Eq, Ord)

emptyList :: IndexedList a
emptyList = IndexedList IntMap.empty 0

append :: IndexedList a -> a -> IndexedList a
append (IndexedList xs n) x = IndexedList (IntMap.insert n x xs) (n+1)

listLength :: IndexedList a -> Int
listLength (IndexedList _ n) = n

getIndex :: IndexedList a -> Int -> a
getIndex (IndexedList xs _) ix = xs IntMap.! ix
