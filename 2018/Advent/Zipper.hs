module Advent.Zipper where

data Zipper a = Zipper ![a] !a ![a] deriving Show

next :: Zipper a -> Zipper a
next (Zipper [] v []) = Zipper [] v []
next (Zipper pre v []) = Zipper [v] (head r) (tail r) where r = reverse pre
next (Zipper pre v suf) = Zipper (v:pre) (head suf) (tail suf)

prev :: Zipper a -> Zipper a
prev (Zipper [] v []) = Zipper [] v []
prev (Zipper [] v suf) = Zipper (tail r) (head r) [v] where r = reverse suf
prev (Zipper pre v suf) = Zipper (tail pre) (head pre) (v:suf)

rotate :: Int -> Zipper a -> Zipper a
rotate 0 z = z
rotate n z | n > 0 = rotate (n-1) (prev z)
           | otherwise = rotate (n+1) (next z)

pop :: Zipper a -> (Zipper a, a)
pop (Zipper [] v []) = error "Zipper would be empty"
pop (Zipper pre v []) = (Zipper [] (head r) (tail r), v) where r = reverse pre
pop (Zipper pre v suf) = (Zipper pre (head suf) (tail suf), v)

push :: a -> Zipper a -> Zipper a
push x (Zipper pre v suf) = Zipper pre x (v:suf)

asList :: Zipper a -> [a]
asList (Zipper pre v suf) = (reverse pre) ++ [v] ++ suf

