import Data.Function
import Data.List

import Debug.Trace

type Elf = Int
type Marble = Int
type Circle = Zipper Marble

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

data Turn = Place Elf Marble | Collect Elf Marble deriving (Show)

data Game = Game Int Int Circle [Turn] deriving (Show)

parse :: String -> Game
parse s = Game (read $ ws !! 0) (read $ ws !! 6) (Zipper [] 0 []) []
    where ws = words s

doRemoval :: Circle -> (Circle, Marble)
doRemoval c = pop $ rotate 7 c

doAdd :: Marble -> Circle -> Circle
doAdd m = push m . rotate (-2)

playTurn :: Elf -> Marble -> Game -> Game
playTurn e m (Game n b c ts)
    | mod m 23 == 0 = let (newC, takeM) = doRemoval c in Game n b newC (Collect e m : Collect e takeM : ts)
    | otherwise = Game n b (doAdd m c) (Place e m : ts)

playGame :: Game -> Game
playGame ga@(Game n b _ _) = foldl (\g (e, m) -> playTurn e m g) ga $ map (\m -> (mod m n, m)) [1..b]

score :: Turn -> (Elf, Int)
score (Place e _) = (e, 0)
score (Collect e m) = (e, m)

scores :: Game -> [(Elf, Int)]
scores (Game _ _ _ ts) = map score ts

solve :: Game -> Int
solve g = foldl1 max $ map (foldl1 (+) . map snd) $ groupBy ((==) `on` fst) $ sort $ scores $ playGame g

main :: IO()
main = interact (show . solve . parse)
