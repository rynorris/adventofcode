import Advent.Zipper

import Data.Function
import Data.List

import Debug.Trace

type Elf = Int
type Marble = Int
type Circle = Zipper Marble

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
