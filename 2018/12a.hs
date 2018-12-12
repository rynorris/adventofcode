import qualified Data.Map.Strict as Map

import Advent.Function

import Debug.Trace

data Pot = Full | Empty deriving (Show, Eq, Ord)
data State = State { leftmost :: Int, pots :: [Pot], rules :: Map.Map [Pot] Pot } deriving (Show, Eq, Ord)

parsePot :: Char -> Pot
parsePot '#' = Full
parsePot '.' = Empty

parseInitial :: String -> [Pot]
parseInitial = map parsePot . head . drop 2 . words

parseRule :: String -> ([Pot], Pot)
parseRule s = (map parsePot key, parsePot $ head value) where (key:_:value:_) = words s

parseRules :: [String] -> Map.Map [Pot] Pot
parseRules = Map.fromList . map parseRule

parse :: String -> State
parse s = State 0 (parseInitial initial) (parseRules rules) where (initial:_:rules) = lines s

grow :: State -> State
grow (State ix ps rs) = State (ix - 2) nextPs rs where
    nextPs = windowsWith (\l -> Map.findWithDefault Empty l rs) 5 $ padding ++ ps ++ padding
    padding = take 4 $ repeat Empty

trim :: State -> State
trim (State ix ps rs) = State (ix + startEmpty) trimmedPs rs where
    trimmedPs = reverse $ dropWhile (== Empty) $ reverse $ drop startEmpty ps
    startEmpty = length $ takeWhile (== Empty) ps
    
score :: State -> Int
score s = foldl1 (+) $ map snd $ filter ((== Full) . fst) $ (\s -> zip (pots s) [(leftmost s)..]) s

solve :: State -> Int
solve s = score $ repeatedly 20 ((\s -> traceShow (score s) s) . trim . grow) s

main :: IO()
main = interact (show . solve . parse)
