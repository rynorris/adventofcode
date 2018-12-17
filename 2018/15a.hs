import Advent.Plane

import Data.Function
import Data.List
import Data.Maybe

import Debug.Trace

data Object = Elf Int | Goblin Int | Wall deriving (Show, Eq, Ord)
type Battlefield = Plane Object

parseObject :: Char -> Maybe Object
parseObject '#' = Just Wall
parseObject 'G' = Just (Goblin 200)
parseObject 'E' = Just (Elf 200)
parseObject _ = Nothing

parse :: String -> Battlefield
parse s = fromList $ concat $ map (\(y,l) -> [(Coord x y, o) | (x, o) <- parseLine l]) $ zip [0..] $ lines s where
    parseLine l = map (\(x, m) -> (x, fromJust m)) $ filter (\(x, m) -> isJust m) $ zip [0..] $ map parseObject l

isGoblin :: Object -> Bool
isGoblin (Goblin _) = True
isGoblin _ = False

isElf :: Object -> Bool
isElf (Elf _) = True
isElf _ = False

isAnimate :: Object -> Bool
isAnimate (Elf _) = True
isAnimate (Goblin _) = True
isAnimate _ = False

hitpoints :: Object -> Int
hitpoints (Elf hp) = hp
hitpoints (Goblin hp) = hp
hitpoints _ = 0

targets :: Object -> Battlefield -> [(Coord, Object)]
targets (Elf _) b = filter (isGoblin . snd) $ toList b
targets (Goblin _) b = filter (isElf . snd) $ toList b

isEmptySpace :: Battlefield -> Coord -> Bool
isEmptySpace b c = isNothing $ getObject c b

emptyNeighbours :: Coord -> Battlefield -> [Coord]
emptyNeighbours c b = filter (isEmptySpace b) $ neighbours c

adjacent :: Coord -> Coord -> Bool
adjacent c d = elem c $ neighbours d

distance :: Battlefield -> Coord -> Coord -> Int
distance b c d = go b [c] d 0 where
    go b cs d x | x > 100 = 100
                | not $ null $ filter (adjacent d) cs = x+1
                | otherwise = go b (nub $ sort $ filter (isEmptySpace b) $ concat $ map neighbours cs) d (x+1)

closest :: Battlefield -> Coord -> [Coord] -> Coord
closest b c cs = head $ sortBy readingOrder $ map snd $ head $ groupBy ((==) `on` fst) $ sort $ map (\x -> (distance b c x, x)) cs

move :: Battlefield -> Coord -> Object -> Coord
move b c o | not $ null $ filter (adjacent c . fst) es = c
           | null ts = c
           | otherwise = step
    where
        step = closest b t $ emptyNeighbours c b
        t = closest b c ts
        ts = concat $ map (flip emptyNeighbours b) $ map fst es
        es = targets o b

attackTarget :: Battlefield -> Coord -> Object -> Maybe (Coord, Object)
attackTarget b c o | null ts = Nothing
                   | otherwise = Just (head ts)
    where ts = filter (adjacent c . fst) $ targets o b

hit :: Object -> Maybe Object
hit (Elf hp) = if hp > 3 then Just (Elf (hp-3)) else Nothing
hit (Goblin hp) = if hp > 3 then Just (Goblin (hp-3)) else Nothing
hit _ = Nothing

doAttack :: Coord -> Object -> Battlefield -> Battlefield
doAttack c o b | isNothing t = b
               | otherwise = if isNothing to' then removeObject tc b else addObject tc (fromJust to') b
    where 
        tc = fst $ fromJust t
        to' = hit $ snd $ fromJust t
        t = attackTarget b c o

timeline :: Battlefield -> [Battlefield]
timeline b = go b [] (-1) where
    go b [] x = b : go b (sortBy (readingOrder `on` fst) $ filter (isAnimate . snd) $ toList b) (x+1)
    go b ((c,o):os) x = go b' os' x where
        os' = filter (\z -> elem z (toList b')) os
        b' = doAttack c' o $ addObject c' o $ removeObject c b
        c' = move b c o

isOver :: Battlefield -> Bool
isOver b = null es || null gs where
    es = filter (isElf . snd) $ toList b
    gs = filter (isGoblin . snd) $ toList b

drawObject :: Object -> Char
drawObject (Elf _) = 'E'
drawObject (Goblin _) = 'G'
drawObject Wall = '#'

solve b = (* steps) $ sum $ map (hitpoints . snd) $ toList final where
    final = head $ dropWhile (not . isOver) $ timeline b
    steps = length $ takeWhile (not . isOver) $ timeline b

main :: IO()
main = interact (show . solve . parse)
