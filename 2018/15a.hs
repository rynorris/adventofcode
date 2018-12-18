import Advent.Plane

import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as Set

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
distance b c d = go b (Set.singleton c) (Set.empty) d 0 where
    go b cs is d x | x > 100 = 100
                   | elem d cs = x
                   | not $ Set.null $ Set.filter (adjacent d) cs = x+1
                   | otherwise = go b (Set.filter (isEmptySpace b) $ foldl (\s c -> Set.insert c s) cs $ filter (not . flip Set.member is) $ concat $ map neighbours $ Set.toList cs) (Set.union is cs) d (x+1)

closest1 :: Battlefield -> Coord -> [Coord] -> Maybe Coord
closest1 b c cs = Just $ head $ sortBy readingOrder $ map snd $ head $ groupBy ((==) `on` fst) $ sort $ map (\x -> (distance b c x, x)) cs

closest2 :: Battlefield -> Coord -> [Coord] -> Maybe Coord
closest2 b c cs = go b (Set.singleton c) (Set.fromList cs) 0 where
    go b wave cs n | n > 100 = Nothing
                   | not $ Set.null hit = Just $ head $ sortBy readingOrder $ Set.toList hit
                   | otherwise = go b (foldl (flip Set.insert) wave $ filter (isEmptySpace b) $ concat $ map neighbours $ Set.toList wave) cs (n+1)
        where hit = Set.intersection wave cs

closest = closest2

move :: Battlefield -> Coord -> Object -> Coord
move b c o | not $ null $ filter (adjacent c . fst) es = c
           | null ts = c
           | null ns = c
           | isNothing t = c
           | isNothing step = c
           | distance b (fromJust t) (fromJust step) == 100 = c
           | otherwise = fromJust step
    where
        step = closest b (fromJust t) ns
        ns = emptyNeighbours c b
        t = closest b c ts
        ts = concat $ map (flip emptyNeighbours b) $ map fst es
        es = targets o b

attackTarget :: Battlefield -> Coord -> Object -> Maybe (Coord, Object)
attackTarget b c o | null ts = Nothing
                   | otherwise = Just (head $ sortBy ((compare `on` (hitpoints . snd)) <> (readingOrder `on` fst)) ts)
    where ts = filter (adjacent c . fst) $ targets o b

hit :: Object -> Maybe Object
hit (Elf hp) = if hp > 3 then Just (Elf (hp-3)) else Nothing
hit (Goblin hp) = if hp > 20 then Just (Goblin (hp-20)) else Nothing
hit _ = Nothing

doAttack :: Coord -> Object -> Battlefield -> [Coord] -> (Battlefield, [Coord])
doAttack c o b cs | isNothing t = (b, cs)
                  | otherwise = (b', cs')
    where 
        b' = if isNothing to' then removeObject tc b else addObject tc (fromJust to') b
        cs' = if isNothing to' then filter (/= tc) cs else cs
        tc = fst $ fromJust t
        to' = hit $ snd $ fromJust t
        t = attackTarget b c o

timeline :: Battlefield -> [Battlefield]
timeline b = go b [] (-1) where
    go b [] x = b : go b (sortBy readingOrder $ map fst $ filter (isAnimate . snd) $ toList b) (x+1)
    go b (c:cs) x | isNothing mo = go b cs x
                  | otherwise = go b' cs' x where
        (b', cs') = (\bf -> doAttack c' o bf cs) $ addObject c' o $ removeObject c b
        c' = move b c o
        o = fromJust mo
        mo = getObject c b

isOver :: Battlefield -> Bool
isOver b = null es || null gs where
    es = filter (isElf . snd) $ toList b
    gs = filter (isGoblin . snd) $ toList b

drawObject :: Object -> Char
drawObject (Elf _) = 'E'
drawObject (Goblin _) = 'G'
drawObject Wall = '#'

totalHealth :: Battlefield -> Int
totalHealth = sum . map (hitpoints . snd) . toList

solve b = (\x -> (steps, x, filter ((/= Wall) . snd) $ toList final)) $ totalHealth final where
    final = head $ steady
    steps = length battle
    (battle, steady) = break isOver $ timeline b

countElves :: Battlefield -> Int
countElves b = length $ filter (isElf . snd) $ toList b

solveB b = isOver $ head steady where
    (battle, steady) = break (\bf -> isOver bf || countElves bf /= nElf) $ timeline b
    nElf = countElves b

doSteps n = drawPlane drawObject . (\b -> traceShow (totalHealth b, filter (isAnimate . snd) $ toList b) b) . head . drop n . timeline
debug b = zip [0..] $ map totalHealth (battle ++ [(head steady)]) where
    (battle, steady) = break isOver $ timeline b

allSteps b = unlines $ map (drawPlane drawObject) $ timeline b

main :: IO()
main = interact (show . solve . parse)
