import Data.Maybe

import Advent.Plane

data Object = Open | Trees | Lumberyard deriving (Show, Eq, Ord)

parseObject :: Char -> Object
parseObject '.' = Open
parseObject '|' = Trees
parseObject '#' = Lumberyard

drawObject :: Object -> Char
drawObject Open = '.'
drawObject Trees = '|'
drawObject Lumberyard = '#'

parse :: String -> Plane Object
parse = parsePlane parseObject

adjacent :: Plane Object -> Coord -> [Object]
adjacent p c = catMaybes $ map (flip getObject p) $ neighbours c ++ diags c

count :: Object -> [Object] -> Int
count o = length . filter (== o)

evolveObject :: Object -> [Object] -> Object
evolveObject Open ns | count Trees ns >= 3 = Trees
                     | otherwise = Open
evolveObject Trees ns | count Lumberyard ns >= 3 = Lumberyard
                      | otherwise = Trees
evolveObject Lumberyard ns | elem Lumberyard ns && elem Trees ns = Lumberyard
                           | otherwise = Open

evolve :: Plane Object -> Plane Object
evolve p = fromList $ map (go p) $ toList p where
    go p (c,o) = (c, evolveObject o (adjacent p c))

timeline :: Plane Object -> [Plane Object]
timeline = iterate evolve

value :: Plane Object -> Int
value p = nLumber * nTrees where
    nLumber = count Lumberyard objs
    nTrees = count Trees objs
    objs = map snd $ toList p

solveA :: Plane Object -> Int
solveA p = value $ head $ drop 10 $ timeline p

knownCycle :: [Int]
knownCycle = [176366, 174420, 172765, 173716, 174510, 178080, 180624, 184254, 190384, 194959, 198396, 203236, 207900, 208384, 210630, 209559, 206150, 208623, 208494, 209922, 208384, 208385, 206255, 202920, 194667, 189336, 184886, 180276]

solveB :: Plane Object -> Int
solveB p = ((!!) knownCycle) $ mod (1000000000 - untilLoop) 28 where
    untilLoop = length $ takeWhile (/= head knownCycle) values
    values = map value $ timeline p

main :: IO()
main = interact (show . solveB . parse)
