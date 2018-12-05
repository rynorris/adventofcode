import Data.Char
import Debug.Trace

data Unit = Unit Char Bool deriving (Show, Eq)

unitType :: Unit -> Char
unitType (Unit c _) = c

unitPolarity :: Unit -> Bool
unitPolarity (Unit _ b) = b

unitToChar :: Unit -> Char
unitToChar (Unit c True) = toLower c
unitToChar (Unit c False) = toUpper c

constructUnit :: Char -> Unit
constructUnit c = Unit (toLower c) (isLower c)

parse :: String -> [Unit]
parse = map constructUnit

shouldCancel :: Unit -> Unit -> Bool
shouldCancel (Unit c1 p1) (Unit c2 p2) = (c1 == c2) && (p1 /= p2)

deleteTwo :: (a -> Bool) -> [a] -> [a]
deleteTwo f (u:v:us) | (f u) = us
                     | otherwise = u : deleteTwo f (v:us)
deleteTwo f (u:us) = u:us
deleteTwo _ [] = []

simplify :: [Unit] -> [Unit]
simplify units = (\us -> traceShow (length us) us) $ map fst $ deleteTwo (uncurry shouldCancel) $ zip units ((drop 1 units) ++ [Unit '!' True])

simplest :: [Unit] -> [Unit]
simplest us | (length simpler) == (length us) = us
            | otherwise = simplest simpler
    where simpler = simplify us

solve :: [Unit] -> Int
solve = length . simplest

main :: IO()
main = interact (show . solve . parse)

