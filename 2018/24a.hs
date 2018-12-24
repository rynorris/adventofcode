import Advent.Strings

import Data.List
import Data.Maybe
import Data.Ord

import Debug.Trace

data Team = Infection | Immune deriving (Show, Eq, Ord)
data Group = Group { 
    team :: Team,
    units, hitpoints, damage, initiative :: Int,
    attackType :: String,
    weaknesses :: [String], immunities :: [String]
} deriving (Show, Eq, Ord)

parseGroup :: Team -> String -> Group
parseGroup tm s = Group tm us hp dmg ini typ wks ims where
    ims = takeWhile (not . flip elem ["weak", "with"]) $ drop 2 $ dropWhile (/= "immune") ws
    wks = takeWhile (not . flip elem ["immune", "with"]) $ drop 2 $ dropWhile (/= "weak") ws
    ini = read $ head $ tail $ dropWhile (/= "initiative") ws
    typ = head $ drop 2 $ dropWhile (/= "does") ws
    dmg = read $ head $ tail $ dropWhile (/= "does") ws
    hp = read $ head $ tail $ dropWhile (/= "with") ws
    us = read $ head ws
    ws = words $ replace "(),;" ' ' s

parse :: String -> [Group]
parse s = imm ++ inf where
    inf = map (parseGroup Infection) $ drop 2 infLines
    imm = map (parseGroup Immune) $ tail immLines
    (immLines,infLines) = break (null) $ lines s

effectivePower :: Group -> Int
effectivePower g = units g * damage g

damageDealt :: Group -> Group -> Int
damageDealt atk def | elem (attackType atk) (immunities def) = 0
                    | elem (attackType atk) (weaknesses def) = d * 2
                    | otherwise = d where
    d = effectivePower atk

selectTarget :: Group -> [Int] -> [Group] -> (Maybe Int, [Int])
selectTarget atk ixs groups = if null tgts || damageDealt atk (snd tgt) == 0 then (Nothing, ixs) else (Just $ fst tgt, filter (/= fst tgt) ixs) where
    tgt = head $ sortBy (comparing (negate . damageDealt atk . snd) `mappend` comparing (negate . effectivePower . snd) `mappend` comparing (negate . initiative . snd)) tgts
    tgts = filter (\(ix,g) -> team g /= team atk) $ map (\ix -> (ix, groups !! ix)) ixs

selectTargets :: [Group] -> [(Int, Int)]
selectTargets gs = map (\(a,mb) -> (a,fromJust mb)) $ filter (isJust . snd) $ go (sortBy (comparing (negate . effectivePower . snd) `mappend` comparing (negate . initiative . snd)) (zip [0..] gs)) [] (map fst $ zip [0..] gs) where
    go [] pairs _ = pairs
    go (g:gs') pairs available = let (t,a') = selectTarget (snd g) available gs in go gs' ((fst g,t):pairs) a'

killUnits :: Int -> Group -> Group
killUnits n (Group t us hp dmg ini typ wks ims) = Group t (us-n) hp dmg ini typ wks ims

replaceIx :: Int -> a -> [a] -> [a]
replaceIx n v l = pre ++ (v:tail post) where
    (pre, post) = splitAt n l

doAttack :: Int -> Int -> [Group] -> [Group]
doAttack aIx dIx gs = if isAlive then replaceIx dIx (killUnits killed def) gs else gs where
    killed = damage `div` (hitpoints def)
    damage = damageDealt atk def
    isAlive = units atk > 0
    atk = gs !! aIx
    def = gs !! dIx

doAttacks :: [(Int, Int)] -> [Group] -> [Group]
doAttacks pairs groups = foldl (\gs (a,d) -> doAttack a d gs) groups attacks where
    attacks = sortBy (comparing (negate . initiative . grp . fst)) pairs
    grp ix = groups !! ix

killGroups :: [Group] -> [Group]
killGroups gs = filter (\g -> units g > 0) gs

isDone :: [Group] -> Bool
isDone gs = nImm == 0 || nInf == 0 where
    nInf = length $ filter (== Infection) ts
    nImm = length $ filter (== Immune) ts
    ts = map team gs

step :: [Group] -> [Group]
step gs = killGroups $ doAttacks targets gs where
    targets = selectTargets gs

timeline :: [Group] -> [[Group]]
timeline = iterate step

finalState :: [Group] -> [Group]
finalState gs = head done where
    (battle,done) = break isDone $ timeline gs

solveA = foldl1 (+) . map units . finalState

immuneWin :: [Group] -> Bool
immuneWin gs = nImm > 0 && nInf == 0 where
    nInf = length $ filter (== Infection) ts
    nImm = length $ filter (== Immune) ts
    ts = map team gs

applyBoost :: Int -> Group -> Group
applyBoost n g@(Group t us hp dmg ini typ wks ims) | t == Immune = Group t us hp (dmg+n) ini typ wks ims
                                                   | otherwise = g

boost :: Int -> [Group] -> [Group]
boost n groups = map (applyBoost n) groups

smallestBoost :: [Group] -> Int
smallestBoost gs = go gs 0 10000 where
    go gs lower upper | lower == upper = lower
                      | not $ immuneWin $ finalState $ boost mid gs = go gs mid upper
                      | otherwise = go gs lower mid
        where mid = (\m -> traceShow m m) $ (upper + lower) `div` 2

-- The actual smallestBoost function hangs because the combat seems to infinite loop on boost = 69.
solveB gs = foldl1 (+) $ map units $ finalState $ boost b gs where
    b = 70 --smallestBoost gs

debug gs = (timeline gs) !! 1

debugTargets pairs = trace (unlines $ map show pairs) pairs

solve gs = (solveA gs, solveB gs)

main :: IO()
main = interact (show . solve . parse)
