import Day13

import Data.List

import Advent.Plane

collisions :: [Cart] -> [Coord]
collisions = map head . filter (\l -> length l > 1) . group . sort . map position

findFirstCollision :: Track -> [Cart] -> Coord
findFirstCollision track carts = go track carts 0 where
    go t cs ix | ix >= length cs = go t (sort cs) 0
               | null cols = go t ((take ix cs) ++ (stepCart t (cs !! ix)):(drop (ix+1) cs)) (ix+1)
               | otherwise = head cols where
        cols = collisions cs

solve :: (Track, [Cart]) -> Coord
solve (t, cs) = findFirstCollision t cs

main :: IO()
main = interact (show . solve . parse)
