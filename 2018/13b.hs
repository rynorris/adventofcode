import Day13

import Data.List

import Advent.Plane

findFinalCartLocation :: Track -> [Cart] -> Coord
findFinalCartLocation track carts = go track (sort carts) [] where
    go t order moved | null order = if length moved == 1 then (position $ head moved) else go t (sort moved) []
                     | otherwise = go t (filter (not . collide) $ tail order) newMoved where
        newMoved = if anyCollisions then (filter (not . collide) moved) else (movedCart:moved)
        anyCollisions = not $ null $ (filter collide $ tail order) ++ (filter collide moved)
        collide = (== (position movedCart)) . position
        movedCart = stepCart t $ head order

solve :: (Track, [Cart]) -> Coord
solve (t, cs) = findFinalCartLocation t cs

main :: IO()
main = interact (show . solve . parse)

