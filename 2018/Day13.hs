module Day13 where

import Data.Either
import Data.List
import qualified Data.Map.Strict as Map

import Advent.Plane

type Track = Map.Map Coord TrackPiece
data TrackPiece = Vert | Horiz | DiagUp | DiagDown | Cross deriving (Eq, Ord, Show)

data Turn = AntiClock | Straight | Clock deriving (Eq, Ord, Show)
data Cart = Cart { position :: Coord, velocity :: Coord, willTurn :: Turn } deriving (Eq, Ord, Show)

parseObject :: (Coord, Char) -> [Either Cart (Coord, TrackPiece)]
parseObject (c, '|') = [Right (c, Vert)]
parseObject (c, '-') = [Right (c, Horiz)]
parseObject (c, '/') = [Right (c, DiagUp)]
parseObject (c, '\\') = [Right (c, DiagDown)]
parseObject (c, '+') = [Right (c, Cross)]
parseObject (c, '<') = [Right (c, Horiz), Left (Cart c (Coord (-1) 0) AntiClock)]
parseObject (c, '^') = [Right (c, Vert), Left (Cart c (Coord 0 (-1)) AntiClock)]
parseObject (c, '>') = [Right (c, Horiz), Left (Cart c (Coord 1 0) AntiClock)]
parseObject (c, 'v') = [Right (c, Vert), Left (Cart c (Coord 0 1) AntiClock)]

parseObjects :: String -> [Either Cart (Coord, TrackPiece)]
parseObjects s = concat $ map parseObject $ filter ((/= ' ') . snd) cs where
    cs = concat $ map (\(y,l) -> [(Coord x y, c) | (x, c) <- zip [0..] l]) $ zip [0..] $ lines s

parse :: String -> (Track, [Cart])
parse s = (Map.fromList $ rights objects, lefts objects) where
    objects = parseObjects s

move :: Cart -> Cart
move (Cart p v t) = Cart (coordAdd p v) v t

turnCoord :: Turn -> Coord -> Coord
turnCoord Straight c = c
turnCoord Clock (Coord x y) = Coord (-y) x
turnCoord AntiClock (Coord x y) = Coord y (-x)

nextTurn :: Turn -> Turn
nextTurn AntiClock = Straight
nextTurn Straight = Clock
nextTurn Clock = AntiClock

turn :: TrackPiece -> Cart -> Cart
turn Vert c = c
turn Horiz c = c
turn DiagUp (Cart p v t) | coordX v == 0 = Cart p (turnCoord Clock v) t
                         | otherwise = Cart p (turnCoord AntiClock v) t
turn DiagDown (Cart p v t) | coordX v == 0 = Cart p (turnCoord AntiClock v) t
                           | otherwise = Cart p (turnCoord Clock v) t
turn Cross (Cart p v t) = Cart p (turnCoord t v) (nextTurn t)

stepCart :: Track -> Cart -> Cart
stepCart track cart = turn piece movedCart where
    piece = (track Map.! (position movedCart))
    movedCart = move cart

