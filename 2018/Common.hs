module Common where

replace :: String -> Char -> String -> String
replace chars target s = map (\c -> if (elem c chars) then target else c) s

remove :: String -> String -> String
remove chars s = filter (\c -> not $ elem c chars) s

swap :: (Int, Int) -> (Int, Int)
swap (a, b) = (b, a)
