import Prelude hiding (map, lookup, replicate, filter)
import Data.Maybe (listToMaybe)

einseins = [ i*2 + 1 | i <- [1 .. 5]]

einszwei = [ (5*i,i==4) | i <- [1 .. 5], i <= 1 || i >= 4]

einsdrei = [ Just (i * i) | i <- [1 .. 5], i `mod` 2 == 1]

einsvier = [ (i, j) | i <- [1 .. 5], j <- [5,4 .. 1], i < j]

map f l = [ f y | y <- l]

lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup a l = listToMaybe [ snd i | i <- l, fst i == a]

replicate n x = [x | i <- [1 .. n]]

filter f l = [ i | i <- l, f i]