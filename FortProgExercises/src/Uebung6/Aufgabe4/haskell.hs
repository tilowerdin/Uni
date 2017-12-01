{- 

\x y -> f (g x) y       f . g
\x y -> f (g (h x y))   f . g . h       ???
\f g x -> g (f x)       flip (.)


-}

f :: Int -> Int
f = (*) 2

g :: Int -> Int
g = (+) 3

h :: Int -> Int -> Int
h = (-)

c :: (c -> d) -> (a -> b -> c) -> a -> b -> d
c f g x y = f (g x y)

makeit :: (a -> b -> c) -> a -> (b -> c)
makeit f x = f x

func :: Int -> Int -> Int
func = \x -> f . g . (h x)

test0 :: Int -> Int -> Int
test0 x y = f (g (h x y))

test2 :: Int -> Int
test2 = (f . g) . h 1

test :: (a -> a) -> (a -> a) -> a -> a
test f g x = g (f x)

test1 :: (a -> a) -> (a -> a) -> a -> a
test1 = flip (.)