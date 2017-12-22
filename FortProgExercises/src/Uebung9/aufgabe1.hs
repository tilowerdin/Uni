{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

data SearchTree = Empty | Branch SearchTree Int SearchTree
  deriving (Eq, Show)

insert :: Int -> SearchTree -> SearchTree
insert x Empty          = Branch Empty x Empty
insert x (Branch l n r)
  | x == n    = Branch l n r
  | x <  n    = Branch (insert x l) n r
  | otherwise = Branch l n (insert x r)

return []
runTests = $quickCheckAll


prop_NotEmpty :: Int -> Bool
prop_NotEmpty x = insert x Empty /= Empty

prop_contains :: SearchTree -> Int -> Bool
prop_contains t i = contains (insert i t) i
  where
    contains :: SearchTree -> Int -> Bool
    contains Empty            _ = False
    contains (Branch t1 x t2) i = x == i || contains t1 i || contains t2 i

prop_flatten :: SearchTree -> Bool
prop_flatten t = sorted (flatten t)
  where 
    sorted :: [Int] -> Bool
    sorted []       = True
    sorted [x]      = True
    sorted (x:y:xs) = x < y && sorted (y:xs)

    flatten :: SearchTree -> [Int]
    flatten Empty            = []
    flatten (Branch t1 x t2) = flatten t1 ++ [x] ++ flatten t2

rand :: Gen Float
rand = arbitrary :: Gen Float

instance Arbitrary SearchTree where
    arbitrary = do
        n <- choose (0,20)
        g n (0,50)
        where
            g :: Int -> (Int, Int) -> Gen SearchTree
            g 0 _          = return Empty
            g n (from, to) 
                | from == to = return (Branch Empty from Empty)
                | otherwise  = do
                    x <- choose (from, to)
                    case x == from of
                        True -> do
                            r <- g (n-1) (from+1, to)
                            return (Branch Empty x r)
                        otherwise -> case x == to of 
                            True -> do
                                l <- g (n-1) (from, to-1)
                                return (Branch l x Empty)
                            otherwise -> do
                                l <- g (n-1) (from, x-1)
                                r <- g (n-1) (x+1, to)
                                return (Branch l x r)

                    


data Tree = Leaf Int | Node Tree Tree deriving (Eq, Show)

instance Arbitrary Tree where
    arbitrary = do
        n <- choose (0,50)
        g n
        where 
            g :: Int -> Gen Tree
            g 0 = do
                i <- choose (0,20)
                return (Leaf i)
            g n = do
                x <- choose (0,n)
                l <- g (max (x-1) 0)
                r <- g (max (n-x-1) 0)
                return (Node l r)

size :: Tree -> Int
size (Leaf a)     = 1
size (Node t1 t2) = size t1 + size t2

toList :: Tree -> [Int]
toList (Leaf a)     = [a]
toList (Node t1 t2) = toList t1 ++ toList t2

prop_Size :: Tree -> Bool
prop_Size t = (size t) == (length . toList) t