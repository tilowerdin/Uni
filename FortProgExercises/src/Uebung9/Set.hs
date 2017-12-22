{-# LANGUAGE TemplateHaskell #-}


module Set where

import Test.QuickCheck

data Set a = Set [a] deriving Show

empty :: Set a
empty = Set []

isEmpty :: Set a -> Bool
isEmpty (Set xs) = null xs

prop_isEmpty :: Set a -> Bool
prop_isEmpty s@(Set []) = isEmpty s == True
prop_isEmpty s          = isEmpty s == False

insert :: a -> Set a -> Set a
insert x (Set xs) = Set (x:xs)

prop_insert :: Eq a => a -> Set a -> Bool
prop_insert a s@(Set xs)
  | elem a xs = size (insert a s) == size s
  | otherwise = size (insert a s) >  size s

member :: Eq a => a -> Set a -> Bool
member x (Set xs) = elem x xs

prop_member :: Eq a => a -> Set a -> Bool
prop_member a s@(Set xs) = member a s == elem a xs

delete :: Eq a => a -> Set a -> Set a
delete x (Set xs) = Set (remove x xs)
  where
  remove _ []                 = []
  remove y (z:zs) | y == z    = zs
                  | otherwise = remove y zs

prop_memberAfterDelete :: Eq a => a -> Set a -> Bool
prop_memberAfterDelete a s = member a (delete a s) == False

union :: Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (xs ++ ys)

prop_union :: Eq a => a -> Set a -> Set a -> Bool
prop_union a s1 s2
  | member a s1 || member a s2 = member a (union s1 s2)
  | otherwise                  = not $ member a (union s1 s2)

prop_unionNotMember :: Eq a => a -> Set a -> Set a -> Bool
prop_unionNotMember a s1 s2 = not $ member a $ delete a $ union s1 s2

intersect :: Ord a => Set a -> Set a -> Set a
intersect (Set s1) (Set s2) = Set (merge s1 s2)
  where
  merge []     ys     = ys
  merge xs     []     = xs
  merge (x:xs) (y:ys) = case compare x y of
    LT -> x : y : merge xs ys
    EQ -> y : merge xs ys
    GT -> y : x : merge xs ys

prop_intersection :: Ord a => a -> Set a -> Set a -> Bool
prop_intersection a s1 s2 
  | member a s1 && member a s2 = member a $ intersect s1 s2
  | otherwise                  = not $ member a $ intersect s1 s2

size :: Set a -> Int
size (Set xs) = length xs

prop_size :: Set a -> Bool
prop_size s@(Set xs) = length xs == size s

instance (Arbitrary a, Eq a) => Arbitrary (Set a) where
    arbitrary = do
      n <- choose (0,50)
      g n empty
      where
        g :: (Arbitrary a, Eq a) => Int -> Set a -> Gen (Set a)
        g 0 s = return s
        g n s = do
          a <- arbitrary
          g (n-1) (inserting a s)
          where 
            inserting :: Eq a => a -> Set a -> Set a
            inserting a (Set xs) 
              | elem a xs = Set xs
              | otherwise = Set (a:xs)

return []
runTests = $quickCheckAll