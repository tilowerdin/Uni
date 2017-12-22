{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

data Queue a = Queue [a] [a] deriving Show

-- smart constructor
queue :: [a] -> [a] -> Queue a
queue [] ys = Queue (reverse ys) ys
queue xs ys = Queue xs ys

prop_queue :: [a] -> [a] -> Bool
prop_queue l1 l2 = invariant (queue l1 l2)

-- empty queue
emptyQueue :: Queue a
emptyQueue = queue [] []

prop_emptyQueue :: Bool
prop_emptyQueue = invariant emptyQueue

-- Is a queue empty?
isEmptyQueue :: Queue a -> Bool
isEmptyQueue (Queue _ ys) = null ys

prop_isEmptyQueue :: Queue a -> Bool
prop_isEmptyQueue queue@(Queue l1 l2)
    | (not . invariant) queue = True
    | isEmptyQueue queue      = null l1 && null l2
    | otherwise               = (not . null) l1 || (not . null) l2

-- add to a queue
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs ys) = queue xs (x:ys)

prop_enqueue :: a -> Queue a -> Bool
prop_enqueue a q = (not . invariant) q || 
    let newQueue = enqueue a q in invariant newQueue && size newQueue > size q

-- get next element
next :: Queue a -> a
next (Queue (x:_) _) = x
next _               = error "Queue.next: empty queue"

prop_next :: Eq a => a -> Queue a -> Bool
prop_next a q = (not . invariant) q || inQueue a (enqueue a q)
    where
        inQueue :: Eq a => a -> Queue a -> Bool
        inQueue a q = a == next q || inQueue a (dequeue q)

-- remove first element
dequeue :: Queue a -> Queue a
dequeue (Queue (_:xs) ys) = queue ys xs
dequeue _                 = error "Queue.dequeue: empty queue"

prop_dequeue :: Queue a -> Bool
prop_dequeue q = (not . invariant) q || size q == 0 ||
    let newQueue = dequeue q in invariant newQueue && size newQueue < size q

-- size of a queue
size :: Queue a -> Int
size (Queue xs ys) = length xs + length ys

prop_incSize :: a -> Queue a -> Bool
prop_incSize a q = (not . invariant) q || 
    let newQueue = enqueue a q in invariant newQueue && size newQueue > size q

prop_decSize :: Queue a -> Bool
prop_decSize q = (not . invariant) q || size q == 0 ||
    let newQueue = dequeue q in invariant newQueue && size newQueue < size q

-- invariant a queue should fulfill
invariant :: Queue a -> Bool
invariant (Queue xs ys) = not (null xs) || null ys

instance Arbitrary a => Arbitrary (Queue a) where
    arbitrary = do
        l1 <- arbitrary
        l2 <- arbitrary
        return $ queue l1 l2

return []
runTests = $quickCheckAll