import Control.Concurrent
import Control.Concurrent.STM

type Semaphore = TVar Int

newSem :: Int -> STM Semaphore
newSem i = newTVar i

p :: Semaphore -> STM ()
p s = do
  i <- readTVar s
  if i <= 0 then do
    retry
  else
    writeTVar s (i-1)

v :: Semaphore -> STM ()
v s = do
  i <- readTVar s
  writeTVar s (i+1)

l :: Semaphore -> STM Int
l s = readTVar s


main = do
  s <- atomically $ newSem 5
  atomically (l s) >>= print
  atomically $ p s
  atomically $ p s
  atomically $ p s
  atomically $ p s
  atomically $ p s
  forkIO (printTest s)
  getLine
  atomically $ v s
  getLine
  atomically (v s)
  atomically (l s) >>= print

printTest s = do
  atomically $ p s
  putStrLn "Test"
  atomically $ v s