import Control.Concurrent
import Control.Concurrent.STM

type Buffer a = TVar [a]

newBuffer :: [a] -> STM (Buffer a)
newBuffer v = newTVar v

writeB :: Buffer a -> a -> STM ()
writeB b v = do
  c <- readTVar b
  if length c >= 2 then do
    retry
  else do
    writeTVar b (c ++ [v])

readB :: Buffer a -> STM a
readB b = do
  c <- readTVar b
  if length c > 0 then do
    writeTVar b (tail c)
    return (head c)
  else
    retry


main = do
  b <- atomically $ newBuffer []
  forkIO (readBuffer b)
  getLine
  atomically $ writeB b 5
  forkIO (writeThree b)
  getLine
  atomically (readB b) >>= print
  atomically (readB b) >>= print
  atomically (readB b) >>= print

readBuffer b = do
  a <- atomically $ readB b
  print a

writeThree b = do
  atomically $ writeB b 1
  atomically $ writeB b 2
  atomically $ writeB b 3
  putStrLn "r"
  