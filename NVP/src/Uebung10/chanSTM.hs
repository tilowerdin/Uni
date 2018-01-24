import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.MVar

type Chan a = TVar [a]

type ChanM a = MVar [a]

newChan :: STM (Chan a)
newChan = do
  newTVar []

newChanM :: IO (ChanM a)
newChanM = do
  newMVar []

writeChan :: Chan a -> a -> STM ()
writeChan ch v = do
  values <- readTVar ch
  writeTVar ch (values ++ [v])

writeChanM:: ChanM a -> a -> IO ()
writeChanM ch v = do
  values <- takeMVar ch
  putMVar ch (values ++ [v])

readChan :: Chan a -> STM a
readChan ch = do
  values <- readTVar ch
  if null values then do
    retry
  else do
    writeTVar ch (tail values)
    return (head values)

readChanM :: ChanM a -> IO a
readChanM ch = do
  values <- takeMVar ch
  if null values then do
    putMVar ch []
    readChanM ch
  else do
    putMVar ch (tail values)
    return (head values)

non_block_readChan :: Chan a -> STM (Maybe a)
non_block_readChan ch = do
  v <- readChan ch
  return (Just v)
 `orElse`
  return Nothing

mainM = do
  ch <- newChanM
  forkIO $ do
      readChanM ch
      putStrLn "ready"
  getLine
  writeChanM ch 42
  getLine

main = do
  ch <- atomically newChan
  forkIO $ do
      atomically $ readChan ch
      putStrLn "ready"
  getLine
  atomically $ writeChan ch 42
  getLine


