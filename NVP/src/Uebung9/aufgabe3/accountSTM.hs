import Control.Concurrent
import Control.Concurrent.STM

type Account = TVar Int

newAccount :: Int -> STM Account
newAccount am = do
  newTVar am

getBalance :: Account -> STM Int
getBalance acc = do
  readTVar acc

deposit :: Account -> Int -> STM ()
deposit acc am = do
  bal <- readTVar acc
  writeTVar acc (bal + am)

withdraw :: Account -> Int -> STM Bool
withdraw acc am = do
  bal <- readTVar acc
  if bal >= am then do
    writeTVar acc (bal - am)
    return True
  else do
    return False

withdraw2 :: Account -> Int -> STM Bool
withdraw2 acc am = do
  bal <- getBalance acc
  if bal >= am then do
    deposit acc (-am)
    return True
  else
    return False

transfer :: Account -> Account -> Int -> STM Bool
transfer from to am = do
  bal <- getBalance from
  if bal >= am then do
    withdraw from am
    deposit to am
    return True
  else
    return False

main = do 
  k1 <- atomically (newAccount 100)
  k2 <- atomically (newAccount 100)
  forkIO (transferTest 1000 k1 k2)
  transferTest 1000 k2 k1
  getLine
  bal1 <- atomically $ getBalance k1
  print bal1
  bal2 <- atomically $ getBalance k2
  print bal2

transferTest 0 _ _ = do
  return ()
transferTest n k1 k2 = do
  res <- atomically $ transfer k1 k2 1
  if res then do
    transferTest (n-1) k1 k2
  else do
    transferTest n k1 k2

collectedLimitedTransfer :: [Account] -> Account -> Int -> STM Bool
collectedLimitedTransfer _        _    0  = return True
collectedLimitedTransfer []       _    _  = return False
collectedLimitedTransfer (acc:accs) goal am = do
  bal <- getBalance acc
  if am > bal then do
    trans <- collectedLimitedTransfer accs goal (am - bal)
    if trans then do
      transfer acc goal bal
      return True
    else do
      return False
  else do
    transfer acc goal am
    return True


collectedTransferTest = do
  a <- atomically $ newAccount 50
  b <- atomically $ newAccount 100
  c <- atomically $ newAccount 35
  d <- atomically $ newAccount 0
  forkIO (go 1000 [a,b,c,d] d 100)
  go 1000 [c,b,a,d] d 100
  atomically (getBalance a) >>= print
  atomically (getBalance b) >>= print
  atomically (getBalance c) >>= print
  atomically (getBalance d) >>= print
  where
    go 0 _ _ _ = return ()
    go n l g a = do
      atomically $ collectedLimitedTransfer l g a
      go (n-1) l g a


