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

collectedTransfer :: [Account] -> Account -> Int -> IO Bool
collectedTransfer = error "to be defined"

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

