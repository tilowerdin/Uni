import Control.Concurrent

type Account = MVar Int

newAccount :: Int -> IO Account
newAccount am = do
  newMVar am

getBalance :: Account -> IO Int
getBalance acc = do
  readMVar acc

deposit :: Account -> Int -> IO ()
deposit acc am = do
  bal <- takeMVar acc
  putMVar acc (bal + am)

withdraw :: Account -> Int -> IO Bool
withdraw acc am = do
  bal <- takeMVar acc
  if bal >= am then do
    putMVar acc (bal - am)
    return True
  else do
    putMVar acc bal
    return False

transfer :: Account -> Account -> Int -> IO Bool
transfer from to am = do
  res <- withdraw from am
  if res then do
    deposit to am
    return True
  else
    return False


transfer2 :: Account -> Account -> Int -> IO Bool
transfer2 from to am = do
  balFrom <- takeMVar from
  if balFrom >= am then do
    deposit to am
    putMVar from (balFrom - am)
    return True
  else do
    putMVar from balFrom
    return False

collectedLimitedTransfer :: [Account] -> Account -> Int -> IO Bool
collectedLimitedTransfer _        _    0  = return True
collectedLimitedTransfer []       _    _  = return False
collectedLimitedTransfer (acc:accs) goal am = do
  bal <- takeMVar acc
  if am > bal then do
    trans <- collectedLimitedTransfer accs goal (am - bal)
    if trans then do
      deposit goal bal
      putMVar acc 0
      return True
    else do
      putMVar acc bal
      return False
  else do
    deposit goal am
    putMVar acc (bal - am)
    return True

main = do 
  k1 <- newAccount 100
  k2 <- newAccount 100
  forkIO (transferTest 1000 k1 k2)
  transferTest 1000 k2 k1
  getLine
  bal1 <- getBalance k1
  print bal1
  bal2 <- getBalance k2
  print bal2

transferTest 0 _ _ = do
  return ()
transferTest n k1 k2 = do
  res <- transfer2 k1 k2 1
  if res then do
    transferTest (n-1) k1 k2
  else do
    transferTest n k1 k2

