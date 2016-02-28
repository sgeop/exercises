import qualified Data.Bimap as B
import Data.DateTime
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad


type InetAddress = Integer

type AddressCache = TVar (B.Bimap DateTime InetAddress)


offer :: AddressCache -> InetAddress -> IO Bool
offer ac inet = do
  dt <- getCurrentTime
  atomically $ offerSTM ac dt inet
  return True
  where offerSTM a k v = readTVar a >>= \m -> writeTVar a (B.insert k v m)


contains :: AddressCache -> InetAddress -> IO Bool
contains ac v = B.memberR v <$> readTVarIO ac


remove :: AddressCache -> InetAddress -> IO Bool
remove ac v = do 
  atomically $ removeSTM ac v
  return True
  where removeSTM a b = readTVar a >>= \m -> writeTVar a $ B.deleteR v m

peek :: AddressCache -> IO (Maybe InetAddress)
peek ac = do
  e <- isEmpty ac 
  if e 
    then return Nothing
    else do m <- readTVarIO ac
            let (dt, i) = B.findMin m
            return $ Just i


remove' :: AddressCache -> IO (Maybe InetAddress)
remove' = atomically . removeSTM'

removeSTM' :: AddressCache -> STM (Maybe InetAddress)
removeSTM' ac = do
  m <- readTVar ac
  if B.null m
    then return Nothing
    else do
      let ((a, i), m') = B.deleteFindMin m
      writeTVar ac m'
      return $ Just i


take :: AddressCache -> IO InetAddress
take = atomically . takeSTM

takeSTM :: AddressCache -> STM InetAddress
takeSTM ac = do
  m <- readTVar ac
  if B.null m
    then retry
    else do
      let ((a, i), m') = B.deleteFindMin m
      writeTVar ac m'
      return i


size :: AddressCache -> IO Int
size ac = B.size <$> readTVarIO ac


isEmpty :: AddressCache -> IO Bool
isEmpty ac = B.null <$> readTVarIO ac


removeBefore :: AddressCache -> DateTime -> Integer -> STM ()
removeBefore ac dt expSecs = do
  m <- readTVar ac
  writeTVar ac (B.filter isExp m)
  where isExp t _ = toSeconds dt - toSeconds t < expSecs

cleanup :: AddressCache -> Integer -> IO ()
cleanup ac expSecs = 
  forever $ do
  threadDelay (10^6 * 5)
  dt <- getCurrentTime
  atomically $ removeBefore ac dt expSecs
  

new :: Integer -> IO AddressCache
new expSecs = do
   ac <- newTVarIO B.empty
   forkIO $ cleanup ac expSecs
   return ac

main = do
  ac <- new 10
  forkIO $ forever $ do
    i <- getLine
    offer ac (read i :: Integer)
    return ()
  forever $ do
    threadDelay (10^6 * 2)
    a <- readTVarIO ac
    print a
