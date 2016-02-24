import qualified Data.Map.Lazy as Map
import Data.DateTime
import Control.Concurrent.STM


type InetAddress = Int

type AddressCache = TVar (Map.Map DateTime InetAddress)

offer :: AddressCache -> InetAddress -> IO Bool
offer ac inet = do
  dt <- getCurrentTime
  atomically $ offerSTM ac dt inet
  return True
  where offerSTM a k v = readTVar a >>= \m -> writeTVar a (Map.insert k v m)

contains :: AddressCache -> InetAddress -> IO Bool
contains ac v = elem v <$> readTVarIO ac

remove :: AddressCache -> InetAddress -> IO Bool
remove ac v = do 
  atomically $ removeSTM ac v
  return True
  where removeSTM a b = readTVar a >>= \m -> writeTVar a $ Map.filter (== v) m

-- peek :: AddressCache -> IO InetAddress

-- pop :: AddressCache -> IO Maybe InetAddress

size :: AddressCache -> IO Int
size ac = Map.size <$> readTVarIO ac

isEmpty :: AddressCache -> IO Bool
isEmpty ac = null <$> readTVarIO ac


main = do
  let m = Map.empty
  ac <- newTVarIO m
  s <- size ac
  print s
