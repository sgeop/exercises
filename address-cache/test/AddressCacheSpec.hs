module AddressCacheSpec where

import Test.Hspec
import AddressCache
import qualified AddressCache as AC
import qualified Data.Bimap as B
import Control.Concurrent.STM
import Control.Concurrent

main :: IO ()
main = hspec spec

getAC :: IO AddressCache
getAC = newTVarIO B.empty

spec :: Spec
spec = do
  
  describe "isEmpty" $ do
    it "returns True for an empty AddressCache" $ do
      ac <- getAC
      isEmpty ac `shouldReturn` True

    it "returns False for a non Empty AddressCache" $ do
      ac <- getAC
      a <- offer ac 123
      isEmpty ac `shouldReturn` False 


  describe "size" $
    it "returns number of elements in the cache" $ do
      ac <- getAC
      size ac `shouldReturn` 0

      offer ac 6
      size ac `shouldReturn` 1

      offer ac 3
      size ac `shouldReturn` 2


  describe "offer" $
    it "adds an element to the cache" $ do
      ac <- getAC
      offer ac 5
      size ac `shouldReturn` 1


  describe "contains" $
    it "returns True if element is in the cache" $ do
      ac <- getAC
      offer ac 5
      offer ac 4
      contains ac 5 `shouldReturn` True

  describe "peek" $ do
    it "returns newest element in the cache if it's not empty" $ do
      ac <- getAC
      offer ac 6
      offer ac 2
      peek ac `shouldReturn` Just 2
      size ac `shouldReturn` 2

    it "returns Nothing if the cache is empty" $ do
      ac <- getAC
      peek ac `shouldReturn` Nothing

  describe "remove'" $ do
    it "returns and deletes newest element in the cache if it's not empty" $ do
      ac <- getAC
      offer ac 1
      offer ac 2
      remove' ac `shouldReturn` Just 2
      size ac `shouldReturn` 1

    it "returns Nothing if the cache is empty" $ do
      ac <- getAC
      remove' ac `shouldReturn` Nothing

  describe "take" $ do 
    it "returns and deletes newest element in the cache if it's not empty" $ do
      ac <- getAC
      offer ac 123
      offer ac 456
      AC.take ac `shouldReturn` 456
      size ac `shouldReturn` 1

    it "blocks until an element appears when empty" $ do
      ac <- getAC
      forkIO $ do
        threadDelay (10^6)
        offer ac 6
        return ()
      isEmpty ac `shouldReturn` True
      AC.take ac `shouldReturn` 6

  describe "new" $
    it "creates new AddressCache that deletes expired values" $ do
      ac <- new 1 -- argument to new is number of seconds before expiration
      offer ac 1
      isEmpty ac `shouldReturn` False
      -- cache checks for expired values every 5 secs, so we need to sleep
      -- for 6 seconds here. Update to make checking interval configurable for
      -- tests.
      threadDelay (10^6 * 6) 
      isEmpty ac `shouldReturn` True
