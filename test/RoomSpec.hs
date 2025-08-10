module RoomSpec where

import Test.Hspec ( Spec, describe, it, shouldSatisfy, shouldNotReturn )

import RoomLoader
import RoomSolver
import System.Timeout (timeout)

spec :: Spec
spec = do
    describe "Burglar " $ do
        it "gets near zero tolerance for empty room" $ do
            room <- getRoom "testData/bank0.map"
            toleranceApproximation room 3 `shouldSatisfy` (<0.001)
        it "should solve example in resonable time" $ do
            timeout 5000000 (do
                room <- getRoom "testData/bank13.map"
                return (toleranceApproximation room 3)) `shouldNotReturn` Nothing