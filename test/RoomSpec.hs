module RoomSpec where

import Test.Hspec ( Spec, describe, it, shouldBe, shouldSatisfy, shouldNotReturn )

import Room
import RoomLoader
import RoomSolver
import System.Timeout (timeout)

spec :: Spec
spec = do
    describe "Burglar " $ do
        it "gets near zero possibility of detection for empty room" $ do
            room <- getRoom "testData/bank0.map"
            toleranceApproximation room 3 `shouldSatisfy` (<0.001)
        it "should solve example in resonable time" $ do
            timeout 5000000 (do
                room <- getRoom "testData/bank13.map"
                return (toleranceApproximation room 3)) `shouldNotReturn` Nothing
        it "should parse only given numbers of records" $ do
            room <- getRoom "testData/bank2.map"
            length (detectors room) `shouldBe` 2