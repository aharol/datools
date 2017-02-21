import Test.Hspec
import Test.QuickCheck
import Control.Exception

import Numeric.Functions

type Vec = [Double]

nonEmptyVec :: Gen Vec
nonEmptyVec = listOf1 arbitrary

main :: IO ()
main = hspec $ do
    describe "SPECIAL FUNCTIONS:" $ do

      describe "***MEAN***" $ do
        it "Mean of [1,2,6] is 3." $ do
          mean [1, 2, 6] `shouldBe` (3 :: Double)
        it "Mean of an empty list is undefined." $ do
          evaluate (mean [] :: Double) `shouldThrow` errorCall "Average of an empty list"
        it "Mean is withing the minimum X and Maximum X" $ do
          forAll nonEmptyVec $ do
            \xs -> (mean xs >= minimum xs) && (mean xs <= maximum xs)

      describe "***MEDIAN***" $ do
        it "Median of [1,2,6] is 2." $ do
          median [1,2,6] `shouldBe` (2 :: Double)
        it "Median of an empty list is undefined." $ do
          evaluate (median [] :: Double) `shouldThrow` errorCall "Median of an empty list"
        it "Median is withing the minimum X and Maximum X" $ do
          forAll nonEmptyVec $ do
            \xs -> (median xs >= minimum xs) && (median xs <= maximum xs)
