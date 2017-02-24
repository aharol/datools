import Data.List (genericLength)
import Test.Hspec
import Test.QuickCheck
import Control.Exception

import Numeric.Functions

type Vector = [Double]
eps = 1e-10

nonEmptyVec :: Gen Vector
nonEmptyVec = listOf1 arbitrary

vecOfAtLeastTwo :: Gen Vector
vecOfAtLeastTwo = do
    x <- nonEmptyVec
    y <- nonEmptyVec
    return (x++y)

main :: IO ()
main = hspec $ do
    describe "SPECIAL FUNCTIONS:" $ do

        describe "\n***MEAN***\n" $ do
            it "Mean of [1,2,6] is 3." $ do
                mean [1, 2, 6] `shouldBe` (3 :: Double)
            it "Mean of an empty list is undefined." $ do
                evaluate (mean [] :: Double) `shouldThrow` errorCall "Average of an empty list"
            it "Mean of a vector is between the minimum and the maximum" $ do
                forAll nonEmptyVec $ do
                    \xs -> (mean xs >= minimum xs) && (mean xs <= maximum xs)
            it "The sum of the deviations around mean is 0.0" $ do
                forAll nonEmptyVec $ do
                    \xs -> sum (map (\x -> x - mean xs) xs) < eps
            it "The product of the arithmetic mean and the number of items equals the sum of items" $ do
                forAll nonEmptyVec $ do
                    \xs -> (sum xs - genericLength xs * mean xs) < eps

        describe "\n***MEDIAN***\n" $ do
            it "Median of [1,2,6] is 2." $ do
                median [1,2,6] `shouldBe` (2 :: Double)
            it "Median of an empty list is undefined." $ do
                evaluate (median [] :: Double) `shouldThrow` errorCall "Median of an empty list"
            it "Median of a vector is between the minimum and the maximum" $ do
                forAll nonEmptyVec $ do
                    \xs -> (median xs >= minimum xs) && (median xs <= maximum xs)

        describe "\n***STANDARD DEVIATION AND ERROR***\n" $ do
            it "Standard deviation of [1,3,5] is 2." $ do
                std [1,3,5] `shouldBe` (2 :: Double)

            it "Standard deviation is non-negative" $ do
                forAll vecOfAtLeastTwo $ do
                    \xs -> std xs >= (0 :: Double)
