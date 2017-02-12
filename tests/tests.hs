--import Data.Monoid
import Test.Framework (defaultMain)
import Tests.SpecFunctions
-- import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2
-- import Test.HUnit
-- import Test.QuickCheck
-- import Utils

main :: IO ()
main = defaultMain [
    Tests.SpecFunctions.tests
    ]
    --    [ testCase "rev" testRev
    --    , testCase "mean" testMean
    --    , testProperty "listRevRevId" propListRevRevId
    --    ] mempty

-- testRev :: Assertion
-- testRev = reverse [1, 2, 3] @?= [3, 2, 1]
--
-- propListRevRevId :: [Int] -> Property
-- propListRevRevId xs = not (null xs) ==> reverse (reverse xs) == xs
--
-- testMean :: Assertion
-- testMean = mean [1.0, 2.0, 3.0] @?= 2.0
