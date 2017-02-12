module Tests.SpecFunctions (
    tests
    ) where


import Test.QuickCheck  hiding (choose,within)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Numeric.SpecFunctions


tests :: Test
tests = testGroup "Special functions"
    []
