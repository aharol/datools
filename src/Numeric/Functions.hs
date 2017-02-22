module Numeric.Functions
    ( mean
    , median) where

import Data.List


{-Generic mean-}
mean :: (Real a, Fractional a) => [a] -> a
mean []   = error "Average of an empty list"
mean xs = let (sum, count) = go xs
            in sum / count
    where go [] = (0, 0)
          go (h:t) = let (sum', count') = go t
                        in (sum' + h, count' + 1)

{-Generic median-}
median :: (Real a, Fractional a) => [a] -> a
median [] = error "Median of an empty list"
median xs = if oddInLength then
                middleValue
            else
                (middleValue + beforeMiddleValue) / 2
    where
        sortedList = sort xs
        oddInLength = 1 == mod (genericLength xs) 2
        middle = floor $ (genericLength xs) / 2
        middleValue = genericIndex sortedList middle
        beforeMiddleValue = genericIndex sortedList (middle - 1)
