module Numeric.SpecFunctions
    ( mean
    , median) where

import Data.List


{-Generic mean-}
mean :: (Fractional a) => [a] -> a
mean []   = error "Cannot take average of empty list"
mean nums = let (sum, count) = go nums
            in sum / count
    where go [] = (0,0)
          go (x:xs) = let (sum',count') =  go xs
                       in (sum' + x, count' + 1)

{-Generic median-}
median :: (Real a, Fractional a) => [a] -> a
median [] = 0
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
