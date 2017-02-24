module Numeric.Functions
    ( mean
    , median
    , std
    ) where

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
median :: (Fractional a, Real a) => [a] -> a
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

{-Standard deviation-}
std :: (Floating a, Real a) => [a] -> a
std [_] = error "Standard deviation of a population containing a single number is undefined"
std xs = sqrt (sum [(x - mu)^2 | x <- xs]) / z
    where
        mu = mean xs
        z = sqrt $ fromIntegral (genericLength xs - 1)
