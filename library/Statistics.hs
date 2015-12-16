module Statistics
( mean
, dispersion
) where

import Data.List

-- | Statistical mean of a list of objects
mean :: Floating f => [f] -> f
mean xs = sum xs / genericLength xs

-- | Statistical dispersion of a list of objects
dispersion :: Floating f => f -> [f] -> f
dispersion mean objects = 1 / (n - 1) * s
    where
        n = genericLength objects
        s = sum $ map ((mean -) . (** 2)) objects

-- | Prabability density of the normal distribution
normalDensity :: (Floating f) => f -> f -> f -> f
normalDensity mean disp x = exp power / denom
    where
        power = - (x - mean) ^ 2 / (2 * disp)
        denom = sqrt (2 * pi * disp)
