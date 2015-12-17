module NaiveBayes
( Class
, Classifier
, classMap
, train
, likelihood
, tell
, splitDataset
) where

import System.Random
import Data.List
import Data.List.Extras
import Data.Map (toList, fromListWith)

import Statistics

type Feature = Double
type Object  = [Feature]
type Index   = Int
type Label   = String
type Dataset = [(Object, Label)]

data Class = Class
    { label :: String
    , aprioriP :: Double
    , cMean :: Object
    , cDispersion :: [Double]
    }

data Classifier = Classifier
    { classes :: [Class]
    , trainedOn :: [Index]
    }

-- | Groups objects into classes
classMap :: [(Object, Label)] -> [(Label, [Object])]
classMap h = toList $ fromListWith (++) [(label, [obj]) | (obj, label) <- h]

-- | Extracts available classes, centers and a priori probabilities from the training set
train :: Double -> Dataset -> Classifier
train total set =
    Classifier { classes = map makeClass . classMap $ set
               , trainedOn = [] }
    where
        makeClass (label, objs) =
            Class { label = label
                  , aprioriP = genericLength objs / total
                  , cMean = cmean
                  , cDispersion = cdisp }
          where
            cmean = mean2d objs
            cdisp = dispersion2d cmean objs

-- | Tests a classifier against test data returning number of errors
test :: Classifier -> Dataset -> Int
test model = length . filter (\(o, l) -> label (tell model o) == l)

-- | Probability of object being a part of class
likelihood :: (Floating f) => Object -> Class -> Double
likelihood x c = ap * pxc
    where
        ap  = aprioriP c
        cms = cMean c
        cds = cDispersion c
        pxc = product $ zipWith3 normalDensity cms cds x

-- | Tells the most appropriate class for an object
tell :: Classifier -> Object -> Class
tell model obj = argmax (likelihood obj) $ classes model

-- | Best classifier
best :: (RandomGen g) => [g] -> Double -> Dataset -> Classifier
best gs share set = best
    where
        (best, _) = argmin (\(m, tst) -> test m tst) cases
        cases = [ (model trn, tst) | (trn, tst) <- splits ]
        splits = map (\g -> splitDataset g share set) gs
        model = train (genericLength set)

-- | Splits a given dataset into train and test sets
splitDataset :: (RandomGen g) => g -> Double -> Dataset -> (Dataset, Dataset)
splitDataset g share set = branch share $ zip rs set
    where
        rs = randoms g :: [Double]
        branch share [] = ([], [])
        branch share ((v, x) : vxs)
            | v < share = (x : train, test)
            | otherwise = (train, x : test)
          where
            (train, test) = branch share vxs
