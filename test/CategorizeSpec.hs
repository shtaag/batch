{-# LANGUAGE FlexibleInstances #-}

module CategorizeSpec
    ( spec
    ) where

import Test.Hspec
import Control.Monad
import Control.Applicative
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)
import Model
import Logic.Categorize
import qualified Data.Text as DT

--------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "calculateNumPerAge" $ do
        prop "input withMales" $ prop_calcNumPerAge_allMales
        prop "input withFemales" $ prop_calcNumPerAge_allFemales

    describe "fromAllNumPerAge" $ do
        prop "whole size should be the same" $ \x ->
            (size x) == (numOfPerson $ fromAllNumPerAge x)
    
    describe "joint test" $ do
        prop "whole size of person should be the same" $ \xs ->
            (numOfPerson $ fromAllNumPerAge $ calculateNumPerAge xs)
                == (length xs)

--------------------------------------------------------------------------

prop_calcNumPerAge_allMales :: [Person] -> Property
prop_calcNumPerAge_allMales xs =
    forAll genMales $ \xs ->
    collect (length xs) $
        (sumFemales $ calculateNumPerAge xs) == 0
  where
    sumFemales :: AllNumPerAge -> Int
    sumFemales x = (over0Female x)
                + (over20Female x)
                + (over40Female x)
                + (over60Female x)

prop_calcNumPerAge_allFemales :: [Person] -> Property
prop_calcNumPerAge_allFemales xs =
    forAll genFemales $ \xs ->
    collect (length xs) $
        (sumMales $ calculateNumPerAge xs) == 0
  where
    sumMales :: AllNumPerAge -> Int
    sumMales x = (over0Male x)
              + (over20Male x)
              + (over40Male x)
              + (over60Male x)

instance Arbitrary Person where
    arbitrary = return Person 
        `ap` (DT.pack <$> arbitrary)
        `ap` arbitrary
        `ap` arbitrary

newtype Males = Males {getMale :: Sex}

instance Arbitrary Males where
    arbitrary = elements [Males Male]

instance Arbitrary Sex where
    arbitrary = elements [Male, Female]

instance Arbitrary AllNumPerAge where
    arbitrary = return AllNumPerAge
            `ap` arbitrary
            `ap` arbitrary
            `ap` arbitrary
            `ap` arbitrary
            `ap` arbitrary
            `ap` arbitrary
            `ap` arbitrary
            `ap` arbitrary

size :: AllNumPerAge -> Int
size x = (over0Male x)
    + (over0Female x)
    + (over20Male x)
    + (over20Female x)
    + (over40Male x)
    + (over40Female x)
    + (over60Male x)
    + (over60Female x)

numOfPerson :: [NumPerAge] -> Int
numOfPerson = foldr sum' 0
  where
    sum' :: NumPerAge -> Int -> Int
    sum' (NumPerAge _ _ x) y  = x + y

genMales :: Gen [Person]
genMales = (filter (\x -> personSex x == Male)) <$> arbitrary

genFemales :: Gen [Person]
genFemales = (filter (\x -> personSex x == Female)) <$> arbitrary
