{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck.All
import Logic.Categorize
import Model
import Control.Monad
import Control.Applicative
import Test.QuickCheck
import qualified Data.Text as DT

main :: IO ()
main = $(quickCheckAll) >>= \passed -> case passed of
                True -> putStrLn "All passed."
                False -> putStrLn "Some failed."

--------------------------------------------------------------------------

prop_a xs =  \xs ->
            (over0Female $ calculateNumPerAge $ filterFemale xs) == 0
prop_b xs = \xs ->
            (over0Female $ calculateNumPerAge $ filterMale xs) == 0

prop_c x = \x ->
            (size x) == (numOfPerson $ fromAllNumPerAge x)
    
prop_d xs = \xs ->
            (numOfPerson $ fromAllNumPerAge $ calculateNumPerAge xs)
                == (length xs)

--------------------------------------------------------------------------

instance Arbitrary Person where
    arbitrary = return Person 
        `ap` (DT.pack <$> arbitrary)
        `ap` arbitrary
        `ap` arbitrary

instance Arbitrary Sex where
    arbitrary = elements [Male]

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

filterMale :: [Person] -> [Person]
filterMale xs = [x | x <- xs, (personSex x) == Female]

filterFemale :: [Person] -> [Person]
filterFemale xs = [x | x <- xs, (personSex x) == Male]
