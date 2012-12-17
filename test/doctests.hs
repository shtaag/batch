module Main where

import Test.DocTest

main :: IO ()
main = doctest ["Logic/Categorize.hs", "-package-conf=./cabal-dev/packages-7.4.1.conf"]
