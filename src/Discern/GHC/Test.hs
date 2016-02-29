module Discern.GHC.Test where

import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Tasty.QuickCheck
import Test.Tasty.SmallCheck

defaultOptionSet :: OptionSet
defaultOptionSet = mconcat (map singleOption os)
    where os = [ QuickCheckTests 1000
               , SmallCheckDepth 10
               ]

runTestTree :: TestTree -> IO [Result]
runTestTree = undefined
