{-# LANGUAGE TupleSections #-}

module Discern.GHC.Test where

import Control.Concurrent.STM

import qualified Data.IntMap.Strict as I

import Discern.Report

import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Tasty.QuickCheck hiding (Success, Failure)
import Test.Tasty.SmallCheck

defaultOptionSet :: OptionSet
defaultOptionSet = mconcat os
    where os = [ singleOption (QuickCheckTests 1000)
               , singleOption (SmallCheckDepth 6)
               ]

runTestTree :: TestTree -> IO [TestRep]
runTestTree t = launchTestTree defaultOptionSet t smap
    where smap sm = mapM wait (zip (testsNames defaultOptionSet t) (map snd (I.toList sm)))
                    >>= (\tvs -> return (\_ -> return (map (\(n, r) -> TestRep n r) tvs)))
          wait (n, tv) = (n,) <$>
                atomically (do s <- readTVar tv
                               case s of Done r -> return r
                                         _      -> retry)
