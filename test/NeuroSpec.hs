module NeuroSpec(main, spec)
where

import NamedFunc
import Test.Hspec
--import Test.QuickCheck
import Control.Exception (evaluate)
import System.Random
import Data.List
import GHC.Float(int2Double)

import Neuro


main :: IO ()
main = hspec spec

seed = mkStdGen 14

nId  = (0, 1)

any' x = True

doubleRandList n = take n (randoms seed :: [Double])

-- TODO
--dilist = [newDelayedInput (0, i) 0 [] | i <- [0..]]
ilist  = [newInput  (0, i) 0 | i <- [0..]]
olist  = [newOutput (0, i) 0 | i <- [0..]]

mean :: [Double] -> Double
mean x = sum x / (int2Double . length $ x)

hlist j = [newNeuron (j, i)
                     (doubleRandList 5)
                     (named mean "mean")
          | i <- [1..]]

spec :: Spec
spec = do
    describe "Neuro.NetworkElem" $ do
        describe "Neuron" $ do
            it "is defined by sinaptic weights and transfer function" $ do
                isNeuron $ newNeuron nId [1..4] (head `named` "head")

        describe "Input" $ do
            it "contains input value for current iteration"  $ isInput $ newInput nId 4.0

        describe "Output" $ do
            it "contains output value for current iteration" $ isOutput $ newOutput nId 2.4e18

        describe "DelayedInput" $ do
            it "introduces values, delayed by `DelaySink`"   $ isDelayedInput $ newDelayedInput nId 2 []

        describe "DelaySink" $ do
            it "guards a value to be delayed"
                $ isDelaySink $ newDelaySink nId 0.1 $ [newDelayedInput nId 2 []]
            it "is linked to a DelayedInput" $ do
                evaluate (newDelaySink nId 0.1 $ [newInput nId 0]) `shouldThrow` anyException

    describe "NetworkLayer" $ do
        describe "InLayer" $ do
            it "can be build only of input and delay elements"
                $ (newNetworkLayer . newLayer $ take 5 ilist) `shouldSatisfy` isInLayer    -- TODO ++ take 3 dilist

        describe "HiddenLayer" $ do
            it "can de build only of neurons and delays"
                $ (newNetworkLayer . newLayer $ take 8 $ hlist 1) `shouldSatisfy` isHiddenLayer    -- TODO ++ take 1 dilist

        describe "OutLayer" $ do
            it "can de build only of outputs and delays"
                $ (newNetworkLayer . newLayer $ take 8 olist) `shouldSatisfy` isOutLayer   -- TODO ++ take 1 dilist
