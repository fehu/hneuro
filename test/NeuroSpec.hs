module NeuroSpec(main, spec)
where

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

nId  = 0

any' x = True

doubleRandList n = take n (randoms seed :: [Double])

dilist = [newDelayedInput i 0 [] | i <- [0..]]
ilist  = [newInput  i 0 | i <- [0..]]
olist  = [newOutput i 0 | i <- [0..]]

hlist  = [newNeuron i
                    (doubleRandList 5)
                    "mean"
                    (\x -> sum x / (int2Double . length $ x))
         | i <- [1..]]

spec :: Spec
spec = do
    describe "Neuro.NetworkElem" $ do
        describe "Neuron" $ do
            it "is defined by sinaptic weights and transfer function" $ do
                isNeuron $ newNeuron nId [1..4] "head" head

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
                $ (newNetworkLayer . isolatedLayer $ take 5 ilist ++ take 3 dilist) `shouldSatisfy` isInLayer

        describe "HiddenLayer" $ do
            it "can de build only of neurons and delays"
                $ (newNetworkLayer . isolatedLayer $ take 8 hlist ++ take 1 dilist) `shouldSatisfy` isHiddenLayer

        describe "OutLayer" $ do
            it "can de build only of outputs and delays"
                $ (newNetworkLayer . isolatedLayer $ take 8 olist ++ take 1 dilist) `shouldSatisfy` isOutLayer
