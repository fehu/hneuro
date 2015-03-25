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

doubleRandList n = take n (randoms seed :: [Double])

dilist  = (repeat $ newDelayedInput 0)
--dslist  = (repeat $ newDelaySink 0, ) TODO
ilist   = (repeat $ newInput 0)
olist   = (repeat $ newOutput 0)
hlist   = [newNeuron (doubleRandList 5)
                     "mean"
                     (\x -> sum x / (int2Double . length $ x))        | _ <- [1..]]
--                     (\x -> sum x / (int2Double $ length x))        | _ <- [1..]]

--takeAfter a t xs = take t $ drop a xs

spec :: Spec
spec = do
    describe "Neuro.NetworkElem" $ do
        describe "Neuron" $ do
            it "is defined by sinaptic weights and transfer function" $ do
                isNeuron $ newNeuron [1..4] "head" head

        describe "Input" $ do
            it "contains input value for current iteration"     $ isInput $ newInput 4.0

        describe "Output" $ do
            it "contains output value for current iteration"    $ isOutput $ newOutput 2.4e18

        describe "DelayedInput" $ do
            it "introduces values, delayed by `DelaySink`"      $ isDelayedInput $ newDelayedInput 0

        describe "DelaySink" $ do
            it "guards a value to be delayed"
                $ isDelaySink $ newDelaySink 0.1 1 $ newDelayedInput 0
            it "is linked to a DelayedInput" $ do
                evaluate (newDelaySink 0.1 1 $ newInput 0) `shouldThrow` anyException

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
