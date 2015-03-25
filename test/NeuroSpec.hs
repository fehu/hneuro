module NeuroSpec(main, spec)
where

import Test.Hspec
--import Test.QuickCheck
import Control.Exception (evaluate)
import System.Random
import Data.List

import Neuro


main :: IO ()
main = hspec spec

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

dilist  = (replicate 5 $ newDelayedInput 0)
ilist   = (replicate 5 $ newInput 0)          ++ take 2 dilist
--h1list  = [newNeuron ]

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

--    describe "NetworkLayer" $ do
--        describe "InLayer" $ do
--            it ("randomlist ") $ do
--                seed <- newStdGen
--                randomlist 5 seed
--                1 `shouldBe` 1
--            it "can be build only of input and delay elements"  $ newNetworkLayer
