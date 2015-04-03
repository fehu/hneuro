module Neuro.RunSpec ( main, spec )
where

import Test.Hspec

import TestsCommon

import Neuro
import Neuro.Run

import Data.Map ( empty, fromList, elems )

main = hspec spec

spec =
    describe "Running Network" $ do
        describe "iteration step" $ do
            it "calculates network outputs for one step" $
                runSteoNNet1 (fromList $ head nnet1Inps) Nothing `shouldSatisfy` (not . null . elems . snd)



runSteoNNet1 inp nF = nnetIter nnetSynsCache1 inp (map layerElems $ layers nnet1) empty nF
