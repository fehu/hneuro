module Neuro.RunSpec ( main, spec )
where

import Test.Hspec

main = hspec spec

spec =
    describe "Running Network" $ do
        describe "iteration step" $ do
            it "calculates network outputs for one step" $ False
