module Neuro.CreationSpec ( main, spec )
where

import Test.Hspec
import Test.Hspec.QuickCheck
--import Test.QuickCheck
--import Test.Hspec.Monadic
--import Test.Hspec.QuickCheck
import Control.Exception (evaluate)
import Control.Applicative

import Data.Map (fromList)
--import Functor


import NamedFunc

import Neuro
import Neuro.DSL as DSL
import Neuro.Create

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Element Creation using DSL" $ do

        it "creates neurons"        $ ( createElem cnf (anId, DSL.Neuron) )    `shouldSatisfy` isNeuron

        it "creates inputs"         $ ( createElem cnf (anId, DSL.In) )        `shouldSatisfy` isInput

        it "creates outputs"        $ ( createElem cnf (anId, DSL.Out) )       `shouldSatisfy` isOutput

        it "creates delays"         $ ( createElem cnf (anId, DSL.Delay) )     `shouldSatisfy` isDelaySink

        it "creates delayed inputs" $ ( createElem cnf (anId, DSL.Delayed 1) ) `shouldSatisfy` isDelayedInput

    describe "Layers Creation using DSL" $ do

        prop "creates input layers of inputs and delayed inputs" $ do
            let f a b = mkLayer $ (genElems DSL.In          0 0 a) ++
                                  (genElems (DSL.Delayed 1) 0 a b)
            \(a, b) -> testLayer (a>0) (b>0) isInpLayer (f a b)
--                       else evaluate ( f a b ) `shouldThrow` errorCall "empty new layer"
--                                                             (||) <$> errorCall "empty new layer"
--                                                                  <*> errorCall "empty new layer"

        prop "creates Hidden layers of neurons, delays and delayed inputs" $ do
            let f a b c = mkLayer $ (genElems Neuron      1 0       a) ++
                                    (genElems Delay       1 a       b) ++
                                    (genElems (Delayed 1) 1 (a+b)   c)
            \(a, b, c) -> testLayer (any (>0) [a, b, c]) False isHiddenLayer $ f a b c

        prop "creates Hidden layers of neurons, delays and delayed inputs" $ do
            let f a b = mkLayer $ (genElems DSL.Out     0 0 a) ++
                                  (genElems (DSL.Delay) 0 a b)
            \(a, b) -> testLayer (a>0) (b>0) isOutLayer (f a b)

        prop "won't create if has any layer mixed of input, neuron and output" $ do
            let f a b c = mkLayer $ (genElems Neuron      1 0         a) ++
                                    (genElems In          1 a         b) ++
                                    (genElems Out         1 (a+b)     c)

            let testOk a b c = any (\(x:xs) -> x && all not xs) $ map (map (>0)) $ orderComb [a, b, c]

            let test a b c = if testOk a b c then ff `shouldSatisfy`
                                                  (\x -> any ($x) [isInpLayer, isHiddenLayer, isOutLayer])
                             else if all (<=0) [a, b ,c] then evaluate ff `shouldThrow`
                                                                errorCall "empty new layer"
                             else                             evaluate ff `shouldThrow`
                                                                errorCall "incompatible layer"
                           where ff = f a b c
            let absTest a b c = test (abs a) (abs b) (abs c)
            absTest


--testLayer c1 c2 tst f =     if      c1 > 0 then f `shouldSatisfy` tst    else if c2 > 0 then f `shouldSatisfy` isHiddenLayer    else                evaluate f `shouldThrow` errorCall "empty new layer"

orderComb :: [a] -> [[a]]
orderComb xs = orderComb_f xs xs

orderComb_f _ [] = []
orderComb_f (x:xs) (_:c) = (x:xs) : orderComb_f (xs++[x]) c


testLayer c1 c2 tst f =
    if      c1 then f `shouldSatisfy` tst
    else if c2 then f `shouldSatisfy` isHiddenLayer
    else            evaluate f `shouldThrow` errorCall "empty new layer"

anId = (1, 1)

cnf = CConf { zero = 0
            , weights = \_ -> [1, 2]
            , transfer = \_ -> (head `named` "head")
            }

-- cnf = CConf { zero = 0, weights = \_ -> [1, 2], transfer = \_ -> (head `named` "head") }

genElems el l b n = [ ((l, i), el) | i <- [(b+1)..(b+n)] ]

mkLayer l = createLayer cnf $ fromList l

--positive :: Gen Int
--positive = suchThat arbitrary (> 0)


--createInputLayer = genElems In

--genElems l n dsl = [ createElem cnf ((l, i), dsl) | i <- [1..] ]

--createInputLayer l = createLayer cnf $ fromList l --[ ((0, i), DSL.In) | i <- [1..n] ]

--createInputLayer n = createLayer cnf $ fromList [ ((0, i), DSL.In) | i <- [1..n] ]
