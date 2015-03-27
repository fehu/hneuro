module Main
( main )
where

import NamedFunc
--import Neuro
import Neuro.DSL as DSL
import Neuro.GenTikz
import Neuro.Create

import Data.List

main = tikzTest

------------------- DSL Tst -------------------

ilayer = Layer [In, Delayed 1, In, Delayed 2, In]
layer1 = Layer [Neuron, Neuron, Neuron, Delayed 3]
layer2 = Layer $ replicate 10 Neuron
ulayer = Layer (replicate 2 Out ++ [Delay, Delay])

struct = neuroNetStructure ilayer [layer1, layer2] ulayer
istruct = identifyNeuroNetStruct struct

all2all' = all2all istruct

connections = (all2all' 0 1 [] [2])
--          +:+ (all2all' 1 2 [] [1, 2, 9, 10])
          +:+ (Connections [
                    (1 `sel` 1)         --> (2 `sel` 1)
                  , (1 `sel` 2)         --> (2 `sel'` [1..5])
                  , (2 `sel'` [1..4])   --> (3 `sel'` [1, 3])
                  , (2 `sel'` [5..8])   --> (3 `sel'` [2, 4])
                ])
rconnections = resolveConnections istruct connections

nnet = (istruct, rconnections)

------------------- Creation Tst -------------------

--cnf = CConf { zero = 0
--            , w = \_ -> [1, 2]
--            , tf = \_ -> ((\x -> 1 / sum x ) `named` "1/x")
--            }
--
--crElem = [ createElem cnf ((1,1), DSL.Neuron)
--         , createElem cnf ((1,2), DSL.In)
--         , createElem cnf ((1,3), DSL.Out)
--         , createElem cnf ((1,4), DSL.Delayed 1)
--         , createElem cnf ((1,5), DSL.Delay)
--         ]
--
--crTest = putStrLn $ show crElem

------------------- Tikz Tst -------------------

writeTst :: String -> IO ()
writeTst txt = writeFile "test.tikz" txt

tikzTest :: IO ()
tikzTest = do
    let tikzNet = genTikz (istruct, rconnections)
    let tikz    = tikzScope "tikzpicture" tikzNet []
    writeTst tikz

--------------------------------------------------
