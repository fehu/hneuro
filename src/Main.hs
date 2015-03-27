module Main
( main )
where

import NamedFunc
import Neuro
import Neuro.DSL as DSL
import Neuro.GenTikz
import Neuro.Create

import Data.List

main = crTest

------------------- DSL Tst -------------------

ilayer = Layer [In, Delayed 1, In, Delayed 2, In]
layer1 = Layer [Neuron, Neuron, Neuron, Delayed 3]
layer2 = Layer $ replicate 10 Neuron
ulayer = Layer (replicate 2 Out ++ [Delay])

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

--cr = fromDSL nnet CConf { zero = 0
--                        , weights  = \x -> [0]
--                        , transfer = \x -> head `named` "head"
--                        }

--cnf :: CreationConf Float
cnf = CConf { zero = 0
            , weights = \_ -> [1, 2]
            , transfer = \_ -> (head `named` "head")
            }

crElem = [ createElem cnf ((1,1), DSL.Neuron)
         , createElem cnf ((1,2), DSL.In)
         , createElem cnf ((1,3), DSL.Out)
         , createElem cnf ((1,4), DSL.Delayed 1)
         , createElem cnf ((1,5), DSL.Delay)
         ]

crTest = putStrLn $ show crElem

------------------- Tikz Tst -------------------

writeTst :: String -> IO ()
writeTst txt = writeFile "test.tikz" txt

tikzTest :: IO ()
tikzTest = do
    let d   = intercalate "\n" defs
    let llt = genStruct istruct
    let cc  = genConnections rconnections []
    let txt = intercalate "\n\n" [d, llt, cc]
    let res = tikzScope "tikzpicture" txt []
    writeTst res

--------------------------------------------------
