module Main
( main )
where
import Neuro
import Neuro.DSL
import Neuro.GenTikz

import Data.List

main = test


------------------- Tst -------------------

ilayer = Layer [In, Delayed 1, In, Delayed 2, In]
layer1 = Layer [Neuron, Neuron, Neuron, Delayed 3]
layer2 = Layer $ replicate 10 Neuron
ulayer = Layer (replicate 2 Out ++ [Delay i | i <- [1..2]])

struct = neuroNetStructure ilayer [layer1, layer2] ulayer
istruct = fst $ identifyNeuroNetStruct 0 struct

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

writeTst :: String -> IO ()
writeTst txt = writeFile "test.tikz" txt

test :: IO ()
test = do
    let d   = intercalate "\n" defs
    let llt = genStruct istruct
    let cc  = genConnections rconnections []
    let txt = intercalate "\n\n" [d, llt, cc]
    let res = tikzScope "tikzpicture" txt []
    writeTst res
