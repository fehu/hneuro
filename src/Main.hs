module Main where
import Neuro
import Neuro.DSL
import Neuro.GenTikz as Tikz
--import Data.Text.Lazy(unpack)

main = test


------------------- Tst -------------------

ilayer = Layer [In, Delayed 1, In, Delayed 2, In]
layer1 = Layer [Neuron, Neuron, Neuron, Delayed 3]
layer2 = Layer $ replicate 10 Neuron
ulayer = Layer (replicate 2 Out ++ [Delay i | i <- [1..3]])

struct = neuroNetStructure ilayer [layer1, layer2] ulayer
istruct = fst $ identifyNeuroNetStruct 0 struct

all2all' = all2all istruct

connections = (all2all' 0 1 [] [])
          +:+ (all2all' 1 2 [] [1, 2, 9, 10])
          +:+ (Connections [
                    (1 `sel` 1) --> (2 `sel` 1)
                  , (1 `sel` 2) --> (2 `sel'` [1..5])
                  , (1 `sel'` [1..5]) --> (2 `sel'` [9, 10])
                ])

l0 = Tikz.genLayer 0 $ getILayer 0 istruct

test :: IO ()
test = putStrLn $ l0 []
