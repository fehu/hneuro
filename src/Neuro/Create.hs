module Neuro.Create
( fromDSL
, CreationConf(..)
, createElem
, createStruct
, createLayer
, createConnections
) where

import NamedFunc
import Neuro
import Neuro.DSL as DSL
import Data.Map (assocs, (!))

fromDSL :: (Eq a) => DSLResolved -> CreationConf a -> Network a

createElem :: CreationConf a -> (DSL.Id, DSL.Elem) -> NetworkElem a

createLayer  :: (Eq a) => CreationConf a -> DSLLayer -> NetworkLayer a

createStruct :: (Eq a) => CreationConf a -> [DSLLayer] -> [NetworkLayer a]

createConnections :: CreationConf a -> [Layer a] -> [DSLConnection] -> [Synapse a]


data CreationConf a = CConf { zero  :: a
                            , w     :: DSL.Id -> [a]
                            , wf    :: DSL.Id -> NamedFunc a  (a -> a)
                            , tf    :: DSL.Id -> NamedFunc [a] a
                            }

fromDSL dsl conf = Network layouts synapses
                 where layouts  = createStruct conf $ DSL.layers dsl
                       synapses = createConnections conf (map layerElems layouts) $ connections dsl

createElem CConf { w  = w,
                   wf = wf,
                   tf = tf  } (id, DSL.Neuron)    = newNeuron id (w id) (wf id) (tf id)
createElem CConf { zero = z } (id, DSL.In)        = newInput  id z
createElem CConf { zero = z } (id, DSL.Out)       = newOutput id z
createElem _                  (id, DSL.Delayed d) = newDelayedInput id d []

createLayer conf layer = newNetworkLayer $ newLayer $ map (createElem conf) $ assocs $ iElems layer

createStruct conf ls = map (createLayer conf) ls

createConnections conf layers conns = map (createConnection conf layers) conns

getElem layers id = (layers !! idLayer id) ! id

createConnection conf layers conn = newSynapse fromElem toElem --(from conn) (to conn)
                                  where fromElem = getElem layers $ idFromPair $ from conn
                                        toElem   = getElem layers $ idFromPair $ to conn
