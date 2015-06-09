module Neuro.Create
( fromDSL
, CreationConf(..)
, createElem
--, createStruct
, createLayer
, createDelayed
, createConnections
) where

import NamedFunc
import Neuro
import Neuro.DSL as DSL
import Data.Map (assocs, (!))

fromDSL :: (Eq a) => DSLResolved -> CreationConf a -> Network a

createNeuron  :: CreationConf a -> DSL.Id -> Neuron a
createDelayed :: DSLDelayed -> DelayedInput a

createLayer  :: (Eq a) => CreationConf a -> DSLLayer -> Layer a

--createStruct :: (Eq a) => CreationConf a -> [DSLLayer] -> [] -> [Layer a]

createConnections :: CreationConf a -> [Layer a] -> [DSLConnection] -> [Synapse]


data CreationConf a = CConf { w     :: DSL.Id -> [a]
                            , wf    :: DSL.Id -> NamedFunc a  (a -> a)
                            , tf    :: DSL.Id -> NamedFunc [a] a
                            }

fromDSL dsl conf = Network layouts synapses
                 where layouts  = createStruct conf $ DSL.layers dsl
                       synapses = createConnections conf (map DSL.layers layouts) $ connections dsl

createNeuron CConf { w  = w
                   , wf = wf
                   , tf = tf } id = newNeuron id (w id) (wf id) (tf id)

createDelayed (DSLDelayed id d)   = newDelayedInput id d []

createLayer conf layer = newLayer $ map (createNeuron conf) $ assocs $ neuronIds layer -- ??

createStruct conf ls = map (createLayer conf) ls

createConnections conf layers conns = map (createConnection conf layers) conns

getElem layers id = (layers !! neuronIdLayer id) ! id

createConnection conf layers conn = Synapse fromElem toElem --(from conn) (to conn)
                                  where fromElem = getElem layers $ neuronIdFromPair $ DSL.from conn
                                        toElem   = getElem layers $ neuronIdFromPair $ DSL.from conn
