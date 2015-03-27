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
import Data.Map (assocs)

fromDSL :: (Eq a) => DSL.NeuroNet -> CreationConf a -> Network a

createElem :: CreationConf a -> (DSL.Id, DSL.Elem) -> NetworkElem a

createLayer  :: (Eq a) => CreationConf a -> DSL.IdentifiedLayer -> NetworkLayer a

createStruct :: (Eq a) => CreationConf a -> DSL.IdentifiedNeuroNetStruct -> [NetworkLayer a]

createConnections :: CreationConf a -> DSL.HardConnections -> [Synapse]


data CreationConf a = CConf { zero     :: a
                            , weights  :: DSL.Id -> [a]
                            , transfer :: DSL.Id -> NamedFunc ([a] -> a)
                            }

fromDSL (nstruct, conn) conf = Network layouts synapses
                             where layouts  = createStruct conf nstruct
                                   synapses = createConnections conf conn

createElem CConf { weights  = w, transfer = f } (id, DSL.Neuron)    = newNeuron id (w id) (f id)
createElem CConf { zero = z                   } (id, DSL.In)        = newInput  id z
createElem CConf { zero = z                   } (id, DSL.Out)       = newOutput id z
createElem _                                    (id, DSL.Delayed d) = newDelayedInput id d []
createElem CConf { zero = z                   } (id, DSL.Delay)     = newDelaySink id z

createLayer conf elMap = newNetworkLayer $ newLayer $ map (createElem conf) $ assocs elMap

createStruct conf nstruct = map (createLayer conf) $ joinNetStruct nstruct

createConnections conf (DSL.HardConnections conn) = [Synapse from to | Just (from, to) <- map linkId conn]
