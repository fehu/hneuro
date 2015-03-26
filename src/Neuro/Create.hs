module Neuro.Create
( fromDSL
, CreationConf
, newElem
, newStruct
, newConnections
) where

import NamedFunc
import Neuro
import Neuro.DSL as DSL
import Data.Map (assocs)

fromDSL :: (Eq a) => DSL.NeuroNet -> CreationConf a -> Network a

newElem :: CreationConf a -> (DSL.Id, DSL.Elem) -> NetworkElem a

--newLayer  :: (Eq a) => CreationConf a -> DSL.IdentifiedLayer -> NetworkLayer a

newStruct :: (Eq a) => CreationConf a -> DSL.IdentifiedNeuroNetStruct -> [NetworkLayer a]

newConnections :: CreationConf a -> DSL.HardConnections -> [Synapse]


data CreationConf a = CConf { weights  :: DSL.Id -> [a]
                            , transfer :: DSL.Id -> NamedFunc ([a] -> a)
                            }

fromDSL (nstruct, conn) conf = Network layouts synapses
                             where layouts  = newStruct conf nstruct
                                   synapses = newConnections conf conn

newElem CConf { weights  = w, transfer = f } (id, DSL.Neuron)    = newNeuron id (w id) (f id)

newLayer conf elMap = newNetworkLayer $ Neuro.newLayer $ map (newElem conf) $ assocs elMap

newStruct conf nstruct = map (Neuro.Create.newLayer conf) $ joinNetStruct nstruct

newConnections conf (DSL.HardConnections conn) = [Synapse from to | Just (from, to) <- map linkId conn]
