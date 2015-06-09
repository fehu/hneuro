module TestsCommon(
  nnetDsl1, nnet1, nnetSynsCache1, nnet1Inps
, nnetDsl2, nnet2, nnetSynsCache2, nnet2Inps1
, nnetDsl3, nnet3, nnetSynsCache3 --, nnet3InpsExp1
) where

import NamedFunc
import Neuro
import Neuro.DSL
import Neuro.Create


--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

nnetDsl1 = dsl $ NeuroNet  $ Layer [In, Delayed 1, In, Delayed 2, In]
                           : Layer [Neuron, Neuron, Neuron, Delayed 3]
                           : Layer (replicate 10 Neuron)
                           : Layer (replicate 4 Out)
                           : all2all 0 1 [] [2]
--                           : all2all 1 2 [] [1, 2, 9, 10]
                           : [ (1 `sel` 1)         --> (2 `sel` 1)
                             , (1 `sel` 2)         --> (2 `sel'` [1..5])
                             , (2 `sel'` [1..4])   --> (3 `sel'` [1, 3])
                             , (2 `sel'` [5..8])   --> (3 `sel'` [2, 4])
                             ]

nnet1 = fromDSL nnetDsl1 cnf1

cnf1 = CConf { zero = 0
             , w = \_ -> [1, 2]
             , wf = \_ -> named (*) "*"
             , tf = \_ -> named (sin . sum) "sin"
             }

nnetSynsCache1 = mkSynapsesCache $ synapses nnet1


nnet1Inps = [ [ (ElemId 0 1, 0.67)
              , (ElemId 0 3, 0.205)
              , (ElemId 0 5, -1.005)
              ]
            , [ (ElemId 0 1, 0.227)
              , (ElemId 0 3, -0.502)
              , (ElemId 0 5, 6.34)
              ]
            ]

--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

nnetDsl2 = dsl $ NeuroNet  $ Layer [In, Delayed 1]
                           : Layer (replicate 2 Neuron)
                           : Layer (replicate 3 Neuron)
                           : Layer (replicate 2 Out)
                           : all2all 0 1 [] []
                           : all2all 1 2 [] []
                           : all2all 2 3 [] []
                           : [ (1 `sel` 2)   <-- (3 `sel` 1)
                             ]

cnf2 = CConf { zero = 0
             , w  = \(l, _) -> case l of 0 -> error "shouldn't happen"
                                         1 -> replicate 1 2
                                         2 -> replicate 1 3
                                         3 -> error "shouldn't happen"
             , wf = \_ -> named (*) "*"
             , tf = \_ -> named (sin . sum) "sin"
             }

nnet2 = fromDSL nnetDsl2 cnf2

nnetSynsCache2 = mkSynapsesCache $ synapses nnet2

nnet2Inps1 = [ [ (ElemId 0 1, 0.67) ]
             , [ (ElemId 0 1, 0.205) ]
             , [ (ElemId 0 1, -1.005) ]
             , [ (ElemId 0 1, 0.227) ]
             , [ (ElemId 0 1, -0.502) ]
             , [ (ElemId 0 1, 6.34) ]
             ]

--nnet2Expected =

--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

nnetDsl3 = dsl $ NeuroNet  $ Layer [In, In]
                           : Layer (replicate 2 Neuron)
                           : Layer (replicate 1 Out)
                           : all2all 0 1 [] []
                           : all2all 1 2 [] []
                           : []

cnf3 = CConf { zero = 0
             , w  = \(l, _) -> case l of 0 -> error "shouldn't happen"
                                         1 -> replicate 1 2
                                         2 -> error "shouldn't happen"
             , wf = \_ -> named (*) "*"
             , tf = \_ -> named (sin . sum) "sin"
             }

nnet3 = fromDSL nnetDsl3 cnf3

nnetSynsCache3 = mkSynapsesCache $ synapses nnet3

--nnet3InpsExp1 =  [ [ (ElemId 0 1, 0.67, ??) ]
--                 , [ (ElemId 0 1, 0.205) ]
--                 , [ (ElemId 0 1, -1.005) ]
--                 , [ (ElemId 0 1, 0.227) ]
--                 , [ (ElemId 0 1, -0.502) ]
--                 , [ (ElemId 0 1, 6.34) ]
--                 ]
