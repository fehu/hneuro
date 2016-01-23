{-# LANGUAGE FlexibleInstances
           , UndecidableInstances
           , ExistentialQuantification
           , ConstraintKinds
         #-}

-----------------------------------------------------------------------------
--
-- Module      :  Neuro
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module Neuro (

) where

import GHC.Exts

-----------------------------------------------------------------------------

--class NeuralNetwork nn where
--    type NNInput  nn :: * -> *
--    type NNOutput nn :: * -> *
--
--    nnOutput :: nn -> NNInput nn v -> NNOutput nn v

class NeuralNetwork nn v where
    type NNInput  nn v :: *
    type NNOutput nn v :: *

    nnOutput :: nn -> NNInput nn v -> NNOutput nn v


-----------------------------------------------------------------------------

data NeuroNetElem n =
    forall a . (Neuron a)         => SomeNeuron (a n)
  | forall i . (NeuroNetInput i)  => SomeNNInput (i n)
  | forall o . (NeuroNetOutput o) => SomeNNOutput (o n)

-----------------------------------------------------------------------------

class Neuron a where
    type NInput a :: * -> *
    neuronOutput :: a n -> NInput a n -> n

class NeuroNetInput a where
    hasMoreInput        :: a n -> IO Bool
    nextNNetInput       :: a n -> IO n
    maybeNextNNetInput  :: a n -> IO (Maybe n)

class NeuroNetOutput a where
    hasMoreOutput       :: a n -> IO Bool
    nextNNetOutput      :: a n -> IO n
    maybeNextNNetOutput :: a n -> IO (Maybe n)

-----------------------------------------------------------------------------

class (Neuron a) =>
    Synapse (s :: * -> *) (a :: * -> *) where
        synapseInput  :: s n -> a n
        synapseOutput :: s n -> a n



-----------------------------------------------------------------------------
-----------------------------------------------------------------------------


data ListNNInput nn n = ListNNInput [n]


data PredefinedNetInput nn n = PredefinedNetInput [NNInput nn n]


--data NInputRef n = NInputRef (IORef n)



--class SimpleNeuron n where
--    type SNInput n :: * -> *
--    neuronWeightedInput  :: n v -> SNInput n v -> [v]
--    neuronReduceInputs   :: n v -> [v] -> v
--    neuronActivationFunc :: n v-> v -> v
--
--instance (SimpleNeuron sn) =>
--    Neuron sn where
--        type NInput  n = SNInput n
--        neuronOutput n = neuronActivationFunc n .
--                         neuronReduceInputs n .
--                         neuronWeightedInput n

-----------------------------------------------------------------------------



