{-# LANGUAGE FlexibleInstances
           , UndecidableInstances
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

class Neuron n where
    type NInput n :: * -> *
    neuronOutput :: n -> NInput n v -> v

--class Neuron n input where
--    type NInput n :: * -> *
--    neuronOutput :: n -> input n v -> v

class NeuralNetwork

-----------------------------------------------------------------------------

class SimpleNeuron n where
    type SNInput n :: * -> *
    neuronWeightedInput  :: n -> SNInput n v -> [v]
    neuronReduceInputs   :: n -> [v] -> v
    neuronActivationFunc :: n -> v -> v

instance (SimpleNeuron sn) =>
    Neuron sn where
        type NInput  n = SNInput n
        neuronOutput n = neuronActivationFunc n .
                         neuronReduceInputs n .
                         neuronWeightedInput n

-----------------------------------------------------------------------------



