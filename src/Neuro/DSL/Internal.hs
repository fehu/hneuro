-----------------------------------------------------------------------------
--
-- Module      :  Neuro.DSL.Internal
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Neuro.DSL.Internal (

  NElem(..)
, NeuronInputs(..)

, NLayer
, SomeLayer(..)

, mkNeuron'

) where

import Nat.Vec

import Data.HList

-----------------------------------------------------------------------------

data NElem = NInput Int
           | forall n . Neuron (Vec n NElem) Int Int

type NLayer n = Vec n NElem

data SomeLayer = forall n . SomeLayer (Vec n NElem)

data NeuronInputs prev = forall n . NeuronInputs (HList prev -> Vec n NElem)

----------------------------------------------------------------------------

mkNeuron' :: HList prev -> Int -> Int -> NeuronInputs prev -> NElem
mkNeuron' hl n i (NeuronInputs sel) = Neuron (sel hl) n i

