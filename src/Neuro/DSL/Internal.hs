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

import Neuro.Util

import Data.HList

-----------------------------------------------------------------------------

data NElem = NInput
           | forall n . Neuron (Vec n NElem)

type NLayer n = Vec n NElem

data SomeLayer = forall n . SomeLayer (Vec n NElem)

data NeuronInputs prev = forall n . NeuronInputs (HList prev -> Vec n NElem)

----------------------------------------------------------------------------

mkNeuron' :: HList prev -> NeuronInputs prev -> NElem
mkNeuron' hl (NeuronInputs sel) = Neuron $ sel hl

