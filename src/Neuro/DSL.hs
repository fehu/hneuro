-----------------------------------------------------------------------------
--
-- Module      :  Neuro.DSL
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-- * all the inputs are in the first layer
-- * the last layer contains the outputs

{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts #-}

module Neuro.DSL (

  nnet
, inputsLayer
, nextLayer
, lastLayer
, (==>)

, NNDescriptor(..)
, NElem

, NeuronInputs(..)
--, SomeLayer(..)
, module Nat.Vec
, module Data.HList

) where

import Nat.Vec
import Neuro.DSL.Internal

import Data.HList


----------------------------------------------------------------------------

type NNBuilder prev = (HList prev, Int)

inputsLayer :: (GenVec n) => Nat' n -> NNBuilder '[NLayer n]
inputsLayer _ = (genVec NInput (undefined :: Nat' n) .*. HNil, 0)


nextLayer :: Vec n (NeuronInputs prev) -> NNBuilder prev -> NLayer n
nextLayer nsel (prev, l) = fmap (uncurry (mkNeuron' prev (l+1))) $ vecZip' [1..] nsel

-- | Alias for 'nextLayer'.
lastLayer = nextLayer

(==>) :: NNBuilder prev -> (NNBuilder prev -> NLayer n) -> NNBuilder (NLayer n ': prev)
b@(prev, l) ==> mkNext = let next = mkNext b
                    in (next .*. prev, l+1)

newtype NNDescriptor = NNDescriptor [SomeLayer]


data HLayersToList = HLayersToList

instance ([SomeLayer] ~ r) => ApplyAB HLayersToList (Vec n NElem, r) [SomeLayer] where
    applyAB _ (vec, acc) = SomeLayer vec : acc


nnet hlb = NNDescriptor . hFoldr HLayersToList ([] :: [SomeLayer]) $ fst hlb

-----------------------------------------------------------------------------


