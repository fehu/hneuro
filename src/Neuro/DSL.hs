{-# LANGUAGE TypeOperators
           , ExistentialQuantification
       #-}


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

module Neuro.DSL (

) where

import Neuro.Util

import Data.HList

-----------------------------------------------------------------------------

data NeuronInputs prev = forall n . NeuronInputs (HList prev -> Vec n NElem)

data NElem = NInput
           | forall n . Neuron (Vec n NElem)

type NLayer n = Vec n NElem


----------------------------------------------------------------------------

mkNeuron' :: HList prev -> NeuronInputs prev -> NElem
mkNeuron' hl (NeuronInputs sel) = Neuron $ sel hl

----------------------------------------------------------------------------

inputsLayer :: (GenVec n) => SomeNat n -> HList '[NLayer n]
inputsLayer _ = genVec NInput (undefined :: SomeNat n) .*. HNil


nextLayer :: Vec n (NeuronInputs prev) -> HList prev -> NLayer n
nextLayer nsel prev = fmap (mkNeuron' prev) nsel

-- | Alias for 'nextLayer'.
lastLayer = nextLayer

(==>) :: HList prev -> (HList prev -> NLayer n) -> HList (NLayer n ': prev)
prev ==> mkNext = let next = mkNext prev
                    in next .*. prev






----------------------------------------------------------------------------
 ---- Examples ---- Examples ---- Examples ---- Examples ---- Examples ----
----------------------------------------------------------------------------

test = inputsLayer (undefined :: SomeNat Nat2)
    ==> nextLayer (  NeuronInputs (\(HCons il _) -> vecElem1 il +: vecElem2 il +: VNil)
                  +: NeuronInputs (\(HCons il _) -> vecElem1 il +: vecElem2 il +: VNil)
                  +: VNil
                  )
    ==> nextLayer (  NeuronInputs (\(HCons l1 _)            -> vecElem1 l1 +: vecElem2 l1 +: VNil)
                  +: NeuronInputs (\(HCons l1 (HCons il _)) -> vecElem1 il +: vecElem2 l1 +: VNil)
                  +: NeuronInputs (\(HCons l1 (HCons il _)) -> vecElem1 l1 +: vecElem2 il +: VNil)
                  +: VNil
                  )
    ==> lastLayer (  NeuronInputs (\(HCons l2 _) -> vecElem1 l2
                                                 +: vecElem2 l2
                                                 +: vecElem3 l2
                                                 +: VNil
                                  ) +: VNil
                  )


-----------------------------------------------------------------------------


