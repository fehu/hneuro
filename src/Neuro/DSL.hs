{-# LANGUAGE GADTs
           , FlexibleContexts
           , TypeSynonymInstances
           , FlexibleInstances
           , TypeOperators
           , ExistentialQuantification
       #-}


-----------------------------------------------------------------------------
--
-- Module      :  N3
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

module Neuro.DSL (

) where

import Data.HList

-----------------------------------------------------------------------------
-- From https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/kind-polymorphism-and-promotion.html

data Nat = Ze | Su Nat

data Vec :: Nat -> * -> * where
  VNil  :: Vec Ze a
  VCons :: a -> Vec n a -> Vec (Su n) a

instance Functor (Vec n) where fmap f (VCons h t) = VCons (f h) (fmap f t)

instance Foldable (Vec n) where foldr _ b0 VNil = b0
                                foldr f b0 (VCons a t) = let res = f a b0
                                                         in foldr f res t

class GenVec (n :: Nat) where genVec :: a -> SomeNat n ->  Vec n a

instance                GenVec Nat0     where genVec _ _ = VNil
instance (GenVec np) => GenVec (Su np)  where genVec a _ = a +: genVec a undefined


infixr 5 +:
(+:) :: a -> Vec n a -> Vec (Su n) a
(+:) = VCons

-----------------------------------------------------------------------------

type Nat0 = Ze
type Nat1 = Su Nat0
type Nat2 = Su Nat1
type Nat3 = Su Nat2

data SomeNat (n :: Nat)

type Vec1 a = Vec Nat1 a
type Vec2 a = Vec Nat2 a
type Vec3 a = Vec Nat3 a

-----------------------------------------------------------------------------

class VecElem (n :: Nat) (vn :: Nat) where vecElem :: SomeNat n -> Vec vn a -> a

instance VecElem Nat1 Nat1 where vecElem _ (VCons x _) = x

instance VecElem Nat1 Nat2 where vecElem _ (VCons x _) = x
instance VecElem Nat2 Nat2 where vecElem _ (VCons _ (VCons x _)) = x

instance VecElem Nat1 Nat3 where vecElem _ (VCons x _) = x
instance VecElem Nat2 Nat3 where vecElem _ (VCons _ (VCons x _)) = x
instance VecElem Nat3 Nat3 where vecElem _ (VCons _ (VCons _ (VCons x _))) = x


vecElem1 :: (VecElem Nat1 vn) => Vec vn a -> a
vecElem1 = vecElem (undefined :: SomeNat Nat1)

vecElem2 :: (VecElem Nat2 vn) => Vec vn a -> a
vecElem2 = vecElem (undefined :: SomeNat Nat2)

vecElem3 :: (VecElem Nat3 vn) => Vec vn a -> a
vecElem3 = vecElem (undefined :: SomeNat Nat3)

-----------------------------------------------------------------------------

data NeuronInputs prev = forall n . NeuronInputs (HList prev -> Vec n NElem)

data NElem = NInput
           | forall n . Neuron (Vec n NElem)
--           | NOutput

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


-- * all the inputs are in the first layer
-- * the last layer contains the outputs

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


