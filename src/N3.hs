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

module N3 (

) where

import Data.HList

-----------------------------------------------------------------------------
-- From https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/kind-polymorphism-and-promotion.html

data Nat = Ze | Su Nat

data Vec :: Nat -> * -> * where
  VNil  :: Vec Ze a
  VCons :: a -> Vec n a -> Vec (Su n) a

--type VecR a (n :: Nat) = Vec n a

instance Functor (Vec n) where fmap f (VCons h t) = VCons (f h) (fmap f t)


infixr 5 +:
(+:) :: a -> Vec n a -> Vec (Su n) a
(+:) = VCons

--instance Foldable (Vec n) where foldr f b0 t =

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

--type Inputs

--data NElem = NElem
data NElem = NInput
           | forall n . Neuron (Vec n NElem)
           | NOutput

type NLayer n = Vec n NElem

--type NLayers = HList

--type PreviousLayers ln = Vec

--type SelectSynapseInput prev = prev -> NElem

--type SelectSynapseInput prev = HList prev -> NElem

--type EmptyNNet = HNil


----------------------------------------------------------------------------

--mkInputLayer ::

--class Neuron n (in' :: Nat) where nInputs   :: n in' -> Vec in' NElem
--                                  newNeuron :: Vec in' NElem -> n in'

mkNeuron' :: HList prev -> NeuronInputs prev -> NElem
mkNeuron' hl (NeuronInputs sel) = Neuron $ sel hl

--nInputs (Neuron v) = v

--data NeuronDescriptor (n :: Nat) = NeuronDescriptor (Vec n NElem)

--instance Neuron NeuronDescriptor n where nInputs (NeuronDescriptor v) = v
--                                         newNeuron = NeuronDescriptor

--mkNeuron :: HList prev -> Vec (SelectSynapseInput prev) n -> NElem
--mkNeuron = undefined

--nextLayer :: HList prev -> Vec (SelectSynapseInput prev) n -> NLayer n
--nextLayer l selVec = undefined

----------------------------------------------------------------------------

inputsLayer :: SomeNat n -> HList '[NLayer n]
inputsLayer = undefined -- TODO

nextLayer2 :: HList prev -> Vec n (NeuronInputs prev) -> HList (NLayer n ': prev)
nextLayer2 prev nsel = layer .*. prev
    where layer = fmap (mkNeuron' prev) nsel



nextLayer :: Vec n (NeuronInputs prev) -> HList prev -> NLayer n
nextLayer nsel prev = fmap (mkNeuron' prev) nsel

(==>) :: HList prev -> (HList prev -> NLayer n) -> HList (NLayer n ': prev)
prev ==> mkNext = let next = mkNext prev
                    in next .*. prev

--data LayersBuilder (nIn :: Nat) (nOut :: Nat) hl = LayersBuilder hl

--newtype LayersBuilder hl = LayersBuilder hl
--
--
--instance Functor LayersBuilder
----    where fmap f (LayersBuilder hl) =
--
--instance Applicative LayersBuilder
--
--instance Monad LayersBuilder


--test = do l1 <- inputLayer
--          l2 <- nextLayer2 (HCons layer _ -> vecElem1 layer)


test = undefined
    where l1 = inputsLayer (undefined :: SomeNat Nat1)
          l2 = nextLayer2 l1
                $ NeuronInputs (\(HCons layer _) -> vecElem1 layer +: VNil) +: VNil

          -- undefined

test2 = inputsLayer (undefined :: SomeNat Nat2)
    ==> nextLayer (  NeuronInputs (\(HCons il _) -> vecElem1 il +: vecElem2 il +: VNil)
                  +: NeuronInputs (\(HCons il _) -> vecElem1 il +: vecElem2 il +: VNil)
                  +: VNil
                  )
    ==> nextLayer (  NeuronInputs (\(HCons l1 _)            -> vecElem1 l1 +: vecElem2 l1 +: VNil)
                  +: NeuronInputs (\(HCons l1 (HCons il _)) -> vecElem1 il +: vecElem2 l1 +: VNil)
                  +: NeuronInputs (\(HCons l1 (HCons il _)) -> vecElem1 l1 +: vecElem2 il +: VNil)
                  +: VNil
                  )
    ==> nextLayer (  NeuronInputs (\(HCons l2 _) -> vecElem1 l2
                                                 +: vecElem2 l2
                                                 +: vecElem3 l2
                                                 +: VNil
                                  ) +: VNil
                  )

----------------------------------------------------------------------------


foo :: Nat -> Vec1 a -> a
foo _ (VCons x _) = x

a = foo Ze
b = foo (Su Ze)

-----------------------------------------------------------------------------

type A = Vec (Su Ze) Int

nat1 :: Nat
nat1 = Su Ze
