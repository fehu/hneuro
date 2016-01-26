{-# LANGUAGE GADTs, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}


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

data Vec :: * -> Nat -> * where
  Nil  :: Vec a Ze
  Cons :: a -> Vec a n -> Vec a (Su n)

-----------------------------------------------------------------------------

type Nat0 = Ze
type Nat1 = Su Nat0
type Nat2 = Su Nat1

data SomeNat (n :: Nat)

type Vec1 a = Vec a Nat1
type Vec2 a = Vec a Nat2

-----------------------------------------------------------------------------

class VecElem (n :: Nat) (vn :: Nat) where vecElem :: SomeNat n -> Vec a vn -> a

instance VecElem Nat1 Nat1 where vecElem _ (Cons x _) = x

vecElem1 :: (VecElem Nat1 vn) => Vec a vn -> a
vecElem1 = vecElem (undefined :: SomeNat Nat1)

-----------------------------------------------------------------------------

--data NElem = NElem
data NElem = NInput
           | Neuron
           | NOutput

type NLayer n = Vec NElem n

--type NLayers = HList

--type PreviousLayers ln = Vec

--type SelectSynapseInput prev = prev -> NElem

type SelectSynapseInput prev = HList prev -> NElem

--type EmptyNNet = HNil


----------------------------------------------------------------------------

--mkInputLayer ::



mkNeuron :: HList prev -> Vec (SelectSynapseInput prev) n -> NElem
mkNeuron = undefined

--nextLayer :: HList prev -> Vec (SelectSynapseInput prev) n -> NLayer n
--nextLayer l selVec = undefined

----------------------------------------------------------------------------



----------------------------------------------------------------------------


foo :: Nat -> Vec1 a -> a
foo _ (Cons x _) = x

a = foo Ze
b = foo (Su Ze)

-----------------------------------------------------------------------------

type A = Vec Int (Su Ze)

nat1 :: Nat
nat1 = Su Ze
