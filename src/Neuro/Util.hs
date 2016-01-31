-----------------------------------------------------------------------------
--
-- Module      :  Neuro.Util
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

{-# LANGUAGE GADTs
           , FlexibleContexts
           , FlexibleInstances
           , ExistentialQuantification
       #-}

module Neuro.Util (

  Nat(..)
, Nat'
, N0
, N1
, N2
, N3

, SomeNat(..)
, Nat2Integral(..)

, Vec(..)
, Vec1
, Vec2
, Vec3

, GenVec(..)
, (+:)
, vec2list
, vecZip, vecZip'


, VecElem(..)
, vecElem1
, vecElem2
, vecElem3

) where

-----------------------------------------------------------------------------
-- from https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/kind-polymorphism-and-promotion.html

data Nat = Zero | Succ Nat

data Vec :: Nat -> * -> * where
  VNil  :: Vec Zero a
  VCons :: a -> Vec n a -> Vec (Succ n) a


-- from https://hackage.haskell.org/package/HList-0.4.0.0/docs/src/Data-HList-FakePrelude.html#HNat2Integral


class Nat2Integral (n :: Nat) where
    nat2int :: Integral i => Nat' n -> i


instance Nat2Integral Zero where nat2int _ = 0

instance Nat2Integral n => Nat2Integral (Succ n) where
    nat2int n = nat2int (nPred n) + 1

nPred :: Nat' (Succ n) -> Nat' n; nPred _ = undefined

-----------------------------------------------------------------------------

vec2list :: Vec n a -> [a]
vec2list (VCons h t) = h : vec2list t
vec2list VNil = []

vecZip :: Vec n a -> [b] -> Vec n (a, b)
vecZip (VCons a as) (b:bs) = (a, b) +: vecZip as bs
vecZip VNil _ = VNil
vecZip _ []   = error "vecZip: list is shorter than vector"

vecZip' :: [b] -> Vec n a -> Vec n (b, a)
vecZip' (b:bs) (VCons a as) = (b, a) +: vecZip' bs as
vecZip' _ VNil = VNil
vecZip' [] _   = error "vecZip': list is shorter than vector"


--vecLength :: Vec n a -> Int
--vecLength _ = nat2int (undefined :: Nat' n)

-----------------------------------------------------------------------------

instance Functor (Vec n) where fmap f (VCons h t) = VCons (f h) (fmap f t)
                               fmap _ VNil = VNil

instance Foldable (Vec n) where foldr _ b0 VNil = b0
                                foldr f b0 (VCons a t) = let res = f a b0
                                                         in foldr f res t

class GenVec (n :: Nat) where genVec :: (Int -> a) -> Nat' n ->  Vec n a

instance GenVec N0 where genVec _ _ = VNil
instance (GenVec np, Nat2Integral np) => GenVec (Succ np) where
    genVec f n = f (nat2int n) +: genVec f undefined


infixr 5 +:
(+:) :: a -> Vec n a -> Vec (Succ n) a
(+:) = VCons

-----------------------------------------------------------------------------

type N0 = Zero
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2

--type Nat' (n :: Nat) = Proxy n

data Nat' (n :: Nat)

data SomeNat = forall n . (Nat2Integral n) => SomeNat (Nat' n)

type Vec1 a = Vec N1 a
type Vec2 a = Vec N2 a
type Vec3 a = Vec N3 a

-----------------------------------------------------------------------------

class VecElem (n :: Nat) (vn :: Nat) where vecElem :: Nat' n -> Vec vn a -> a

instance VecElem N1 N1 where vecElem _ (VCons x _) = x

instance VecElem N1 N2 where vecElem _ (VCons x _) = x
instance VecElem N2 N2 where vecElem _ (VCons _ (VCons x _)) = x

instance VecElem N1 N3 where vecElem _ (VCons x _) = x
instance VecElem N2 N3 where vecElem _ (VCons _ (VCons x _)) = x
instance VecElem N3 N3 where vecElem _ (VCons _ (VCons _ (VCons x _))) = x


vecElem1 :: (VecElem N1 vn) => Vec vn a -> a
vecElem1 = vecElem (undefined :: Nat' N1)

vecElem2 :: (VecElem N2 vn) => Vec vn a -> a
vecElem2 = vecElem (undefined :: Nat' N2)

vecElem3 :: (VecElem N3 vn) => Vec vn a -> a
vecElem3 = vecElem (undefined :: Nat' N3)

-----------------------------------------------------------------------------
