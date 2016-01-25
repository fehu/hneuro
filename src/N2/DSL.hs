{-# LANGUAGE ExistentialQuantification
           , TypeSynonymInstances
           , FlexibleInstances
--           , DataKinds
--           , PolyKinds
--           , ConstraintKinds
         #-}

-----------------------------------------------------------------------------
--
-- Module      :  N2.DSL
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module N2.DSL (

) where

import N2

import Data.Typeable
import Data.FixedList
--import qualified Data.FixedList as FL

data TODO

-----------------------------------------------------------------------------

data LayerType = InputLayer
               | HiddenLayer
               | OutputLayer

---- Cannot be empty
--data Layer a l = Layer LayerType (Cons l a) deriving Typeable
--
--data SomeLayer = forall a l . SomeLayer (Layer a l)
--
--data ANNetwork l = ANNetwork (Cons l SomeLayer)




-----------------------------------------------------------------------------

-- Cannot be empty
data Layer (t :: LayerType) a l = Layer (l a) deriving Typeable

data SomeLayer (t :: LayerType) = forall a l . SomeLayer (Layer t a l)
data SomeCLayer (t :: LayerType) l = forall a . SomeCLayer (Layer t a l)

data ANNElem = NInput
             | Neuron
             | NOutput

--data Synapse a b = Synapse a b

data ANNetwork inputs hidden outputs = ANNetwork (SomeCLayer InputLayer inputs)
                                                 (hidden (SomeLayer HiddenLayer))
                                                 (SomeCLayer OutputLayer outputs)


-----------------------------------------------------------------------------

data Nat = Succ Nat
         | Zero

type Nat0 = Zero
type Nat1 = Succ Zero
type Nat2 = Succ Nat1
type Nat3 = Succ Nat2
type Nat4 = Succ Nat3

--nat0 = Zero
--nat1 = Succ nat0
--nat2 = Succ nat1
--nat3 = Succ nat2

-----------------------------------------------------------------------------


--data NInput = NInput
--data NOutput = NOutput
--data Neuron = Neuron

--type family NeuronByLayer (t :: LayerType) :: *
--
--type instance NeuronByLayer InputLayer = NInput
--type instance NeuronByLayer OutputLayer = NOutput



--type family LayerByNElem t :: LayerType
--
--type instance LayerByNElem NInput = InputLayer

-----------------------------------------------------------------------------

class (FixedList f) => GenFixedList f where
    genFixedList :: a -> f a

instance GenFixedList FixedList1 where genFixedList a = a :. Nil
instance GenFixedList FixedList2 where genFixedList a = a :. genFixedList a
instance GenFixedList FixedList3 where genFixedList a = a :. genFixedList a

-----------------------------------------------------------------------------

--type family FixedListElem n (f :: * -> *) :: *       -- (n :: Nat)

type family IsNat a :: Nat

class FixedListElem (n :: Nat) f where
    selElem :: (IsNat b ~ n) => b -> Layer t a f -> a

instance FixedListElem Nat1 FixedList1 where selElem _ (Layer (a :. _)) = a
instance FixedListElem Nat1 FixedList2 where selElem _ (Layer (a :. _)) = a
instance FixedListElem Nat1 FixedList3 where selElem _ (Layer (a :. _)) = a
instance FixedListElem Nat1 FixedList4 where selElem _ (Layer (a :. _)) = a

instance FixedListElem Nat2 FixedList2 where selElem _ (Layer (_ :. a :. _)) = a
instance FixedListElem Nat2 FixedList3 where selElem _ (Layer (_ :. a :. _)) = a
instance FixedListElem Nat2 FixedList4 where selElem _ (Layer (_ :. a :. _)) = a

instance FixedListElem Nat3 FixedList3 where selElem _ (Layer (_ :. _ :. a :. _)) = a
instance FixedListElem Nat3 FixedList4 where selElem _ (Layer (_ :. _ :. a :. _)) = a

instance FixedListElem Nat4 FixedList4 where selElem _ (Layer (_ :. _ :. _ :. a :. _)) = a

--instance FixedListElem Nat1 FixedList1 where fElem _ (a :. _) = a
--instance FixedListElem Nat1 FixedList2 where fElem _ (a :. _) = a
--instance FixedListElem Nat1 FixedList3 where fElem _ (a :. _) = a
--instance FixedListElem Nat1 FixedList4 where fElem _ (a :. _) = a
--
--instance FixedListElem Nat2 FixedList2 where fElem _ (_ :. a :. _) = a
--instance FixedListElem Nat2 FixedList3 where fElem _ (_ :. a :. _) = a
--instance FixedListElem Nat2 FixedList4 where fElem _ (_ :. a :. _) = a
--
--instance FixedListElem Nat3 FixedList3 where fElem _ (_ :. _ :. a :. _) = a
--instance FixedListElem Nat3 FixedList4 where fElem _ (_ :. _ :. a :. _) = a
--
--instance FixedListElem Nat4 FixedList4 where fElem _ (_ :. _ :. _ :. a :. _) = a

--selElem :: (FixedListElem n l ~ a) => n -> Layer t a l -> a
--selElem n l = FixedListElem n l


--data Sel (n :: Nat) a = Sel a

--selElem :: (FixedListElem n l, IsNat x ~ n) => x -> Layer t a l -> a
--selElem i l = fElem i l

--a = selElem (Succ Zero) (Layer (1 :. Nil))

-----------------------------------------------------------------------------

type Sel prev sel a = prev a -> sel a


--type NextLayerElem prev = prev ->

--type Inputs l a self = l (Synapse (SelElem a) self)

--hiddenLayer :: Cons


-----------------------------------------------------------------------------


inputLayer :: (GenFixedList l) => SomeCLayer InputLayer l
inputLayer = SomeCLayer . Layer $ genFixedList NInput

--outputLayer :: (GenFixedList l) => SomeCLayer OutputLayer l
--outputLayer = SomeCLayer . Layer $ genFixedList NOutput

type InputLayer' l  = SomeCLayer InputLayer l
type OutputLayer' l = SomeCLayer OutputLayer l


describeANN :: ( GenFixedList i
               , GenFixedList h
               , GenFixedList o) => ANNetwork i h o
describeANN = ANNetwork inputLayer
                        undefined
                        undefined
--                        outputLayer

--nnet = ANNetwork (inputLayer :: InputLayer' FixedList2)
--                 undefined
--                 (outputLayer :: OutputLayer' FixedList2)


--nnet :: ANNetwork FixedList2 FixedList1 FixedList2
--nnet = ANNetwork inputLayer
--                 undefined
--                 outputLayer

nnet = describeANN :: ANNetwork FixedList2 FixedList1 FixedList2

-----------------------------------------------------------------------------












