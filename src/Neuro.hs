module Neuro
( NetworkElem
, ElemId, getId
, newNeuron, newInput, newOutput, newDelaySink, newDelayedInput
,  isNeuron,  isInput,  isOutput,  isDelaySink,  isDelayedInput
, Layer
, isInpLayer, isHiddenLayer, isOutLayer
, newLayer
, NetworkLayer
, newNetworkLayer
, Synapse(..)
, Network(..)
) where

import NamedFunc
import Data.Map (Map, keys, elems, fromList)
import Data.Set (Set)

--  -- Network Elements
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

type ElemId = (Int, Int)

data NetworkElem a = Neuron {id :: ElemId, weights :: [a], transfer :: NamedFunc ([a] -> a)}
                   | Input         ElemId  a
                   | Output        ElemId  a
                   | DelaySink     ElemId  a
                   | DelayedInput  ElemId [a] Int
                   deriving Eq

newNeuron id w f        = Neuron id w f
newInput id x           = Input id x
newOutput id x          = Output id x
newDelayedInput id d xs = DelayedInput id xs d
newDelaySink id x       = DelaySink id x

isNeuron (Neuron _ _ _)             = True
isNeuron _                          = False

isInput (Input _ _)                 = True
isInput _                           = False

isOutput (Output _ _)               = True
isOutput _                          = False

isDelayedInput (DelayedInput _ _ _) = True
isDelayedInput _                    = False

isDelaySink (DelaySink _ _)         = True
isDelaySink _                       = False

getId (Neuron id _ _)       = id
getId (Input id _)          = id
getId (Output id _)         = id
getId (DelaySink id _ )     = id
getId (DelayedInput id _ _) = id

instance Show a => Show (NetworkElem a) where
    show (Neuron id w f)            = "neuron:" ++ show id ++ "(" ++ show w ++ ", " ++ show f ++ ")"
    show (Input id x)               = "in:"     ++ show id ++ "=" ++ show x
    show (Output id x)              = "out:"    ++ show id ++ "=" ++ show x
    show (DelaySink id x)           = "delay:"  ++ show id ++ "=" ++ show x
    show (DelayedInput id d xs)     = "delayed:" ++ show id ++ "=" ++ show xs

instance Eq a => Ord (NetworkElem a) where
    (Neuron id1 _ _)        `compare` (Neuron id2 _ _)       = id1 `compare` id2
    (Input id1 _)           `compare` (Input id2 _)          = id1 `compare` id2
    (Output id1 _)          `compare` (Output id2 _)         = id1 `compare` id2
    (DelaySink id1 _)       `compare` (DelaySink id2 _)      = id1 `compare` id2
    (DelayedInput id1 _ _)  `compare` (DelayedInput id2 _ _) = id1 `compare` id2




--  -- Network Layers
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

type Layer a = Map ElemId (NetworkElem a)

newLayer :: Eq a => [NetworkElem a] -> Layer a
newLayer xs = if not $ null xs then fromList $ map (\x -> (getId x, x)) xs
                               else error "empty new layer"

data NetworkLayer a = InLayer     (Layer a)
                    | HiddenLayer (Layer a)
                    | OutLayer    (Layer a)

isInpLayer (InLayer _)          = True
isInpLayer _                    = False

isHiddenLayer (HiddenLayer _)   = True
isHiddenLayer _                 = False

isOutLayer (OutLayer _)         = True
isOutLayer _                    = False

compatible :: Layer a -> [NetworkElem a -> Bool] -> [NetworkElem a -> Bool] -> Bool
compatible layer req can = all tstAll (elems layer) && any tst (elems layer)
                     where tst x    = any ($ x) req
                           tstAll x = any ($ x) (req ++ can)
--                     where tstf = \k -> foldr (\f a -> f(k) || a) False tst
--                           tst  = [f, isDelayedInput, isDelaySink]

newNetworkLayer :: Layer a -> NetworkLayer a
newNetworkLayer layer | compatible layer [isInput ] [isDelayedInput             ]    = InLayer layer
                      | compatible layer [isNeuron,  isDelayedInput, isDelaySink] [] = HiddenLayer layer
                      | compatible layer [isOutput] [                isDelaySink]    = OutLayer layer
                      | otherwise                                                    = error "incompatible layer"

layerElems :: NetworkLayer a -> Layer a
layerElems (InLayer x) = x
layerElems (OutLayer x) = x
layerElems (HiddenLayer x) = x

instance Show a => Show (NetworkLayer a) where
    show (InLayer x)     = "InLayer" ++ show x
    show (OutLayer x)    = "OutLayer" ++ show x
    show (HiddenLayer x) = "HiddenLayer" ++ show x

--instance Functor NetworkLayer where
--    fmap f (In elems next) = In (f elems) next


--  -- Network Elements Connections
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

data Synapse = Synapse { from :: ElemId, to :: ElemId }
    deriving (Eq, Show)


--  -- The Network
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

data Network a = Network [NetworkLayer a] [Synapse]
    deriving Show
