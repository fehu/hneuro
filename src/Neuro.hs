module Neuro
( NetworkElem
, newNeuron, newInput, newOutput, newDelaySink, newDelayedInput
,  isNeuron,  isInput,  isOutput,  isDelaySink,  isDelayedInput
, Layer
, isInLayer, isHiddenLayer, isOutLayer
, newLayer, isolatedLayer
, NetworkLayer
, newNetworkLayer
--, test
) where

import NamedFunc
import Data.Map (Map, keys, elems, fromList)
import Data.Set (Set)

--  -- Network Elements
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

type ElemId = Int

data NetworkElem a = Neuron {id :: ElemId, weights :: [a], transfer :: NamedFunc ([a] -> a)}
                   | Input         ElemId a
                   | Output        ElemId a
                   | DelaySink     ElemId a [NetworkElem a]
                   | DelayedInput  ElemId Int [a]
                   deriving Eq

newNeuron id w s f      = Neuron id w $ f `named` s
newInput id x           = Input id x
newOutput id x          = Output id x
newDelayedInput id d xs = DelayedInput id d xs
newDelaySink id x links = if all isDelayedInput links
                          then DelaySink id x links
                          else error "DelaySink may be connected only to DelayedInput"

isNeuron (Neuron _ _ _)             = True
isNeuron _                          = False

isInput (Input _ _)                 = True
isInput _                           = False

isOutput (Output _ _)               = True
isOutput _                          = False

isDelayedInput (DelayedInput _ _ _) = True
isDelayedInput _                    = False

isDelaySink (DelaySink _ _ _)       = True
isDelaySink _                       = False

instance Show a => Show (NetworkElem a) where
    show (Neuron id w f)            = "neuron:" ++ show id ++ "(" ++ show w ++ ", " ++ show f ++ ")"
    show (Input id x)               = "in:"     ++ show id ++ "=" ++ show x
    show (Output id x)              = "out:"    ++ show id ++ "=" ++ show x
    show (DelaySink id x link)      = "delay:"  ++ show id ++ "=" ++ show x ++ "-->" ++ show link
    show (DelayedInput id d xs)     = "delayed:" ++ show id ++ "=" ++ show (head xs)

instance Eq a => Ord (NetworkElem a) where
    (Neuron id1 _ _)        `compare` (Neuron id2 _ _)       = id1 `compare` id2
    (Input id1 _)           `compare` (Input id2 _)          = id1 `compare` id2
    (Output id1 _)          `compare` (Output id2 _)         = id1 `compare` id2
    (DelaySink id1 _ _)     `compare` (DelaySink id2 _ _)    = id1 `compare` id2
    (DelayedInput id1 _ _)  `compare` (DelayedInput id2 _ _) = id1 `compare` id2




--  -- Network Layers
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

--instance Eq (Int, NetworkElem a) where (i1, _) == (i2, _) = i1 == i2

type Layer a = Map (ElemId, NetworkElem a) [NetworkElem a]

newLayer :: Eq a => [(NetworkElem a, [NetworkElem a])] -> Layer a
newLayer dict = fromList $ map (\((k,v),i) -> ((i,k), v)) $ zip dict [1..]

isolatedLayer :: Eq a => [NetworkElem a] -> Layer a
isolatedLayer l = newLayer $ zip l $ repeat []

data NetworkLayer a = InLayer     (Layer a)
                    | HiddenLayer (Layer a)
                    | OutLayer    (Layer a)

isInLayer (InLayer _)           = True
isInLayer _                     = False

isHiddenLayer (HiddenLayer _)   = True
isHiddenLayer _                 = False

isOutLayer (OutLayer _)         = True
isOutLayer _                    = False

compatible :: Layer a -> (NetworkElem a -> Bool) -> Bool
layer `compatible` f = all tstf (map snd $ keys layer)
                     where tstf = \k -> foldr (\f a -> f(k) || a) False tst
                           tst  = [f, isDelayedInput, isDelaySink]

noNext layer = all null $ elems layer

newNetworkLayer :: Layer a -> NetworkLayer a
newNetworkLayer layer | layer `compatible` isInput     = InLayer layer
                      | layer `compatible` isNeuron    = HiddenLayer layer
                      | layer `compatible` isOutput
                                   && noNext layer     = OutLayer layer

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

-- --

--out = newNetworkLayer $ isolatedLayer (replicate 5 $ newOutput 0)
--test = do
--    putStrLn $ show $ newNeuron [1,2] "foo" head
--    putStrLn $ show out




--  -- Network Elements Connections
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

data Synapse a = Synapse { from :: NetworkElem a, to :: NetworkElem a }



