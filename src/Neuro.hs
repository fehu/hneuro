module Neuro
( NetworkElem  --(..)
, newNeuron, newInput, newOutput, newDelaySink, newDelayedInput
,  isNeuron,  isInput,  isOutput,  isDelaySink,  isDelayedInput
, Layer
, isInLayer, isHiddenLayer, isOutLayer
, newLayer, isolatedLayer
, NetworkLayer --(..)
, newNetworkLayer
--, test
) where

import NamedFunc
import Data.Map (Map, keys, elems, fromList)
import Data.Set (Set)

data NetworkElem a = Neuron {weights :: [a], transfer :: NamedFunc ([a] -> a)}
                   | Input a
                   | Output a
                   | DelaySink a Int [NetworkElem a]
                   | DelayedInput Int
                   deriving Eq

newNeuron w s f         = Neuron w $ f `named` s
newInput next_value     = Input next_value
newDelayedInput x       = DelayedInput x
newOutput out           = Output out
newDelaySink x d ixs    = if all isDelayedInput ixs
                          then DelaySink x d ixs
                          else error "DelaySink may be connected only to DelayedInput"

isNeuron (Neuron _ _)           = True
isNeuron _                      = False

isInput (Input _)               = True
isInput _                       = False

isOutput (Output _)             = True
isOutput _                      = False

isDelayedInput (DelayedInput _) = True
isDelayedInput _                = False

isDelaySink (DelaySink _ _ _)   = True
isDelaySink _                   = False


--isDelay = isDelayedInput || isDelaySink

instance Show a => Show (NetworkElem a) where
    show (Neuron {weights = w, transfer = f}) = "Neuron(" ++ show w ++ ", " ++ show f ++ ")"
    show (Input x)                  = "in "  ++ show x
    show (Output x)                 = "out " ++ show x
    show (DelaySink x delay link)   = show x ++ " -- delay " ++ show delay ++ " -->" ++ show link
    show (DelayedInput x)           = "delayed" ++ show x

instance Eq a => Ord (NetworkElem a) where
    (Neuron w1 f1) `compare` (Neuron w2 f2) = LT
--    TODO


-- --
type LayerElemKey a = (Int, NetworkElem a)

--instance Eq (Int, NetworkElem a) where (i1, _) == (i2, _) = i1 == i2

type Layer a = Map (LayerElemKey a) [NetworkLayer a]

newLayer :: Eq a => [(NetworkElem a, [NetworkLayer a])] -> Layer a
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
--noNext :: Layer a -> Bool
noNext layer = all null $ elems layer

newNetworkLayer :: Layer a -> NetworkLayer a
newNetworkLayer layer | layer `compatible` isInput     = InLayer layer
                      | layer `compatible` isNeuron    = HiddenLayer layer
                      | layer `compatible` isOutput
                                   && noNext layer     = OutLayer layer

instance Show a => Show (NetworkLayer a) where
    show (InLayer x)     = "InLayer" ++ show x
    show (OutLayer x)    = "OutLayer" ++ show x
    show (HiddenLayer x) = "HiddenLayer" ++ show x

layerElems :: NetworkLayer a -> Layer a
layerElems (InLayer x) = x
layerElems (OutLayer x) = x
layerElems (HiddenLayer x) = x


--instance Functor NetworkLayer where
--    fmap f (In elems next) = In (f elems) next

-- --

out = newNetworkLayer $ isolatedLayer (replicate 5 $ newOutput 0)
test = do
    putStrLn $ show $ newNeuron [1,2] "foo" head
    putStrLn $ show out
