module Neuro
( NetworkElem
, ElemId
, idLayer, idPos, idPair
, idFromPair
, getId
, newNeuron, newInput, newOutput, newDelayedInput
, isNeuron,  isInput,  isOutput,  isDelayedInput
, getWs, getTf, getWf
, updateNeuronWeights
, updateInput, updateDelayedInput, updateOutput
, Layer
, isInpLayer, isHiddenLayer, isOutLayer
, newLayer
, layerElems
, NetworkLayer
, newNetworkLayer
, Synapse
, newSynapse
, Synapses, SynapsesCache
, mkSynapsesCache, getFrom, getTo
, Network(..)
) where

import NamedFunc
import Data.Map (Map, keys, elems, fromList, (!), fromListWith)
import Data.Set (Set)

--  -- Network Elements
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

data ElemId = ElemId Int Int
    deriving (Show)

idLayer (ElemId l _)  = l
idPos   (ElemId _ n)  = n
idPair  (ElemId l n)  = (l, n)

idFromPair (l,n) = ElemId l n

-- Neuron       -- is defined as `transfer $ map applyW $ zip inputs weights`
-- Input        -- marks an input, holds the value
-- DelayedInput -- marks an input for delayed backloops; holds the delayed values
-- Output       -- marks an output, holds the value

data NetworkElem a = Neuron { id :: ElemId
                            , weights :: [a]
                            , applyW   :: NamedFunc a  (a -> a)
                            , transfer :: NamedFunc [a] a
                            }
                   | Input         ElemId  a
                   | DelayedInput  ElemId [a] Int
                   | Output        ElemId  a
                   deriving Eq

newNeuron id w wf f     = Neuron (idFromPair id) w wf f
newInput id x           = Input  (idFromPair id) x
newOutput id x          = Output (idFromPair id) x
newDelayedInput id d xs = DelayedInput (idFromPair id) xs d

isNeuron (Neuron _ _ _ _)           = True
isNeuron _                          = False

isInput (Input _ _)                 = True
isInput _                           = False

isOutput (Output _ _)               = True
isOutput _                          = False

isDelayedInput (DelayedInput _ _ _) = True
isDelayedInput _                    = False

getId (Neuron id _ _ _)     = id
getId (Input id _)          = id
getId (Output id _)         = id
getId (DelayedInput id _ _) = id

getWs Neuron {weights = w}    = w
getTf Neuron {transfer = tf}  = tf
getWf Neuron {applyW = wf}    = wf

updateNeuronWeights (Neuron id w wf tf)     f = Neuron id (f w) wf tf

updateInput         (Input id v)            f = Input  id $ f v
updateDelayedInput  (DelayedInput id xs d)  f = DelayedInput id (f xs) d

updateOutput        (Output id v)           f = Output id $ f v

instance Eq (ElemId) where
    a == b = idPair a == idPair b

instance Ord (ElemId) where
    a `compare` b = idPair a `compare` idPair b

instance Show a => Show (NetworkElem a) where
    show (Neuron id w wf f)         = show id ++ "(" ++ show wf ++ " " ++ show w ++ ", "
                                                                  ++ show f ++ ")"
    show (Input id x)               = show id ++ "=" ++ show x
    show (Output id x)              = show id ++ "=" ++ show x
    show (DelayedInput id d xs)     = show id ++ "=" ++ show xs

instance Eq a => Ord (NetworkElem a) where
    a `compare` b = getId a `compare` getId b





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

newNetworkLayer :: Layer a -> NetworkLayer a
newNetworkLayer layer | compatible layer [isInput]  [isDelayedInput]   = InLayer layer
                      | compatible layer [isNeuron, isDelayedInput ][] = HiddenLayer layer
                      | compatible layer [isOutput] []                 = OutLayer layer
                      | otherwise                                      = error "incompatible layer"

layerElems :: NetworkLayer a -> Layer a
layerElems (InLayer x) = x
layerElems (OutLayer x) = x
layerElems (HiddenLayer x) = x

instance Show a => Show (NetworkLayer a) where
    show (InLayer x)     = "InLayer" ++ show x
    show (OutLayer x)    = "OutLayer" ++ show x
    show (HiddenLayer x) = "HiddenLayer" ++ show x

--  -- Network Elements Connections
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

data Synapse a = Synapse { from :: NetworkElem a, to :: NetworkElem a }
    deriving (Eq, Show)

newSynapse :: NetworkElem a -> NetworkElem a -> Synapse a

type Synapses a = [Synapse a]
data SynapsesCache = SynapsesCache { fromMap :: (Map ElemId [ElemId])
                                   , toMap   :: (Map ElemId [ElemId]) }

mkSynapsesCache :: Synapses a -> SynapsesCache
getTo   :: SynapsesCache -> ElemId -> [ElemId]
getFrom :: SynapsesCache -> ElemId -> [ElemId]


newSynapse from to = if (idLayer $ getId from) < (idLayer $ getId to) && (isNeuron to || isOutput to)
                                                                      && (not $ isOutput from)
                     then Synapse from to
                     else error "backgoing synapse"

mkSynapsesCache syns = SynapsesCache{ fromMap = fromListWith (++)           pars
                                    , toMap   = fromListWith (++) $ reverse pars }
                     where pars = map (\(Synapse from to) -> (getId from, [getId to])) syns

getTo   SynapsesCache {toMap   = m} id = m ! id
getFrom SynapsesCache {fromMap = m} id = m ! id

--  -- The Network
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

data Network a = Network [NetworkLayer a] (Synapses a)
    deriving Show



