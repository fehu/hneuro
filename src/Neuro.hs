module Neuro
( ElemId(..)
, Neuron(..)
, DelayedInput(..)
, neuronIdLayer, neuronIdPos, neuronIdPair
, neuronIdFromPair
, newNeuron, newDelayedInput
, updateNeuronWeights
, updateDelayedInput

, Layer
, newLayer
, Synapse(..)
, Synapses
, SynapsesCache ( backloops )
, mkSynapsesCache, getFrom, getTo
, Network(..)
) where

import NamedFunc
import Data.Map (Map, keys, elems, fromList, (!), fromListWith, filterWithKey, mapWithKey)
import Data.Set (Set)

--  -- Network Elements
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

data ElemId = NeuronId Int Int
            | DelayedId Int
    deriving (Show)

neuronIdLayer (NeuronId l _)  = l
neuronIdPos   (NeuronId _ n)  = n
neuronIdPair  (NeuronId l n)  = (l, n)

neuronIdFromPair (l,n) = NeuronId l n

newNeuron id w wf f = Neuron (neuronIdFromPair id) w wf f

updateNeuronWeights (Neuron id w wf tf)     f = Neuron id (f w) wf tf

data Neuron a = Neuron { neuronId :: ElemId
                       , weights :: [a]
                       , applyW   :: NamedFunc a  (a -> a)
                       , transfer :: NamedFunc [a] a
                       }
                   deriving Show

data DelayedInput a = DelayedInput { delayedId :: ElemId
                                   , delay :: Int
                                   , delayedValues :: [a]
                                   }
                   deriving Show

newDelayedInput id d xs = DelayedInput (neuronIdFromPair id) d xs

updateDelayedInput  (DelayedInput id xs d)  f = DelayedInput id (f xs) d

instance Eq (ElemId) where
    DelayedId a == DelayedId b  = a == b
    a == b                      = neuronIdPair a == neuronIdPair b

instance Ord (ElemId) where
    DelayedId a `compare` DelayedId b  = a `compare` b
    a `compare` b                      = neuronIdPair a `compare` neuronIdPair b

--instance Show a => Show (NetworkElem a) where
--    show (Neuron id w wf f)         = show id ++ "(" ++ show wf ++ " " ++ show w ++ ", "
--                                                                  ++ show f ++ ")"
--    show (Input id x)               = show id ++ "=" ++ show x
--    show (Output id x)              = show id ++ "=" ++ show x
--    show (DelayedInput id d xs)     = show id ++ "=" ++ show xs

instance Eq (Neuron a) where
    Neuron {neuronId = i1} == Neuron {neuronId = i2} = i1 == i2

instance Eq (DelayedInput a) where
    DelayedInput {delayedId = i1} == DelayedInput {delayedId = i2} = i1 == i2

--instance Eq a => Ord (NetworkElem a) where
--    a `compare` b = getId a `compare` getId b





--  -- Network Layers
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

type Layer a = Map ElemId (Neuron a)

newLayer :: Eq a => [Neuron a] -> Layer a
newLayer xs = if not $ null xs then fromList $ map (\x -> (neuronId x, x)) xs
                               else error "empty new layer"

--  -- Network Elements Connections
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

data Synapse = Synapse { from :: ElemId, to :: ElemId}
    deriving (Eq, Show)

--newSynapse :: ElemId a -> ElemId a -> Synapse a

type Synapses = [Synapse]
data SynapsesCache = SynapsesCache { fromMap   :: Map ElemId [ElemId]
                                   , toMap     :: Map ElemId [ElemId]
                                   , backloops :: Map ElemId [ElemId] }

mkSynapsesCache :: Synapses -> SynapsesCache

--newSynapse from to = if (idLayer $ getId from) < (idLayer $ getId to) && (isNeuron to || isOutput to)
--                                                                      && (not $ isOutput from)
--                     then Synapse from to
--                     else error "backgoing synapse"

mkSynapsesCache syns = SynapsesCache{ fromMap   = fromListWith (++) pars
                                    , toMap     = fromListWith (++) $ reverse pars
                                    , backloops = fromList $ foldr (++) [] $ filter (not . null) $ map f pars
                                    }
                     where pars         = map (\(Synapse from to) -> (from, [to])) syns
                           f (from, to) = map (\x -> (from, to))
                                            $ filter (\t -> neuronIdLayer from <= neuronIdLayer t) to

getTo   SynapsesCache {toMap   = m} id = m ! id
getFrom SynapsesCache {fromMap = m} id = m ! id

--  -- The Network
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

data Network a = Network { layers :: [Layer a]
                         , delayed :: [DelayedInput a]
                         , synapses :: Synapses }
    deriving Show



