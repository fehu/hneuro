module Neuro.DSL
( Elem(..)
, Layer(..)
, Connections(..)
, (+:+)
, Id, LastId, IdentifiedLayer
, getILayer, joinNetStruct
, ElemSel
, sel, sel'
, Link
, (-->), all2all
, linkId
, HardConnections(..)
, getConnections, getConnectionIds
, ANeuroNetStruct(..), NeuroNetStruct, IdentifiedNeuroNetStruct
, neuroNetStructure, identifyNeuroNetStruct
, resolveConnections
, NeuroNet
) where

import Data.Map (Map, fromList, assocs)
import Data.Foldable (toList)

flatMap f = concatMap (Data.Foldable.toList . f)

--  -- DSL Elements
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

data Elem = Neuron | In | Out | Delayed Int | Delay
    deriving (Eq, Show)

data Layer = Layer [Elem] deriving (Eq, Show)

data Connections = Connections [Link] deriving Show
(+:+) :: Connections -> Connections -> Connections

data ElemSel = Sel {layer :: Int, i :: [Int]}
             deriving (Eq, Show)

sel  :: Int -> Int   -> ElemSel
sel' :: Int -> [Int] -> ElemSel

type Id = (Int, Int)
type LastId = Id

data Link = HardLink (Id, Elem) (Id, Elem)
          | WeakLink ElemSel ElemSel
          deriving (Eq, Show)

(-->)   :: ElemSel -> ElemSel -> Link
all2all :: IdentifiedNeuroNetStruct -> Int       -> Int     -> [Int]      -> [Int] -> Connections

linkId  :: Link -> Maybe (Id, Id)

type IdentifiedLayer = Map Id Elem
getILayer :: Int -> IdentifiedNeuroNetStruct -> IdentifiedLayer


data ANeuroNetStruct a = ANeuroNetStruct { inputs :: a
                                         , hidden :: [a]
                                         , outputs :: a
                       } deriving Show

joinNetStruct :: ANeuroNetStruct a -> [a]

type NeuroNetStruct = ANeuroNetStruct Layer

neuroNetStructure :: Layer -> [Layer] -> Layer -> NeuroNetStruct

type IdentifiedNeuroNetStruct = ANeuroNetStruct IdentifiedLayer
identifyNeuroNetStruct :: NeuroNetStruct -> IdentifiedNeuroNetStruct

data HardConnections = HardConnections [Link]
getConnections   :: HardConnections -> [((Id, Elem), (Id, Elem))]
getConnectionIds :: HardConnections -> [(Id, Id)]

resolveConnections :: IdentifiedNeuroNetStruct ->  Connections -> HardConnections

type NeuroNet = (IdentifiedNeuroNetStruct, HardConnections)


--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

shiftOneIndexing i = i - 1

getLayer (Layer xs) = xs

getILayer 0 (ANeuroNetStruct inp _ _)   = inp
getILayer i (ANeuroNetStruct _ hid out) = if i <= length hid
                                          then hid !! (shiftOneIndexing i)
                                          else if i == 1 + length hid
                                               then out
                                          else error "out of range"

getConnections (HardConnections l) = map f l
                                   where f x = case x of (HardLink from to) -> (from, to)

getConnectionIds c = map (\((i1,_), (i2,_)) -> (i1, i2)) $ getConnections c

(Connections c1) +:+ (Connections c2) = Connections (c1 ++ c2)

l `sel`  i  = Sel l [i]
l `sel'` is = Sel l is

from --> to = WeakLink from to

all2all inet from to exceptFrom exceptTo = Connections [HardLink a b | a <- fFrom, b <- fTo]
                                         where fFrom = fb exceptFrom $ assocs $ getILayer from inet
                                               fTo   = fb exceptTo   $ assocs $ getILayer to inet
                                               ff e  = \(_,x) -> not $ x `elem` e
                                               fb e l = map fst $ filter (ff e) $ zip l [1..]

linkId link = case link of (HardLink (from, _) (to, _)) -> Just (from, to)
                           _                            -> Nothing

joinNetStruct (ANeuroNetStruct i h o) = [i] ++ h ++ [o] -- reverse $

neuroNetStructure inp hid out = ANeuroNetStruct inp hid out

identifyNeuroNetStruct nnet = ANeuroNetStruct (head l) ((init . tail) l) (last l)
                        where j = map getLayer $ joinNetStruct nnet
                              l = map identify $ zip j [0..]
                              identify (layer, k) = fromList ll
                                                  where ll = [ (id, e) | (e, n) <- zip layer [1..]
                                                             , let id = (k, n)
                                                             ]
isDelay Delay = True
isDelay _     = False

resolveConnections inet (Connections links) = if all testConn conn
                                              then HardConnections conn
                                              else error "back referencing non-delay synapses present"
                               where f link = case link of h@(HardLink _ _) -> [h]
                                                           w@(WeakLink _ _) -> convertWeak inet w
                                     conn   = flatMap f links
                                     testConn (HardLink (from, e) (to, _)) = isDelay e || to > from

hsel inet l n = (assocs $ getILayer l inet) !! shiftOneIndexing n

convertWeak inet (WeakLink (Sel fromL fromN) (Sel toL toN)) =
    [ HardLink (hsel inet fromL from) (hsel inet toL to) | from <- fromN, to <- toN ]

    --          -- --          -- --          -- --          -- --          -- --          --
    --          -- --          -- --          -- --          -- --          -- --          --
