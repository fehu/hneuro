module Neuro.DSL
( Elem(..)
, Layer(..)
, Connections(..)
, (+:+)
, Id, LastId, IdentifiedLayer
, zipId, zzipId, getILayer, joinNetStruct
, ElemSel
, sel, sel'
, Link
, (-->), all2all
, HardConnections
, getConnections, getConnectionIds
, ANeuroNetStruct(..), NeuroNetStruct, IdentifiedNeuroNetStruct
, neuroNetStructure, identifyNeuroNetStruct
, resolveConnections
--, test
) where

import Data.Map (Map, fromList, assocs)
import Data.Foldable (toList)

flatMap f = concatMap (Data.Foldable.toList . f)

--  -- DSL Elements
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

data Elem = Neuron | In | Out | Delayed Int | Delay Int
    deriving (Eq, Show)

data Layer = Layer [Elem] deriving (Eq, Show)

data Connections = Connections [Link] deriving Show
(+:+) :: Connections -> Connections -> Connections

data ElemSel = Sel {layer :: Int, i :: [Int]}
             deriving (Eq, Show)

sel  :: Int -> Int   -> ElemSel
sel' :: Int -> [Int] -> ElemSel

type Id = Int
type LastId = Id

type ZippedId a = [(Id, a)]
type ZuppedIdL a = (ZippedId a, LastId)

data Link = HardLink (Id, Elem) (Id, Elem)
          | WeakLink ElemSel ElemSel
          deriving (Eq, Show)

(-->)   :: ElemSel -> ElemSel -> Link
all2all :: IdentifiedNeuroNetStruct -> Int       -> Int     -> [Int]      -> [Int] -> Connections


type IdentifiedLayer = Map Id Elem
getILayer :: Int -> IdentifiedNeuroNetStruct -> IdentifiedLayer


data ANeuroNetStruct a = ANeuroNetStruct { inputs :: a
                                         , hidden :: [a]
                                         , outputs :: a
                       } deriving Show


type NeuroNetStruct = ANeuroNetStruct Layer

neuroNetStructure :: Layer -> [Layer] -> Layer -> NeuroNetStruct

zipId  :: LastId ->  [a]  -> ZuppedIdL a
zzipId :: LastId -> [[a]] -> ([ZippedId a], LastId)

type IdentifiedNeuroNetStruct = ANeuroNetStruct IdentifiedLayer
identifyNeuroNetStruct :: LastId -> NeuroNetStruct -> (IdentifiedNeuroNetStruct, LastId)

data HardConnections = HardConnections [Link]
getConnections   :: HardConnections -> [((Id, Elem), (Id, Elem))]
getConnectionIds :: HardConnections -> [(Id, Id)]

resolveConnections :: IdentifiedNeuroNetStruct ->  Connections -> HardConnections

type NeuroNet = (IdentifiedNeuroNetStruct, HardConnections)


--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

getLayer (Layer xs) = xs

getILayer 0 (ANeuroNetStruct inp _ _)   = inp
getILayer i (ANeuroNetStruct _ hid out) = if i <= length hid
                                          then hid !! (i-1)
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

joinNetStruct (ANeuroNetStruct i h o) = reverse $ [i] ++ h ++ [o]

neuroNetStructure inp hid out = ANeuroNetStruct inp hid out

zipId i xs = (zipped, i+len+1)
           where len = length xs
                 zipped = zip [i..] xs

zzipId i xss = foldr f ([], i) xss
             where f xs (ixs, ii) = (ixs ++ [fst r], snd r)
                                  where r = zipId ii xs

identifyNeuroNetStruct i nnet = (anet, ni)
                              where anet = ANeuroNetStruct (head l) ((init . tail) l) (last l)
                                    j   = joinNetStruct nnet
                                    zzipped = zzipId i $ map getLayer j
                                    l   = map fromList $ fst zzipped
                                    ni  = snd zzipped

resolveConnections inet (Connections links) = HardConnections $ flatMap f links
                               where f link = case link of h@(HardLink _ _) -> [h]
                                                           w@(WeakLink _ _) -> convertWeak inet w

shiftOneIndexing i = i - 1

hsel inet l n = (assocs $ getILayer l inet) !! shiftOneIndexing n

convertWeak inet (WeakLink (Sel fromL fromN) (Sel toL toN)) =
    [ HardLink (hsel inet fromL from) (hsel inet toL to) | from <- fromN, to <- toN ]

    --          -- --          -- --          -- --          -- --          -- --          --
    --          -- --          -- --          -- --          -- --          -- --          --
