 {-# LANGUAGE OverloadedStrings #-}

module Neuro.GenTikz (
--genTikz
 genLayer
, (~>)
, defs

, genStruct, genConnections

, tikzScope
)
where

import Data.List
import Data.Map(keys)

import Neuro.DSL

--genTikz :: NeuroNet -> String -- TODO ?String?

a ~> b = (a, b)

type TikzParams = [TikzParam]
type TikzParam = (String, String)
--data TikzParam = RelOf { rel :: String
--                           , dir :: String
--                           , dist :: Maybe String
--                           }
--                   | TBD

genLayer  :: Int -> IdentifiedLayer -> TikzParams -> (Int -> TikzParams) -> String
genStruct :: IdentifiedNeuroNetStruct -> String
genConnections :: HardConnections -> TikzParams -> String

-- -- TikZ
tikzBegin x = "\\begin{" ++ x ++ "}"
tikzEnd   x = "\\end{" ++ x ++ "}"

tikzForeach param _in expr = "\\foreach " ++ param ++ " in " ++ _in ++ " { " ++ expr ++ " }"

tikzParams_i (k, "") = k
tikzParams_i (k, v)  = k ++ "=" ++ v

tikzParams p = "[" ++ (foldr (++) "" $ intersperse ", " $ map tikzParams_i p) ++ "]"

tikzScope name body p = tikzBegin name ++ tikzParams p ++ "\n" ++ body ++ "\n" ++ tikzEnd name

tikzNode id body p = "\\node" ++ tikzParams p ++ "(" ++ id ++ "){" ++ body ++ "};"

tikzDraw p body = "\\draw" ++ tikzParams p ++ "\n\t" ++ (intercalate "\n\t" body) ++ ";"

tikzId i = "(" ++ show i ++ ")"

nodeDistMm mm = "node distance" ~> (show mm ++ "mm")

-- -- -- --

defs = ["\\tikzset{chained/.style={circle,draw, font=\\small}}"]

genLayerDefaultScopeParams n = [ "start chain" ~> (show n ++ " going below") ]

genLayerDefaultNodeParams n  = [ "on chain" ~> show n
                               , "chained" ~> ""
                               ]

genLayer i l p np = tikzScope "scope" body mp
                  where ids = keys l
                        n  = length ids
                        body = intercalate "\n" $
                               [tikzNode id t $ mnp c | (a, c) <- zip ids [1..]
                                                    , let id = show a
                                                    , let t  = (show i ++ "-" ++ show c)]
                        mp    = genLayerDefaultScopeParams i ++ p
                        mnp j = genLayerDefaultNodeParams i ++ np j

genStructInner []     _ _  _ _  = []
genStructInner (i:xs) p np s ln = tl:(genStructInner xs p np s h)
                              where l  = getILayer i s
                                    tl = genLayer i l p $ np ln
                                    h  = head $ keys l

genStruct s = intercalate "\n" $ l0:ll
            where l0 = genLayer 0 (getILayer 0 s) [nodeDistMm 2] (\_ -> [])
                  ll = genStructInner [1..n-1] p np s 0
                     where j = joinNetStruct s
                           n = length j
                           p = [nodeDistMm 2]
                           np ln 1 = ["right" ~> ("1cm of " ++ show ln)]
                           np  _ _ = []

--genStruct s = intercalate "\n" $ l0:ll
--            where l0 = genLayer 0 (getILayer 0 s) [nodeDistMm 2] []
--                  ll = [genLayer i l p $ np l | i <- [1..n-1], let l = getILayer i s]
--                     where j = joinNetStruct s
--                           n = length j
--                           p     = [nodeDistMm 2]
--                           np id = ["right" ~> ("1cm of " ++ show (i-1) ++ "-\\x")]

genConnections c p = intercalate "\n" $ map f $ getConnectionIds c
                   where f = \(x, y) -> tikzDraw p [tikzId x ++ " -- " ++ tikzId y]
