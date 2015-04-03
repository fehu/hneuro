 {-# LANGUAGE OverloadedStrings #-}

module Neuro.GenTikz
( genTikz
, (~>)
, defs
, genStruct, genConnections

, tikzScope
)
where

import Data.List
import Data.Map(keys)

import Neuro.DSL

genTikz :: DSLResolved -> String -- TODO ?String?

a ~> b = (a, b)

type TikzParams = [TikzParam]
type TikzParam = (String, String)

genLayer  :: Int -> DSLLayer -> TikzParams -> (Int -> TikzParams) -> String
genStruct :: [DSLLayer] -> String
genConnections :: [DSLConnection] -> TikzParams -> String

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

tikzId i = "(" ++ showId i ++ ")"
showId (l,n) = show l ++ "-" ++ show n

nodeDistMm mm = "node distance" ~> (show mm ++ "mm")

-- -- -- --

firstElemId = (0, 1)

defs = ["\\tikzset{chained/.style={circle,draw, font=\\small}}"]

genLayerDefaultScopeParams n = [ "start chain" ~> (show n ++ " going below") ]

genLayerDefaultNodeParams n  = [ "on chain" ~> show n
                               , "chained" ~> ""
                               ]

genLayer i l p np = tikzScope "scope" body mp
                  where ids = keys $ iElems l
                        n  = length ids
                        body = intercalate "\n" $
                               [tikzNode id t $ mnp c | (a, c) <- zip ids [1..]
                                                    , let id = show (fst a) ++ "-" ++ show (snd a)
                                                    , let t  = (show i ++ "-" ++ show c)]
                        mp    = genLayerDefaultScopeParams i ++ p
                        mnp j = genLayerDefaultNodeParams i ++ np j

genStructInner []     _ _  _ _  = []
genStructInner (i:xs) p np ls ln = tl:(genStructInner xs p np ls h)
                               where l  = ls !! i
                                     tl = genLayer i l p $ np ln
                                     h  = head $ keys $ iElems l

genStruct ls = intercalate "\n" $ l0:ll
             where l0 = genLayer 0 (ls !! 0) [nodeDistMm 2] (\_ -> [])
                   ll = genStructInner [1..n-1] p np ls firstElemId
                      where n = length ls
                            p = [nodeDistMm 2]
                            np ln 1 = ["right" ~> ("1cm of " ++ showId ln)]
                            np  _ _ = []

genConnections cs p = intercalate "\n" $ map f cs
                    where f = \c -> tikzDraw p [(tikzId $ from c) ++ " -- " ++ (tikzId $ to c)]

genTikz dsl = intercalate "\n\n" ( defs ++ [tstr, tconn] )
               where tstr = genStruct $ layers dsl
                     tconn = genConnections (connections dsl) []
