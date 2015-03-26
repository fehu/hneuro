 {-# LANGUAGE OverloadedStrings #-}

module Neuro.GenTikz (
--genTikz
 genLayer
)
where

import Data.List
--import Data.Text.Lazy
--import Data.Text.Format

import Neuro.DSL

--genTikz :: NeuroNet -> String -- TODO ?String?

a ~> b = (a, b)

type GenLayerParams = [GenLayerParam]
data GenLayerParam = RelOf { rel :: String
                           , dir :: String
                           , dist :: Maybe String
                           }
                   | TBD

genLayer :: Int -> IdentifiedLayer -> GenLayerParams -> String

tikzBegin x = "\\begin{" ++ x ++ "}"
tikzEnd   x = "\\end{" ++ x ++ "}"
tikzForeach param _in expr = "\\foreach " ++ param ++ " in " ++ _in ++ " { \n" ++ expr ++ " \n}"

--layerFormat :: Format
--layerFormat = "{1}[start chain={0} going below, node distance=2mm]\n"

genLayer n xs p = (tikzBegin "scope") ++ mkScopeParams ++ "\n" ++
--                        "[start chain=" ++ show n ++ " going below, node distance=2mm]\n" ++
                  (tikzForeach "\\x" "{1,...,5}" node) ++
                  (tikzEnd "scope")
                where scopeParams = ["start chain" ~> (show n ++ " going below"),
                                     "node distance" ~> "2mm"
                                    ]
                      mkScopeParams = "[" ++ (foldr1 (++) $ intersperse ", " $
                                                map (\(k,v) -> k ++ "=" ++ v) scopeParams) ++
                                      "]"
                      --(\(x,acc) -> acc ++ ", " ++ x)
                      node = "\\node[on chain=" ++ show n ++ " , chained] " ++
                             "(" ++ show n ++ "-\\x){" ++ show n ++ "-\\x};"

--genLayer n xs p = format "{}[start chain={0} going below, node distance=2mm]" [show "a"]
--                where s = "{1}[start chain={0} going below, node distance=2mm]\n" ++
--                          ""

--    \\foreach \\x in {{1,...,5}} {{ \\node[on chain={0} , chained] ({0}-\\x){{{0}-\\x}}; }}\
--    scope


--                where s = "\
--    \\begin{scope}[start chain=%d going below, node distance=2mm]\
--        \\foreach \\x in {1,...,5} { \\node[on chain=%d , chained] (%d-\\x){%d-\\x}; }\
--    \end{scope}\n"
