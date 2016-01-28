-----------------------------------------------------------------------------
--
-- Module      :  Neuro.Show.TikzOld
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, TypeOperators, FlexibleInstances, FlexibleContexts #-}

module Neuro.Show.TikzOld (

) where

import Neuro.Show
import Neuro.DSL
import Neuro.DSL.Internal

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List



data Tikz = Tikz ByteString

tikz (Tikz t) = t

instance NShow (NNDescriptor h) Tikz where
--    nShow (NNDescriptor layers) = Tikz . latexDocSurround $ BS.concat [
--        tikzDefs
--      , tikzPicture $ BS.concat [
--            tikzStyles
--          ]
--      ]
--
--instance (NShow (HList rest) Tikz) => NShow (HList (NLayer n ': rest)) Tikz where
--    nShow (HCons layer htail) = Tikz $ BS.concat [BS.pack "\n", tikz $ nShow layer, tikz $ nShow htail]
--
--instance (NShow (HList '[]) Tikz) => NShow (HList (NLayer n ': '[])) Tikz where
--    nShow (HCons layer HNil) = Tikz $ BS.concat [BS.pack "\n", tikz $ nShow layer]

--instance (NShow (HList '[]) Tikz) => NShow (HList (NLayer n ': rest)) Tikz where
--    nShow HNil                = Tikz BS.empty


--instance NShow (NLayer n) Tikz where
--    nShow l = Tikz . BS.concat . map (tikz . nShow) $ vec2list l

-- \node[output neuron,pin={[pin edge={->}]right:Output}, right of=H-3] (O) {};
mkNode params at body = BS.concat $ map BS.pack ["\\node", params', at', body]
                    where params' = case params of [] -> ""
                                                   _  -> "[" ++ List.intercalate "," params ++ "]"
                          at' = case at of Just pos -> pos
                                           _        -> "(0)"

--instance NShow NElem Tikz where
--    nShow NInput = Tikz $ mkNode []


--latexDocSurround body = BS.concat
--    [ BS.pack  "\\documentclass{article}\n\
--               \\\usepackage{tikz}\n\
--               \\\begin{document}\n\
--               \\\pagestyle{empty}\n"
--   , body
--   , BS.pack "\n\\end{document}"
--   ]
--

--tikzDefs = BS.pack "\n\\def\\layersep{2.5cm}\n"
--
--tikzPicture body = BS.unwords
--    [ BS.pack "\\begin{tikzpicture}[shorten >=1pt,->,draw=black!50, node distance=\\layersep]"
--    , BS.pack "\\end{tikzpicture}"
--    ]
--
--tikzStyles = BS.unwords $ map BS.pack [
--    "\\tikzstyle{every pin edge}=[<-,shorten <=1pt];"
--  , "\\tikzstyle{neuron}=[circle,fill=black!25,minimum size=17pt,inner sep=0pt];"
--  , "\\tikzstyle{input neuron}=[neuron, fill=green!50];"
--  , "\\tikzstyle{output neuron}=[neuron, fill=red!50];"
--  , "\\tikzstyle{hidden neuron}=[neuron, fill=blue!50];"
--  , "\\tikzstyle{annot} = [text width=4em, text centered];"
--  ]




