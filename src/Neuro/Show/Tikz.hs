-----------------------------------------------------------------------------
--
-- Module      :  Neuro.Show.Tikz
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE TypeOperators #-}

module Neuro.Show.Tikz (

  NShow(..)

, Tikz(..)

, RenderConfig(..)
, nnet2tikz

, defaultRenderConfig

) where

import Neuro.Show
import Neuro.DSL
import Neuro.DSL.Internal

import Tikz.DSL

import Data.HList
import Data.ByteString.Char8 (ByteString)
--import qualified Data.ByteString.Char8 as BS

--import Control.Applicative( (<*>) )

-----------------------------------------------------------------------------

newtype Tikz = Tikz ByteString


instance NShow NNDescriptor Tikz where nShow = nnet2tikz defaultRenderConfig

nnet2tikz cfg (NNDescriptor layers) = Tikz . elemRepr 0
                           $ picture (pictureAttrs cfg)
                                     (pictureStyles cfg)
                                     (pictureDefs cfg)
                                     (renderLayers cfg layers)

--                            where body =
--                            [renderLayer, renderSynopses] <*> [cfg]
--                                        <*> zipWith (mkALayer cfg) [1..] layers

data RenderConfig = RenderConfig { pictureAttrs   :: PictureAttrs
                                 , pictureStyles  :: PictureStyles
                                 , pictureDefs    :: PictureDefs
                                 , renderLayers   :: [SomeLayer] -> [Expr]
                                 }


nElemId :: NElem -> String
nElemId (NInput i) = "0-" ++ show i
nElemId (Neuron _ l i) = show l ++ "-" ++ show i

-----------------------------------------------------------------------------

defaultRenderConfig = RenderConfig defaultPicAttrs
                                   defaultPicStyles
                                   defaultPicDefs
                                   defaultRenderLayers

defaultPicAttrs  = PictureAttrs [ Attr "shorten >=1pt"
                                , Attr "->"
                                , Attr "draw=black!50"
                                ]
defaultPicStyles = PictureStyles [ Style "every pin edge" [ Attr "<-"
                                                          , Attr "shorten <=1pt"
                                                          ]
                                 , Style "neuron" [ Attr "circle"
                                                  , Attr "fill=black!25"
                                                  , Attr "minimum size=17pt"
                                                  , Attr "inner sep=0pt"
                                                  ]
                                 , Style "input neuron"  [Attr "neuron", Attr "fill=green!50"]
                                 , Style "output neuron" [Attr "neuron", Attr "fill=red!50"]
                                 , Style "hidden neuron" [Attr "neuron", Attr "fill=blue!50"]
                                 , Style "annot" [ Attr "text width=4em"
                                                 , Attr "text centered"
                                                 , Attr "node distance=1cm"
                                                 ]
                                 ]

defaultPicDefs = PictureDefs [ Def "layersep" "2.5cm" ]

layerAnnot (e:_) name =
      Expr ( Node "input layer"
                  [Attr "annot", Attr $ "above of=" ++ nElemId e]
                  Nothing
                  [Expr $ RawExpr name]
          )

defaultRenderLayers :: [SomeLayer] -> [Expr]
defaultRenderLayers = defaultRenderLayers' . zipWith f [0..] . reverse
    where f n (SomeLayer l) = (n, vec2list l)

defaultRenderLayers' ((0, l) : ls) = Expr EmptyLine : defaultRenderInputs l' ++ annot : defaultRenderLayers' ls
    where annot = layerAnnot l' "Input Layer"
          l' = reverse l

defaultRenderLayers' [(n, l)] = Expr EmptyLine : defaultRenderOutputs l ++ [annot]
    where annot = layerAnnot l "Output Layer"

defaultRenderLayers' ((n, l) : ls) = Expr EmptyLine : defaultRenderHidden l ++ annot : defaultRenderLayers' ls
    where annot = layerAnnot l $ "Hidden Layer \\#" ++ show n



mkNode l i name attrs = Node name attrs (Just $ "\\layersep*" ++ show l ++ ",-" ++ show i) []

defaultRenderInputs (n@(NInput i): ns) = Expr node : defaultRenderInputs ns
    where node = mkNode 0 i (nElemId n) [Attr "input neuron", Attr $ "pin=left:Input " ++ show i]
defaultRenderInputs [] = []


defaultRenderGeneric attrs (n@(Neuron inputs l i) : ns) =
    Expr node : synapses ++ defaultRenderGeneric attrs ns

    where node = mkNode l i (nElemId n) (attrs i)
          synapses = [] -- TODO

defaultRenderGeneric _ [] = []

defaultRenderHidden = defaultRenderGeneric (const [Attr "hidden neuron"])

defaultRenderOutputs =
    defaultRenderGeneric (\i -> [ Attr "output neuron"
                                , Attr $ "pin={[pin edge={->}]right:Output " ++ show i ++ "}"
                                ])

--defaultRenderSynopses :: ALayer -> [Expr]
--defaultRenderSynopses i (SomeLayer l) = defaultRenderSynopses' i $ zip [1..] $ vec2list l

defaultRenderSynopses' = undefined

--defaultRenderSynopses' l ((i, Neuron ) : ns) = Path Edge
--                                              ("")
--                                              ""

