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

) where

import Neuro.Show
import Neuro.DSL
import Neuro.DSL.Internal

import Tikz.DSL

import Data.HList
import Data.ByteString.Char8 (ByteString)
--import qualified Data.ByteString.Char8 as BS

import Control.Applicative( (<*>) )

-----------------------------------------------------------------------------

newtype Tikz = Tikz ByteString


--instance NShow NNDescriptor Tikz where nShow = nnet2tikz defaultRenderConfig

nnet2tikz cfg (NNDescriptor layers) = Tikz . elemRepr 0
                           $ picture (pictureAttrs cfg)
                                    (pictureStyles cfg)
                                    (renderLayers cfg layers)

--                            where body =
--                            [renderLayer, renderSynopses] <*> [cfg]
--                                        <*> zipWith (mkALayer cfg) [1..] layers

data RenderConfig = RenderConfig { pictureAttrs   :: PictureAttrs
                                 , pictureStyles  :: PictureStyles
--                                 , mkALayer       :: Int -> SomeLayer -> ALayer
                                 , renderLayers   :: [SomeLayer] -> [Expr]
--                                 , renderLayer    :: ALayer -> [Expr]
--                                 , renderSynopses :: ALayer -> [Expr]
                                 }

data NElemExtra = NElemExtra { nElem :: NElem
                             , nElemLayer :: Int
                             , nElemIndex :: Int
                             , nElemName  :: String
                             }
data ALayer = ALayer Int [NElemExtra]

-----------------------------------------------------------------------------

--defaultRenderConfig = RenderConfig defaultPicAttrs
--                                   defaultPicStyles
--                                   defaultRenderLayer
--                                   defaultRenderSynopses

defaultPicAttrs  = PictureAttrs [Attr ""]
defaultPicStyles = PictureStyles []


layerAnnot (NElemExtra _ _ _ fstName : _) name =
    Expr $ Node "input layer"
                [ Attr "annot"
                , Attr $ "above of=" ++ fstName
                , Attr "node distance=1cm"
                  ]
                Nothing
                [Expr $ RawExpr name]

defaultRenderLayers :: [SomeLayer] -> [Expr]
defaultRenderLayers = defaultRenderLayers' . zipWith f [0..]
    where f n (SomeLayer l) = (n, map (mkNElemExtra n) . zip [1..] $ vec2list l)
          mkNElemExtra n (i, e) = NElemExtra e i i (show n ++ "-" ++ show i)


defaultRenderLayers' ((0, l) : ls) = defaultRenderInputs l ++ annot : defaultRenderLayers' ls
    where annot = layerAnnot l "Input Layer"

defaultRenderLayers' [(n, l)] = defaultRenderOutputs l ++ [annot]
    where annot = layerAnnot l "Output Layer"

defaultRenderLayers' ((n, l) : ls) = defaultRenderHidden l ++ annot : defaultRenderLayers' ls
    where annot = layerAnnot l $ "Hidden Layer #" ++ show n



mkNode l i name attrs = Node name attrs (Just $ show l ++ ",-" ++ show i) []

defaultRenderInputs (NElemExtra NInput 0 i name : ns) = Expr node : defaultRenderInputs ns
    where node =  mkNode 0 i name [Attr "input neuron", Attr $ "pin=left:Input " ++ show i]
defaultRenderInputs [] = []


defaultRenderGeneric attrs (NElemExtra (Neuron inputs) n i name : ns) =
    Expr node : synapses ++ defaultRenderInputs ns

    where node = mkNode n i name (attrs i)
          synapses = undefined -- TODO

defaultRenderGeneric _ [] = []

defaultRenderHidden = defaultRenderGeneric (const [Attr "hidden neuron"])

defaultRenderOutputs =
    defaultRenderGeneric (\i -> [ Attr "output neuron"
                                , Attr $ "pin={[pin edge={->}]right:Output " ++ show i
                                ])

--defaultRenderSynopses :: ALayer -> [Expr]
--defaultRenderSynopses i (SomeLayer l) = defaultRenderSynopses' i $ zip [1..] $ vec2list l

defaultRenderSynopses' = undefined

--defaultRenderSynopses' l ((i, Neuron ) : ns) = Path Edge
--                                              ("")
--                                              ""

