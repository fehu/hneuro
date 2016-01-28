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

module Neuro.Show.Tikz (

) where

import Neuro.Show
import Neuro.DSL

import Tikz.DSL

import Data.ByteString.Char8 (ByteString)

-----------------------------------------------------------------------------

newtype Tikz = Tikz ByteString


pAttrs  = PictureAttrs [Attr ""]
pStyles = PictureStyles []

instance NShow (NNDescriptor h) Tikz where
    nShow (NNDescriptor h) = Tikz . elemRepr 0
                           $ picture pAttrs pStyles
                            $ do let l = 1
                                 [] -- TODO




