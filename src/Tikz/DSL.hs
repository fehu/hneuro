-----------------------------------------------------------------------------
--
-- Module      :  Tikz.DSL
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}

module Tikz.DSL (

  Repr

, Elem(..)
, ElemType(..)
, TypeOfElem(..)

, Attr(..)

, Expr(..)
, ExprType(..)
, TypeOfExpr(..)

, Cmd(..)
, Env(..)

, EmptyLine(..)
, CommentLines(..)

, picture
, PictureDefs(..)
, PictureAttrs(..)
, PictureStyles(..)

, Style(..)

, Node(..)

, Path(..)
, PathType(..)

) where


import qualified Data.List as List

import Prelude hiding (concat, unwords, lines)
import Data.ByteString.Char8 (ByteString, pack, concat, intercalate, unwords, append)
import qualified Data.ByteString.Char8 as BS

-----------------------------------------------------------------------------

type Repr = ByteString

type Indent = Int

-----------------------------------------------------------------------------


data ElemType = TAttr | TExpr
data ExprType =  TCmd | TEnv

type family TypeOfElem e :: ElemType
type family TypeOfExpr e :: ExprType

class Elem a where -- elemType      :: a -> ElemType
                   elemMultiline :: a -> Bool
                   elemRepr      :: Indent -> a -> ByteString

data Attr = forall e . (Elem e, TypeOfElem e ~ TAttr) => Attr e
data Expr = forall e . (Elem e, TypeOfElem e ~ TExpr) => Expr e

data Cmd = forall e . (Elem e, TypeOfElem e ~ TExpr, TypeOfExpr e ~ TCmd) => Cmd e
data Env = Env { envName  :: String
               , envAttrs :: [Attr]
               , envBody  :: [Expr]
               }
data EmptyLine = EmptyLine
data CommentLines = CommentLines [String]


instance Elem Attr where -- elemType _ = TAttr
                         elemMultiline (Attr a) = elemMultiline a
                         elemRepr i (Attr a) = elemRepr i a
instance Elem Expr where -- elemType _ = TExpr
                         elemMultiline (Expr e) = elemMultiline e
                         elemRepr i (Expr e) = elemRepr i e
instance Elem Cmd  where -- elemType _ = TExpr
                         elemMultiline (Cmd c)  = elemMultiline c
                         elemRepr i (Cmd c) = elemRepr i c

instance Elem Env  where -- elemType _ = TExpr
                         elemMultiline _ = True
                         elemRepr i e = concat [
                              indent i
                            , latexKey "begin"
                            , key
                            , optArgs $ envAttrs e
                            , newline
                            , lines . map (elemRepr (nextIndent i)) $ envBody e
                            , newline
                            , indent i
                            , latexKey "end"
                            , key
                            ]
                            where key = surroundBracketsCurly . pack $ envName e

instance Elem EmptyLine where -- elemType _ = TExpr
                              elemMultiline _ = True
                              elemRepr _ _ = newline

instance Elem CommentLines where -- elemType _ = TExpr
                                 elemMultiline _ = True
                                 elemRepr i (CommentLines ls) = pack $ pre
                                                                    ++ List.intercalate pre ls
                                                                    ++ "\n"
                                                              where pre = indent' i ++ "% "

type instance TypeOfElem Attr = TAttr
type instance TypeOfElem Expr = TExpr
type instance TypeOfElem Cmd  = TExpr
type instance TypeOfElem Env  = TExpr
type instance TypeOfElem EmptyLine    = TExpr
type instance TypeOfElem CommentLines = TExpr

-----------------------------------------------------------------------------

newtype PictureDefs   = PictureDefs [Cmd]
newtype PictureAttrs  = PictureAttrs [Attr]
newtype PictureStyles = PictureStyles [Expr]

-----------------------------------------------------------------------------

indent  = pack . indent'
indent' = flip replicate ' '

nextIndent = (+) 4

newline = pack "\n"

lines = intercalate (pack "\n")


latexKey k = pack $ "\\" ++ k

surround  pre suf x = concat [pack pre, x, pack suf]
surround' pre suf x = concat $ map pack [pre, x, suf]

surroundBracketsCurly  = surround "{" "}"
surroundBracketsSquare = surround "[" "]"
surroundBracketsRound  = surround "(" ")"

surroundBracketsCurly'  = surround' "{" "}"
surroundBracketsSquare' = surround' "[" "]"
surroundBracketsRound'  = surround' "(" ")"

surroundSpace s = surround s' s'
                where s' = replicate s ' '

optArgs [] = BS.empty
optArgs as = surroundBracketsSquare . intercalate (pack ",") $ map (elemRepr 0) as

-----------------------------------------------------------------------------

type instance TypeOfElem String = TAttr

instance Elem String where elemMultiline _ = False
                           elemRepr _ = pack

-----------------------------------------------------------------------------

picture (PictureAttrs attrs) (PictureStyles styles) body =
    Env "tikzpicture" attrs (styles ++ Expr EmptyLine : body)

data Style = Style String [Attr]

instance Elem Style where elemMultiline _ = False
                          elemRepr i (Style name vals) = concat [
                              indent i
                            , latexKey "tikzstyle"
                            , surroundBracketsCurly' name
                            , pack " = "
                            , optArgs vals
                            , pack ";"
                            ]

data Node = Node String [Attr] (Maybe String) [Expr]

instance Elem Node where
    elemMultiline (Node _ _ _ []) = False
    elemMultiline _               = True
    elemRepr i (Node name attrs at body) = concat [
          indent i
        , latexKey "node"
        , optArgs attrs
        , surroundBracketsRound' name
        , at'
        , body'
        ]
        where at' = case at of Just a -> concat [
                                            pack "at "
                                          , surroundBracketsRound' a
                                          ]
                               _      -> BS.empty

              body' = case body of [] -> pack "{};"
                                   _  -> concat [
                                          pack "{"
                                        , newline
                                        , lines $ map (elemRepr (nextIndent i)) body
                                        , newline
                                        , indent i
                                        , pack "}"
                                        ]

data PathType = Edge

data Path = Path PathType String String

instance Elem Path where elemMultiline _ = False
                         elemRepr i (Path Edge from to) = concat [
                              indent i
                            , latexKey "path "
                            , surroundBracketsRound' from
                            , pack " edge "
                            , surroundBracketsRound' to
                            , pack ";"
                            ]


-----------------------------------------------------------------------------

--data GenericExprHead = GenericExprHead String (Maybe Attr) [Attr]
--
--surround pre suf x = concat [pack pre, x, pack suf]
--
--mainAttrRepr :: Maybe Attr -> Repr
--mainAttrRepr (Just a) = surround "{" "}" $ elemRepr 0 a --concat [pack "{", elemRepr a 0, pack "}"]
--mainAttrRepr _        = BS.empty
--
--extraAttrsRepr :: [Attr] -> Repr
--extraAttrsRepr [] = BS.empty
--extraAttrsRepr as = surround "[" "]" . intercalate (pack ",") $ map (elemRepr 0) as
--
--exprHeadRepr (GenericExprHead name mAttr eAttrs) ind =
--    concat [pack $ i ++ "\\" ++ name, mainAttrRepr mAttr, extraAttrsRepr eAttrs]
--        where i = replicate ind ' '
--
--data GenericExpr = GenericExpr GenericExprHead ExprType MkExpr Bool
--
--instance Elem GenericExpr where elemType _ = TExpr
--                                elemMultiline (GenericExpr _ _ _ m) = m
--                                elemRepr ind (GenericExpr gh repr _) = exprHeadRepr gh ind `append` repr ind
--
--type MkExpr = Indent -> Repr

--newExpr :: String -> (Maybe Attr) -> [Attr] -> MkExpr -> Repr
--newExpr name ma eas bf =

--instance Elem GenericCmd where elemType _ = TExpr Cmd

-----------------------------------------------------------------------------

--data ElemType = TAttr | TExpr
--data ExprType =  Cmd | Env
--
--class ElemExpr e where exprType      :: e -> ExprType
--                       exprMultiline :: e -> Bool
--
--class ElemAttr a
--
--class Elem a where elemType      :: a -> ElemType
--                   elemMultiline :: a -> Bool


--type family A (e :: Constraint) where
--    A (ElemExpr e) = TExpr
--    A (ElemAttr e) = TAttr

--class B a e where elemType'      :: a -> ElemType
--                  elemMultiline' :: a -> Bool
--
--instance B a (ElemExpr)


--instance (A (ElemAttr a) ~ TAttr) => Elem a
--instance (A (ElemExpr e) ~ TExpr) => Elem e


--instance (Expr a) => Elem a


--class ElemRepr repr where elemRepr :: (Elem a) => a -> repr
--                          str2repr :: String -> repr



--data TElem (t :: ElemType) = forall a . Elem a t => TElem (a t)

--class ElemRepr repr where elemRepr :: (Elem a t) => a t -> repr
--                          str2repr :: String -> repr

--class (Elem a t) => ElemRepr a t repr where elemRepr :: a t -> repr

--data Expr = forall a . Elem a TExpr

-----------------------------------------------------------------------------





-----------------------------------------------------------------------------


