{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Types
-- Description : Types for the lexer
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : MIT
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental
module Types
  ( BasicType(..)
  , Keyword(..)
  , Getter(..)
  , Operator(..)
  , Punctuation(..)
  , TokenData(..)
  , Pos
  , pos1
  , mkPos
  , SourcePos(..)
  , TokenPos(..)
  , Token(..)
  , Input(..)
  , LexError(..)
  , HasPath(..)
  , HasRowSpan(..)
  , HasColSpan(..)
  , HasText(..)
  , HasPosition(..)
  , HasPayload(..)
  , row
  , column
  ) where

import Control.Exception (assert)
import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Data.Text as T
import Text.Megaparsec.Pos hiding (mkPos)
import Lens.Micro
import Lens.Micro.TH
import qualified Text.PrettyPrint.ANSI.Leijen as P


_defaultShow :: P.Pretty a => a -> String
_defaultShow x = P.displayS (P.renderPretty 0.4 50 . P.pretty $ x) ""

-- TODO: Use UnpackSumTypes extension to make 'BasicType' unpackable.
data BasicType
  = TpInt
  | TpBool
  | TpChar
  | TpVoid
  deriving (Read, Show, Eq, Generic)

instance NFData BasicType

instance P.Pretty BasicType where
  pretty = P.bold . P.blue . P.text . go
    where go TpInt = "Int"
          go TpBool = "Bool"
          go TpChar = "Char"
          go TpVoid = "Void"

-- instance Show BasicType where
--   show = _defaultShow

{-
data BuiltinType
  = TpSimple {-# UNPACK #-}!BasicType
  | TpPair {-# UNPACK #-}!(BuiltinType, BuiltinType)
  | TpList !BuiltinType
  deriving (Read, Show, Eq, Generic)

instance NFData BuiltinType
-}

-- TODO: Use UnpackSumTypes extension to make 'Keyword' unpackable.
data Keyword
  = KwIf
  | KwElse
  | KwWhile
  | KwReturn
  | KwVar
  deriving (Read, Show, Eq, Generic)

instance NFData Keyword

instance P.Pretty Keyword where
  pretty = P.bold . P.green . P.text . go
    where go KwIf = "if"
          go KwElse = "else"
          go KwWhile = "while"
          go KwReturn = "return"
          go KwVar = "var"

-- instance Show BasicType where
--   show = _defaultShow

-- TODO: Use UnpackSumTypes extension to make 'Getter' unpackable.
data Getter
  = GtHead
  | GtTail
  | GtFirst
  | GtSecond
  deriving (Read, Show, Eq, Generic)

instance NFData Getter

instance P.Pretty Getter where
  pretty = P.bold . P.text . go
    where go GtHead = ".hd"
          go GtTail = ".tl"
          go GtFirst = ".fst"
          go GtSecond = ".snd"

-- instance Show Getter where
--   show = _defaultShow

-- TODO: Use UnpackSumTypes extension to make 'Operator' unpackable.
data Operator
  = OpPlus
  | OpMinus
  | OpTimes
  | OpDivide
  | OpModulo
  | OpEqual
  | OpLess
  | OpGreater
  | OpLessEqual
  | OpGreaterEqual
  | OpNotEqual
  | OpAnd
  | OpOr
  | OpPrepend
  | OpNot
  | OpAssign
  deriving (Read, Show, Eq, Generic)

instance NFData Operator

instance P.Pretty Operator where
  pretty = go
    where go OpPlus = P.char '+'
          go OpMinus = P.char '-'
          go OpTimes = P.char '*'
          go OpDivide = P.char '/'
          go OpModulo = P.char '%'
          go OpEqual = P.text "=="
          go OpLess = P.char '<'
          go OpGreater = P.char '>'
          go OpLessEqual = P.text "<="
          go OpGreaterEqual = P.text ">="
          go OpNotEqual = P.text "!="
          go OpAnd = P.text "&&"
          go OpOr = P.text "||"
          go OpPrepend = P.char ':'
          go OpNot = P.char '!'
          go OpAssign = P.char '='

-- TODO: Use UnpackSumTypes extension to make 'Punctuation' unpackable.
data Punctuation
  = PtSemiColon
  | PtComma
  | PtDoubleColon
  | PtRArrow
  | PtLParen
  | PtRParen
  | PtLBracket
  | PtRBracket
  | PtLBrace
  | PtRBrace
  deriving (Read, Show, Eq, Generic)

instance NFData Punctuation

instance P.Pretty Punctuation where
  pretty = go
    where go PtSemiColon = P.semi
          go PtComma = P.comma
          go PtDoubleColon = P.text "::"
          go PtRArrow = P.text "->"
          go PtLParen = P.lparen
          go PtRParen = P.rparen
          go PtLBracket = P.lbracket
          go PtRBracket = P.rbracket
          go PtLBrace = P.lbrace
          go PtRBrace = P.rbrace


data TokenData
  = TkId {-# UNPACK #-}!T.Text
  | TkKeyword !Keyword
  | TkGetter !Getter
  | TkOperator !Operator
  | TkType !BasicType
  | TkBool !Bool -- NOTE: Why can't we unpack a Bool?
  | TkInt {-# UNPACK #-}!Int
  | TkChar {-# UNPACK #-}!Char
  | TkPunctuation !Punctuation
  deriving (Read, Show, Eq, Generic)

instance NFData TokenData

instance P.Pretty TokenData where
  pretty = go
    where go (TkId x) = P.text . T.unpack $ x
          go (TkKeyword x) = P.pretty x
          go (TkGetter x) = P.pretty x
          go (TkOperator x) = P.pretty x
          go (TkType x) = P.pretty x
          go (TkBool x) = P.bool x
          go (TkInt x) = P.int x
          go (TkChar x) = P.squotes . P.char $ x
          go (TkPunctuation x) = P.pretty x

data TokenPos = TokenPos
  { _tokenPosPath :: !FilePath
  , _tokenPosRowSpan :: {-# UNPACK #-}!(Pos, Pos)
  , _tokenPosColSpan :: {-# UNPACK #-}!(Pos, Pos)
  } deriving (Read, Eq, Generic)

makeFields ''TokenPos

instance NFData TokenPos

makeLensesFor [ ("sourceLine", "row")
              , ("sourceColumn", "column") ] ''SourcePos

instance HasPath SourcePos FilePath where
  path f x = (\name' -> x {sourceName = name'}) <$> f (sourceName x)

pos1 :: Pos
pos1 = unsafePos 1

mkPos :: Int -> Pos
mkPos !n = assert (n > 0) $ unsafePos . fromIntegral $ n


-- instance HasPath SourcePos FilePath where
--   path = sourceName
--
-- instance HasRow SourcePos Pos where
--   row = sourceLine
--
-- instance HasColumn SourcePos Pos where
--   column = sourceColumn

instance Show TokenPos where
  show (TokenPos p rows cols) =
    show p ++ ":" ++ show_ rows ++ ":" ++ show_ cols
    where
      show_ (a, b) = "[" ++ show (unPos a) ++ ", " ++ show (unPos b) ++ ")"

data Token =
  Token { _tokenPosition :: !TokenPos
        , _tokenPayload :: !TokenData
        } deriving (Read, Show, Eq, Generic)

instance NFData Token

makeFields ''Token

data Input = Input { _inputPosition :: !SourcePos
                   , _inputText :: String
                   } deriving (Read, Show, Eq, Generic)

makeFields ''Input


data LexError = LexError !SourcePos !T.Text
  deriving (Eq, Generic)

instance NFData LexError

instance P.Pretty LexError where
  pretty (LexError pos msg) =
    P.red (P.string "Error:")
      P.<+> P.string (pos ^. path) P.<> P.colon
      P.<+> P.int (fromIntegral . unPos $ (pos ^. row)) P.<+> P.colon
      P.<+> P.int (fromIntegral . unPos $ (pos ^. column)) P.<+> P.string (T.unpack msg)

instance Show LexError where
  show x = P.displayS (P.renderPretty 0.4 50 . P.pretty $ x) ""
