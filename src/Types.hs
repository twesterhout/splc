{-# LANGUAGE RankNTypes #-}
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
  ( TokenData(..)
  , TokenPos(..)
  , Token(..)
  , Input(..)
  , HasPath(..)
  , HasRowSpan(..)
  , HasColSpan(..)
  , HasText(..)
  , HasPosition(..)
  , row
  , column
  ) where

import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Data.Text as T
import Text.Megaparsec.Pos
import Lens.Micro.TH



data TokenData
  = TkVarId {-# UNPACK #-}!T.Text
  | TkConId {-# UNPACK #-}!T.Text
  | TkInt {-# UNPACK #-}!Int
  | TkFloat {-# UNPACK #-}!Float
  | TkChar {-# UNPACK #-}!Char
  | TkString !String
  | TkOther !Char
  deriving (Read, Show, Eq, Generic)

instance NFData TokenData

data TokenPos = TokenPos
  { _tokenPosPath :: !FilePath
  , _tokenPosRowSpan :: !(Pos, Pos)
  , _tokenPosColSpan :: !(Pos, Pos)
  } deriving (Read, Eq, Generic)

makeFields ''TokenPos

instance NFData TokenPos

makeLensesFor [ ("sourceLine", "row")
              , ("sourceColumn", "column") ] ''SourcePos

instance HasPath SourcePos FilePath where
  path f x = (\name' -> x {sourceName = name'}) <$> f (sourceName x)

-- instance HasPath SourcePos FilePath where
--   path = sourceName
--
-- instance HasRow SourcePos Pos where
--   row = sourceLine
--
-- instance HasColumn SourcePos Pos where
--   column = sourceColumn

instance Show TokenPos where
  show (TokenPos path rows cols) =
    show path ++ ":" ++ show_ rows ++ ":" ++ show_ cols
    where
      show_ (a, b) = "[" ++ show (unPos a) ++ ", " ++ show (unPos b) ++ ")"

data Token =
  Token !TokenPos
        !TokenData
  deriving (Read, Show, Eq, Generic)

instance NFData Token

data Input = Input { _inputPosition :: !SourcePos
                   , _inputText :: String
                   } deriving (Read, Show, Eq, Generic)

makeFields ''Input
