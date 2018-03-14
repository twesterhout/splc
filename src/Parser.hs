{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

-- |
-- Module      : Parser
-- Description : SPL Parser
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : MIT
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental
module Parser
  ( main
  -- , pIdent
  ) where

import Control.Arrow
import Control.DeepSeq
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Combinators
import Control.Monad.Trans.Except (runExcept)
import qualified Data.Char as Char
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as Set

-- import Data.Functor.Identity
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Extras
import Numeric (showHex)
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.PrettyPrint.ANSI.Leijen as P

import Lexer hiding (main)
import Types

{-
mkDummyTk :: TokenData -> Token
mkDummyTk = Token (Span nopos nopos)
  where
    nopos = SourcePos "" pos1 pos1

type Parser = P.Parsec Void [Token]

badTk :: Token -> Maybe (P.ErrorItem Token)
badTk t = pure (P.Tokens (t :| []))

punkt :: TokenData -> Parser ()
punkt x = P.token go Nothing
  where
    go t
      | x == t ^. payload = Right ()
      | otherwise = Left (badTk t, [P.Tokens $ (mkDummyTk x) :| []])

pLit' :: Parser Lit
pLit' = P.token go Nothing
  where
    go t
      | TkInt n <- t ^. payload = Right $ Int n (t ^. position)
      | TkChar c <- t ^. payload = Right $ Char c (t ^. position)
      | TkBool b <- t ^. payload = Right $ Bool b (t ^. position)
      | otherwise =
        Left $
        ( badTk t
        , [ P.Label ('i' :| "nteger constant")
          , P.Label ('c' :| "haracter constant")
          , P.Label ('b' :| "oolean constant")
          ])

pParen :: Parser a -> Parser a
pParen =
  between
    (punkt TkLParen <?> "opening parenthesis '('")
    (punkt TkRParen <?> "closing parenthesis ')'")

pPair :: Parser Lit
pPair =
  pParen $ do
    first <- pExpr
    punkt TkComma <?> "comma ','"
    second <- pExpr
    return $!! Pair (first, second) (first ^. position <> second ^. position)

-- | Parses an identifier
pIdent :: Parser Ident
pIdent = P.token ident Nothing P.<?> "identifier"
  where
    ident t
      | TkIdent txt <- t ^. payload = Right $ Ident txt (t ^. position)
      | otherwise = Left (pure (P.Tokens (t :| [])), Set.empty)
-}

main :: IO ()
main = do
  {-
  src <-
    T.readFile
      "/home/tom/src/splc/external/spl-test-programs/1-parse-errors/ifThenElseWithVariables.spl"
  let stream =
        Input (SourcePos "ifThenElseWithVariables.spl" pos1 pos1) (T.unpack src)
  let (Right ts) = runExcept $ scan stream
  case P.parse pLit' "Hola" (ts) of
    Left e -> P.putDoc $ parseErrorPretty' ts e
    Right x -> print x
  -}
  let program = In TpInt
  P.putDoc . P.pretty $ program
