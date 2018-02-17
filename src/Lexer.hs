{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Lexer
-- Description : Lexer
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : MIT
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental
module Lexer
  ( Token(..)
  , scan
  , scanSimple
  , textStream
  , main
  ) where

import Control.Monad
import Control.Monad.Trans.Except
import qualified Data.Char as Char
import Data.Functor.Identity
import qualified Data.Text as T
import Numeric (showHex)
import Pipes
import qualified Pipes.Prelude as P

data Token
  = TokenVarId T.Text
  | TokenConId T.Text
  | TokenOther T.Text
  | TokenEOF
  deriving (Read, Show, Eq)

-- |
eofChar :: Char
eofChar = Char.chr 0x03

textStream :: Monad m => T.Text -> Producer Char m ()
textStream = mapM_ yield . T.unpack >=> const (yield (Char.chr 0x03))

scanImpl :: Monad m => Char -> Pipe Char Token (ExceptT String m) ()
scanImpl c
  -- Skip spaces
  | Char.isSpace c = await >>= scanImpl
  -- By construction, input ends with 'Lexer.eofChar'. If, however, we encounter
  -- 'Lexer.eofChar' while processing user input, it's a syntax error.
  | c == eofChar =
    await >>
    lift
      (throwE $
       "Syntax error: Invalid character: EOT (\\x" ++
       showHex (Char.ord eofChar) "" ++ ")")
  | otherwise = do
    (c', token) <- nextToken c
    yield token
    scanImpl c'

scan :: Monad m => Pipe Char Token (ExceptT String m) ()
scan = await >>= scanImpl

scanSimple :: T.Text -> Either String [Token]
scanSimple s = runIdentity . runExceptT . P.toListM $ scan <-< textStream s

--------------------------------------------------------------------------------
lexUnknownImpl :: Monad m => Consumer' Char (ExceptT String m) (Char, String)
lexUnknownImpl =
  await >>= \c ->
    if Char.isSpace c || c == eofChar
      then return (c, [])
      else lexUnknownImpl >>= \(t, cs) -> return (t, c : cs)

lexUnknown :: Monad m => Char -> Consumer' Char (ExceptT String m) (Char, Token)
lexUnknown c =
  lexUnknownImpl >>= \(t, cs) -> return (t, TokenOther . T.pack $ c : cs)

--------------------------------------------------------------------------------
lexVarIdImpl :: Monad m => Consumer' Char (ExceptT String m) (Char, String)
lexVarIdImpl =
  await >>= \c ->
    if Char.isSpace c || c == eofChar
      then return (c, [])
      else if Char.isAlphaNum c
             then lexVarIdImpl >>= \(t, cs) -> return (t, c : cs)
             else lift . throwE $
                  "Syntax error: unexpected character: " ++ show c

lexVarId :: Monad m => Char -> Consumer' Char (ExceptT String m) (Char, Token)
lexVarId c =
  lexVarIdImpl >>= \(t, cs) -> return (t, TokenVarId . T.pack $ c : cs)

--------------------------------------------------------------------------------
lexConIdImpl :: Monad m => Consumer' Char (ExceptT String m) (Char, String)
lexConIdImpl =
  await >>= \c ->
    if Char.isSpace c || c == eofChar
      then return (c, [])
      else if Char.isAlphaNum c
             then lexConIdImpl >>= \(t, cs) -> return (t, c : cs)
             else lift . throwE $
                  "Syntax error: unexpected character: " ++ show c

lexConId :: Monad m => Char -> Consumer' Char (ExceptT String m) (Char, Token)
lexConId c =
  lexConIdImpl >>= \(t, cs) -> return (t, TokenConId . T.pack $ c : cs)

--------------------------------------------------------------------------------
nextToken :: Monad m => Char -> Consumer' Char (ExceptT String m) (Char, Token)
nextToken c =
  case Char.generalCategory c of
    Char.LowercaseLetter -> lexVarId c
    Char.UppercaseLetter -> lexConId c
    otherwise -> lexUnknown c

main :: IO ()
main =
  let x = scanSimple . T.pack $ "facR ( n ) :: Int → Int"
  in case x of
       (Right _) -> return ()
       (Left err) -> putStrLn err
