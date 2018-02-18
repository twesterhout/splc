{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

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

import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans.Except
import qualified Data.Char as Char
import Data.Functor.Identity
import Data.Semigroup
import qualified Data.Text as T
import GHC.Generics (Generic)
import Numeric (showHex)
import Pipes
import qualified Pipes.Prelude as P
import Text.Megaparsec.Pos

pos1 :: Pos
pos1 = unsafePos 1

data TokenData
  = TkVarId {-# UNPACK #-}!T.Text
  | TkConId {-# UNPACK #-}!T.Text
  | TkInt {-# UNPACK #-}!Int
  | TkFloat {-# UNPACK #-}!Float
  | TkChar {-# UNPACK #-}!Char
  | TkString !String
  | TkOther !T.Text
  deriving (Read, Show, Eq, Generic)

instance NFData TokenData

data TokenPos = TokenPos
  { _tokenPosPath :: !FilePath
  , _tokenPosRows :: {-# UNPACK #-}!(Pos, Pos)
  , _tokenPosCols :: {-# UNPACK #-}!(Pos, Pos)
  } deriving (Read, Eq, Generic)

instance NFData TokenPos

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

getPosAfter :: Token -> (Pos, Pos)
getPosAfter (Token (TokenPos _ r c) _) = (snd r, snd c)

-- |
constEOT :: Char
constEOT = Char.chr 0x03

textStream :: Monad m => T.Text -> Producer Char m ()
textStream = mapM_ yield . T.unpack >=> const (yield (Char.chr 0x03))

invalidEOT :: Monad m => ExceptT String m a
invalidEOT = throwE $ "Syntax error: Invalid character: EOT (\\0x02)."

-- | Given a row, column, and space-like character 'scanSpace' consumes a single
-- space/newline and returns the next @(row, column, char)@.
scanSpace :: Monad m => Pos -> Pos -> Char -> Consumer' Char m (Pos, Pos, Char)
scanSpace row col c
  | Char.isSpace c =
    case c of
      '\t' -> await >>= tab
      '\v' -> await >>= vtab
      '\n' -> await >>= newline
      '\f' -> await >>= newline
      '\r' ->
        await >>= \c' ->
          if c' == '\n'
            then await >>= newline
            else newline c'
      _ -> await >>= space
  | otherwise =
    error "Panic!!! Lexer.scanSpace: received a non-space character."
  where
    space c' = return $! (row, col <> pos1, c')
    tab c' = return $! (row, col <> defaultTabWidth, c')
    vtab c' = return $! (row <> pos1 <> pos1, pos1, c')
    newline c' = return $! (row <> pos1, pos1, c')

-- scanComment :: Monad m => SourcePos -> Char -> Consumer' Char m (SourcePos, Char)
-- scanComment pos c
--   | c == constEOT =
scanImpl ::
     Monad m => Pos -> Pos -> Char -> Pipe Char Token (ExceptT String m) ()
scanImpl row col c
  | Char.isSpace c =
    scanSpace row col c >>= \(row', col', c') -> scanImpl row' col' c'
  -- By construction, input ends with 'Lexer.eofChar'. If, however, we encounter
  -- 'Lexer.eofChar' while processing user input, it's a syntax error.
  | c == constEOT = await >> lift invalidEOT
  | otherwise = do
    (c', token) <- nextToken "" row col c
    yield token
    let (row', col') = getPosAfter token
    scanImpl row' col' c'

scan :: Monad m => Pipe Char Token (ExceptT String m) ()
scan = await >>= scanImpl pos1 pos1

scanSimple :: T.Text -> Either String [Token]
scanSimple s = runIdentity . runExceptT . P.toListM $ scan <-< textStream s

--------------------------------------------------------------------------------
lexUnknownImpl ::
     Monad m => Pos -> Consumer' Char (ExceptT String m) (Pos, Char, String)
lexUnknownImpl col =
  await >>= \c ->
    if Char.isSpace c || c == constEOT
      then return (col, c, [])
      else lexUnknownImpl (col <> pos1) >>= \(col', t, cs) ->
             return (col', t, c : cs)

lexUnknown ::
     Monad m
  => FilePath
  -> Pos
  -> Pos
  -> Char
  -> Consumer' Char (ExceptT String m) (Char, Token)
lexUnknown path row col c =
  lexUnknownImpl col >>= \(col', t, cs) ->
    return $!
    ( t
    , Token (TokenPos path (row, row) (col, col')) . TkOther . T.pack $ c : cs)

--------------------------------------------------------------------------------
lexVarIdImpl ::
     Monad m => Pos -> Consumer' Char (ExceptT String m) (Pos, Char, String)
lexVarIdImpl col =
  await >>= \c ->
    if Char.isAlphaNum c
      then lexVarIdImpl (col <> pos1) >>= \(col', t, cs) ->
             return (col', t, c : cs)
      else return (col, c, [])

lexVarId ::
     Monad m
  => FilePath
  -> Pos
  -> Pos
  -> Char
  -> Consumer' Char (ExceptT String m) (Char, Token)
lexVarId path row col c =
  lexVarIdImpl (col <> pos1) >>= \(col', t, cs) ->
    return $!
    ( t
    , Token (TokenPos path (row, row) (col, col')) . TkVarId . T.pack $ c : cs)

--------------------------------------------------------------------------------
{-
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
-}
--------------------------------------------------------------------------------
nextToken ::
     Monad m
  => FilePath
  -> Pos
  -> Pos
  -> Char
  -> Consumer' Char (ExceptT String m) (Char, Token)
nextToken path row col c =
  case Char.generalCategory c of
    Char.LowercaseLetter -> lexVarId path row col c
    _ -> lexUnknown path row col c

main :: IO ()
main =
  let x = scanSimple . T.pack $ "facR ( n ) :: Int → Int"
  in case x of
       (Right _) -> print x >> return ()
       (Left err) -> putStrLn err
