{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}

-- |
-- Module      : Lexer
-- Description : Lexer
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : MIT
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental
module Lexer
  ( scanSimple
  , main
  ) where

import Control.Exception (assert)

-- import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans.Except
import qualified Data.Char as Char

-- import Data.Functor.Identity
import Data.Semigroup
import qualified Data.Text as T

import Lens.Micro
import Lens.Micro.Extras
import qualified Text.PrettyPrint.ANSI.Leijen as P

-- import GHC.Generics (Generic)
import Numeric (showHex)

-- import Pipes
-- import qualified Pipes.Prelude as P
import Text.Megaparsec.Pos

-- import Lens.Micro.TH
import Types

pos1 :: Pos
pos1 = unsafePos 1

bugMessage :: T.Text -> T.Text -> String
bugMessage name msg =
  T.unpack $
  "Panic!!! This should not have happened. Please, submit a bug report to\n\
  \https://github.com/twesterhout/splc/issues with the following message:\n\n\
  \    Programmer error in " <>
  name <>
  ":" <>
  msg

unexpectedChar :: Char -> T.Text
unexpectedChar c =
  "Unexpected character " <> T.pack (show c) <> " (0x" <>
  T.pack (showHex (Char.ord c) "") <>
  ")."

unexpectedCharError :: Char -> Input -> LexError
unexpectedCharError c (view position -> pos) = LexError pos (unexpectedChar c)

moveRightBy :: Pos -> Input -> Input
moveRightBy n = (position . column) %~ (<> n)

-- | Increases the current column by one.
moveRight :: Input -> Input
moveRight = moveRightBy pos1

-- | Increases the current row by one.
moveDown :: Input -> Input
moveDown = (position . row) %~ (<> pos1)

-- | Goes to the first column.
moveToTheLeft :: Input -> Input
moveToTheLeft = (position . column) .~ pos1

unsafeSkipPrint :: Input -> Input
unsafeSkipPrint = moveRight . (text %~ tail)

-- skipPrint :: Char -> Input -> Input
-- skipPrint c = assert (Char.isPrint c) moveRight
-- | Skips a sequence of printable characters.
skipPrints :: Input -> Input
skipPrints = go
  where
    go s =
      case s ^. text of
        (c:cs) ->
          if Char.isPrint c
            then skipPrints (s & moveRight & text .~ cs)
            else s
        [] -> s

skip :: Input -> Input
skip s =
  case s ^. text of
    (c:cs) -> skip' c (s & text .~ cs)
    [] -> s

skip' :: Char -> Input -> Input
skip' c
  | Char.isPrint c = moveRight
  | Char.isSpace c =
    case c of
      '\t' -> moveRightBy defaultTabWidth
      '\v' -> moveDown
      '\n' -> newline
      '\f' -> newline
      '\r' ->
        \s ->
          case s ^. text of
            ('\n':cs) -> s & newline & (text .~ cs)
            _ -> s & newline
      _ ->
        error $
        bugMessage
          "Lexer.skip"
          ("Space should have been matched already! " <> (unexpectedChar c))
  | otherwise = error $ bugMessage "Lexer.skip" (unexpectedChar c)
  where
    newline = moveToTheLeft . moveDown

scanSpace :: Char -> Input -> Input
scanSpace c s = assert (Char.isSpace c) $ skip' c s

-- scanComment :: Char -> Input -> Input
-- scanComment c s = assert (c == '/' && head (s ^. text) == '/') $ skipPrints s
scanMultiComment :: Input -> Except LexError Input
scanMultiComment = go
  where
    go s =
      case s ^. text of
        ('*':'/':_) -> return . skip . skip $ s
        (_:_) -> go . skip $ s
        [] -> throwE $ LexError (s ^. position) "Unterminated comment."

scanOperator' :: Char -> Input -> Except LexError (Token, Input)
scanOperator' = scanUnknown'

-- scanChar :: Char -> Input -> Except String [Token]
scanImpl :: Char -> Input -> Except LexError [Token]
scanImpl c s
  | Char.isSpace c = scan $ scanSpace c s
  | c == '/' =
    case s ^. text of
      ('/':_) -> scan . skipPrints . unsafeSkipPrint . moveRight $ s
      ('*':_) -> scan =<< (scanMultiComment . unsafeSkipPrint . moveRight $ s)
      _ -> scanOperator' c s >>= \(token, s') -> (:) token <$> scan s'
  | otherwise = scanToken' c s >>= \(token, s') -> (:) token <$> scan s'

scan :: Input -> Except LexError [Token]
scan s =
  case s ^. text of
    (c:cs) -> scanImpl c (s & text .~ cs)
    [] -> return []

scanSimple :: T.Text -> Either LexError [Token]
scanSimple = runExcept . scan . Input (SourcePos "" pos1 pos1) . T.unpack

scanUnknown' :: Char -> Input -> Except e (Token, Input)
scanUnknown' ch s =
  let p = s ^. position . path
      r = s ^. position . row
      c = s ^. position . column
      !token = Token (TokenPos p (r, r) (c, c <> pos1)) (TkOther ch)
  in return (token, moveRight s)

-- | Returns whether a character is allowed to occur after an /Expression/.
isValidAfterExp :: Char -> Bool
isValidAfterExp c =
  Char.isSpace c || is2OpStart c || c == ',' || c == ';' || c == ')'

-- Yes, this is very ugly. I guess somebody has to fix Hindent...
is2OpStart :: Char -> Bool
is2OpStart c =
  c == '=' || c == '+' || c == '-' || c == '*' || c == '/' || c == '%' || c ==
  '<' ||
  c ==
  '>' ||
  c ==
  '!' ||
  c ==
  '.'

-- | 
isValidAfterId :: Char -> Bool
isValidAfterId c = isValidAfterExp c || c == '(' || c == '{'

infix 2 `upto`

upto :: SourcePos -> SourcePos -> TokenPos
upto !p !q =
  assert (p ^. path == q ^. path) $
  assert
    ((p ^. row == q ^. row) && (p ^. column < q ^. column) ||
     (p ^. row < q ^. row)) $
  TokenPos (p ^. path) (p ^. row, q ^. row) (p ^. column, q ^. column)

scanIdImpl :: Input -> Except LexError (String, Input)
scanIdImpl = go
  where
    go s@(view text -> (c:_))
      | Char.isAlphaNum c =
        go (unsafeSkipPrint s) >>= \(cs, s') -> return (c : cs, s')
      | isValidAfterId c = return ("", s)
      | otherwise = throwE $ unexpectedCharError c s
    go s = return ("", s)

scanId' :: Char -> Input -> Except LexError (TokenPos, T.Text, Input)
scanId' !x s =
  assert (Char.isAlpha x) $ scanIdImpl (moveRight s) >>= \(xs, s') ->
    let !p = (s ^. position) `upto` (s' ^. position)
        !txt = T.pack $ x : xs
    in return (p, txt, s')

scanVarId' :: Char -> Input -> Except LexError (Token, Input)
scanVarId' x =
  scanId' x >=> \(pos, txt, s') -> return (Token pos (TkVarId txt), s')

scanConId' :: Char -> Input -> Except LexError (Token, Input)
scanConId' x =
  scanId' x >=> \(pos, txt, s') -> return (Token pos (TkConId txt), s')

scanInt :: Int -> Input -> Except LexError (Int, Input)
scanInt = go
  where
    go !n s@(view text -> (!c:_))
      | n < 0 =
        throwE $ LexError (s ^. position) "Detected overflow in 'Int' literal."
      | Char.isDigit c = go (10 * n + Char.digitToInt c) (unsafeSkipPrint s)
      | isValidAfterExp c = return (n, s)
      | Char.isAlpha c =
        throwE $ LexError (s ^. position) "Identifier can't start with a digit."
      | otherwise = throwE $ LexError (s ^. position) (unexpectedChar c)
    go !n s = return (n, s)

-- | Skips consecutive zeros.
skipZeros :: Input -> Input
skipZeros = go
  where
    go s@(view text -> ('0':_)) = go $ unsafeSkipPrint s
    go s = s

scanNum' :: Char -> Input -> Except LexError (Token, Input)
scanNum' c s =
  assert (Char.isDigit c) $
  (if c == '0'
     then scanInt 0 . skipZeros . moveRight $ s
     else scanInt (Char.digitToInt c) (moveRight s)) >>= \(n, s') ->
    let pos = s ^. position `upto` s' ^. position
    in return (Token pos (TkInt n), s')

scanToken' :: Char -> Input -> Except LexError (Token, Input)
scanToken' x s =
  case Char.generalCategory x of
    Char.LowercaseLetter -> scanVarId' x s
    Char.UppercaseLetter -> scanConId' x s
    -- Char.TitlecaseLetter -> fail
    -- Char.ModifierLetter -> fail
    -- Char.OtherLetter -> fail
    -- Char.NonSpacingMark -> fail
    -- Char.SpacingCombiningMark -> fail
    -- Char.EnclosingMark -> fail
    Char.DecimalNumber -> scanNum' x s
    -- Char.LetterNumber -> fail
    -- Char.OtherLetter -> fail
    -- Char.ConnectorPunctuation -> fail
    -- Char.DashPunctuation -> fail
    -- Char.OpenPunctuation -> check for brackets
    -- Char.ClosePunctuation -> check for brackets
    -- Char.InitialQuote -> check for string
    -- Char.FinalQuote -> check for string
    -- Char.OtherPunctuation -> fail
    -- Char.MathSymbol -> fail
    -- Char.CurrencySymbol -> fail
    -- Char.ModifierSymbol -> fail
    -- Char.OtherSymbol -> fail
    -- Char.Space -> fail
    -- Char.LineSeparator -> fail
    -- Char.ParagraphSeparator -> fail
    -- Char.Control -> fail
    -- Char.Format -> fail
    -- Char.Surrogate -> fail
    -- Char.PrivateUse -> fail
    -- Char.NotAssigned -> fail
    _ -> scanUnknown' x s

main :: IO ()
main = do
  let x = scanSimple . T.pack $ "facR ( n ) :: Int → Int"
  do case x of
       (Right tokens) -> mapM_ print tokens
       (Left err) -> P.putDoc (P.pretty err)
     P.putDoc $
       P.pretty
         (LexError
            (SourcePos "/home/skype/text.hs" pos1 pos1)
            (T.pack "Hello world."))
