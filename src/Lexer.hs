{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Lexer
-- Description : Converts source code into a list of tokens.
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : MIT
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental
--
-- This module implements a lexer for the Simple Programming Language. It
-- recognises the following grammar:
--
-- > BoolLiteral  = ‟True“ | ‟False“
-- > CharLiteral  = ‟'“ (c | ‟\“ e) ‟'“
-- where @c@ is any printable unicode character except for @‟\\“@ and
-- @‟\'“@, and @e@ is one of @{‟n“, ‟r“, ‟t“, ‟f“, ‟v“}@.
--
-- > IntLiteral   = d+
-- where @d@ is one of @{‟0“,...,‟9“}@.
--
-- > Keyword      = ‟if“ | ‟else“ | ‟while“ | ‟return“ | ‟var“
-- > BasicType    = ‟Int“ | ‟Bool“ | ‟Char“
-- > Getter       = ‟.hd“ | ‟.tl“ | ‟.fst“ | ‟.snd“
-- > Operator     = ‟+“ | ‟-“ | ‟*“ | ‟/“ | ‟%“
-- >              | ‟==“ | ‟<“ | ‟>“ | ‟<=“ | ‟>=“ | ‟!=“
-- >              | ‟&&“ | ‟||“ | ‟!“
-- >              | ‟=“
-- >              | ‟:“
-- > Punctuation  = ‟;“ | ‟,“ | ‟::“ | ‟->“
-- >              | ‟(“ | ‟)“ | ‟[“ | ‟]“ | ‟{“ | ‟}“
--
--
module Lexer
  ( scan
  , skipPrints
  , scanMultiComment
  , scanToken'
  , scanVarId'
  , scanConId'
  , scanId'
  , scanNum'
  , scanDash'
  , scanSimple
  , main
  ) where

import Control.Exception (assert)

import Control.Arrow

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
import Text.Megaparsec.Pos (defaultTabWidth)

-- import Lens.Micro.TH
import Types

-- | Converts an input stream into a list of tokens.
scan :: Input -> Except LexError [Token]
scan stream@(view text -> txt)
  | (c:_) <- txt = go c (stream & text %~ tail)
  | [] <- txt = return []
  where
    go c s
      | Char.isSpace c = scan $ scanSpace' c s
      | c == '/' =
        case s ^. text of
          ('/':_) -> scan . skipPrints . unsafeSkipPrint . moveRight $ s
          ('*':_) ->
            scan =<< (scanMultiComment . unsafeSkipPrint . moveRight $ s)
          _ -> mkToken' divide c s >>= andLoop
      | otherwise = scanToken' c s >>= andLoop
    andLoop (t, s) = (:) t <$> scan s
    divide x s = assert (x == '/') $ return (TkOperator OpDivide, moveRight s)

scanSpace' :: Char -> Input -> Input
scanSpace' c s = assert (Char.isSpace c) $ skip' c s

-- | Skips a sequence of printable characters. It's most important use case is
-- to skip everything until a newline after encountering @//@.
skipPrints :: Input -> Input
skipPrints = go
  where
    go s@(view text -> txt)
      | (c:_) <- txt =
        if Char.isPrint c
          then go . unsafeSkipPrint $ s
          else s
      | [] <- txt = s

-- | Skips everything until it encounters @‟*/“@. If the stream ends before the
-- comment is closed, an exception is thrown.
scanMultiComment :: Input -> Except LexError Input
scanMultiComment = go
  where
    go s@(view text -> txt)
      | ('*':'/':_) <- txt = return . unsafeSkipPrintN 2 $ s
      | (_:_) <- txt = go . skip $ s
      | [] <- txt = throwE $ LexError (s ^. position) "Unterminated comment."

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
    Char.DashPunctuation -> scanDash' x s
    Char.OpenPunctuation -> scanOpen' x s
    Char.ClosePunctuation -> scanClose' x s
    -- Char.InitialQuote -> fail
    -- Char.FinalQuote -> check for string
    Char.OtherPunctuation -> scanPunctuation' x s
    Char.MathSymbol -> scanOperator' x s
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
    _ -> throwE $ unexpectedCharError x s

scanId' :: Char -> Input -> Except LexError (TokenData, Input)
scanId' !x =
  assert (Char.isAlpha x) $
  go . moveRight >=> return . first (TkId . T.pack . (:) x)
  where
    go s@(view text -> (c:_))
      | Char.isAlphaNum c = first ((:) c) <$> go (unsafeSkipPrint s)
      | otherwise = return ("", s)
    go s = return ("", s)

scanVarId' :: Char -> Input -> Except LexError (Token, Input)
scanVarId' = mkToken' go
  where
    go x = scanId' x >=> return . first tryToKeyword

scanConId' :: Char -> Input -> Except LexError (Token, Input)
scanConId' = mkToken' go
  where
    go x = scanId' x >=> return . first (tryToBool . tryToBasicType)

scanNum' :: Char -> Input -> Except LexError (Token, Input)
scanNum' = mkToken' go
  where
    go x =
      assert (Char.isDigit x) $
      scanInt' (Char.digitToInt x) . skipZeros' x . moveRight >=>
      return . first TkInt
    skipZeros' x =
      if x == '0'
        then skipZeros
        else id
    skipZeros s@(view text -> txt)
      | ('0':_) <- txt = skipZeros . unsafeSkipPrint $ s
      | otherwise = s
    scanInt' !n s@(view text -> (!c:_))
      | n < 0 =
        throwE $ LexError (s ^. position) "Detected overflow in 'Int' literal."
      | Char.isDigit c =
        scanInt' (10 * n + Char.digitToInt c) (unsafeSkipPrint s)
      | isValidAfterExp c = return (n, s)
      | Char.isAlpha c =
        throwE $ LexError (s ^. position) "Identifier can't start with a digit."
      | otherwise = throwE $ LexError (s ^. position) (unexpectedChar c)
    scanInt' !n s = return (n, s)

scanDash' :: Char -> Input -> Except LexError (Token, Input)
scanDash' = mkToken' go
  where
    go x s@(view text -> txt)
      | x == '-' =
        case txt of
          ('>':_) ->
            return (TkPunctuation PtRArrow, unsafeSkipPrint . moveRight $ s)
          _ -> return (TkOperator OpMinus, moveRight s)
      | otherwise = throwE $ unexpectedCharError x s

-- | Constructs an error string indicating a bug in the mentioned function.
bugMessage ::
     T.Text -- ^ Function name
  -> T.Text -- ^ Message
  -> String -- ^ Error string.
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

-- | Skip one printable character assuming the input is not empty.
unsafeSkipPrint :: Input -> Input
unsafeSkipPrint = moveRight . (text %~ tail)

-- | Skip @n@ printable characters assuming the input has at least @n@
-- characters left.
unsafeSkipPrintN :: Int -> Input -> Input
unsafeSkipPrintN = go'
  where
    go' !n = assert (n >= 0) $ go n
    go !n s
      | n < 0 = error $ bugMessage "Lexer.unsafeSkipPrintN" "Negative argument."
      | n == 0 = s
      | otherwise = go (n - 1) . moveRight . (text %~ tail) $ s

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

mkToken' ::
     (Char -> Input -> Except e (TokenData, Input))
  -> Char
  -> Input
  -> Except e (Token, Input)
mkToken' f c s =
  f c s >>= \(t, s') ->
    return (Token (s ^. position `upto` s' ^. position) t, s')

{-
mkToken ::
     (Input -> Except e (TokenData, Input)) -> Input -> Except e (Token, Input)
mkToken f s =
  f s >>= \(t, s') -> return (Token (s ^. position `upto` s' ^. position) t, s')
-}
scanOperator' :: Char -> Input -> Except LexError (Token, Input)
scanOperator' = mkToken' go'
  where
    go' x s = assert (Char.generalCategory x == Char.MathSymbol) $ go x s
    go x s@(view text -> txt)
      | x == '+' = o OpPlus s
      | x == '=' =
        case txt of
          ('=':_) -> o2 OpEqual s
          _ -> o OpAssign s
      | x == '<' =
        case txt of
          ('=':_) -> o2 OpLessEqual s
          _ -> o OpLess s
      | x == '>' =
        case txt of
          ('=':_) -> o2 OpGreaterEqual s
          _ -> o OpGreater s
      | x == '|' =
        case txt of
          ('|':_) -> double (TkOperator OpOr) s
          _ -> invalidOperator x s
      | otherwise = throwE $ unexpectedCharError x s
    o t = single (TkOperator t)
    o2 t = double (TkOperator t)
    single t s = return (t, moveRight s)
    double t s = return (t, unsafeSkipPrint . moveRight $ s)
    invalidOperator _ (view position -> p) =
      throwE $ LexError p "Invalid operator."

scanPunctuation' :: Char -> Input -> Except LexError (Token, Input)
scanPunctuation' = mkToken' go'
  where
    go' x s = assert (Char.generalCategory x == Char.OtherPunctuation) $ go x s
    go x s@(view text -> txt)
      | x == ';' = pt PtSemiColon s
      | x == ',' = pt PtComma s
      | x == '*' = op OpTimes s
      | x == '%' = op OpModulo s
      | x == '.' =
        case txt of
          ('h':'d':_) -> gt GtHead (unsafeSkipPrintN 2 s)
          ('t':'l':_) -> gt GtTail (unsafeSkipPrintN 2 s)
          ('f':'s':'t':_) -> gt GtFirst (unsafeSkipPrintN 3 s)
          ('s':'n':'d':_) -> gt GtSecond (unsafeSkipPrintN 3 s)
          _ -> invalidOperator x s
      | x == ':' =
        case txt of
          (':':_) -> pt PtDoubleColon (unsafeSkipPrint s)
          _ -> op OpPrepend s
      | x == '&' =
        case txt of
          ('&':_) -> op OpAnd (unsafeSkipPrint s)
          _ -> invalidOperator x s
      | x == '!' =
        case txt of
          ('=':_) -> op OpNotEqual (unsafeSkipPrint s)
          _ -> op OpNot s
      | x == '\'' = scanChar' x s
      | otherwise = throwE $ unexpectedCharError x s
    op t s = return (TkOperator t, moveRight s)
    pt t s = return (TkPunctuation t, moveRight s)
    gt t s = return (TkGetter t, moveRight s)
    invalidOperator _ (view position -> p) =
      throwE $ LexError p "Invalid operator."

scanSimple :: T.Text -> Either LexError [Token]
scanSimple = runExcept . scan . Input (SourcePos "" pos1 pos1) . T.unpack

tryToKeyword :: TokenData -> TokenData
tryToKeyword t@(TkId !txt) =
  case txt of
    "if" -> TkKeyword KwIf
    "else" -> TkKeyword KwElse
    "var" -> TkKeyword KwVar
    "while" -> TkKeyword KwWhile
    "return" -> TkKeyword KwReturn
    _ -> t
tryToKeyword _ =
  error $ bugMessage "Lexer.tryToKeyword" "Non-exhaustive pattern."

tryToBasicType :: TokenData -> TokenData
tryToBasicType t@(TkId !txt) =
  case txt of
    "Int" -> TkType TpInt
    "Bool" -> TkType TpBool
    "Char" -> TkType TpChar
    "Void" -> TkType TpVoid
    _ -> t
tryToBasicType t = t

tryToBool :: TokenData -> TokenData
tryToBool t@(TkId !txt) =
  case txt of
    "True" -> TkBool True
    "False" -> TkBool False
    _ -> t
tryToBool t = t

-- | Returns whether a character is allowed to occur after an /Expression/.
isValidAfterExp :: Char -> Bool
isValidAfterExp c =
  Char.isSpace c || is2OpStart c || c == ',' || c == ';' || c == ')'

-- Yes, this is very ugly. I guess somebody has to fix Hindent...
is2OpStart :: Char -> Bool
is2OpStart c =
  c == '=' ||
  c == '+' ||
  c == '-' ||
  c == '*' ||
  c == '/' || c == '%' || c == '<' || c == '>' || c == '!' || c == '.'

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


scanChar' :: Char -> Input -> Except LexError (TokenData, Input)
scanChar' = go
  where
    go x =
      assert (x == '\'') $ scanCharImpl' . moveRight >=> return . first TkChar
    scanCharImpl' s@(view text -> txt)
      | ('\\':c:'\'':_) <- txt =
        let s' = unsafeSkipPrintN 3 s
        in case c of
             'n' -> return ('\n', s')
             'r' -> return ('\r', s')
             't' -> return ('\t', s')
             'v' -> return ('\v', s')
             'f' -> return ('\f', s')
             '\'' -> return ('\'', s')
             '"' -> return ('"', s')
             _ -> throwE $ LexError (s ^. position) "Invalid escape sequence."
      | (c:'\'':_) <- txt =
        let s' = unsafeSkipPrintN 2 s
        in if Char.isPrint c
             then return (c, s')
             else throwE $ LexError (s ^. position) "Invalid character literal."
      | ('\'':_) <- txt =
        throwE $ LexError (s ^. position) "Invalid character literal."
      | otherwise =
        throwE $ LexError (s ^. position) "Unterminated character literal."

scanOpen' :: Char -> Input -> Except LexError (Token, Input)
scanOpen' = mkToken' go
  where
    pt x s = return (TkPunctuation x, moveRight s)
    go x
      | x == '(' = pt PtLParen
      | x == '[' = pt PtLBracket
      | x == '{' = pt PtLBrace
      | otherwise = throwE . unexpectedCharError x

scanClose' :: Char -> Input -> Except LexError (Token, Input)
scanClose' = mkToken' go
  where
    pt x s = return (TkPunctuation x, moveRight s)
    go x
      | x == ')' = pt PtRParen
      | x == ']' = pt PtRBracket
      | x == '}' = pt PtRBrace
      | otherwise = throwE . unexpectedCharError x


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
