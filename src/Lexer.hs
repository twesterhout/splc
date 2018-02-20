{-# LANGUAGE BangPatterns #-}

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

-- import Control.DeepSeq
-- import Control.Monad
import Control.Monad.Trans.Except
import qualified Data.Char as Char

-- import Data.Functor.Identity
import Data.Semigroup
import qualified Data.Text as T

import Lens.Micro
-- import GHC.Generics (Generic)
-- import Numeric (showHex)
-- import Pipes
-- import qualified Pipes.Prelude as P
import Text.Megaparsec.Pos

-- import Lens.Micro.TH
import Types

pos1 :: Pos
pos1 = unsafePos 1

scanSpace :: Char -> Input -> Input
scanSpace c s
  | Char.isSpace c =
    case c of
      '\t' -> tab
      '\v' -> vtab
      '\n' -> newline
      '\f' -> newline
      '\r' ->
        case s ^. text of
          ('\n':cs) -> newline & text .~ cs
          _ -> newline
      _ -> space
  | otherwise =
    error "Panic!!! Lexer.scanSpace: received a non-space character."
  where
    space = s & (position . column) %~ (<> pos1)
    tab = s & (position . column) %~ (<> defaultTabWidth)
    vtab =
      s & (position . row) %~ (<> pos1 <> pos1) & (position . column) .~ pos1
    newline = s & (position . row) %~ (<> pos1) & (position . column) .~ pos1

scanImpl :: Char -> Input -> Except String [Token]
scanImpl c s
  | Char.isSpace c = scan $ scanSpace c s
  | otherwise = scanToken c s >>= \(token, s') -> (:) token <$> scan s'

scan :: Input -> Except String [Token]
scan s =
  case s ^. text of
    (c:cs) -> scanImpl c (s & text .~ cs)
    [] -> return []

scanSimple :: T.Text -> Either String [Token]
scanSimple = runExcept . scan . Input (SourcePos "" pos1 pos1) . T.unpack

step :: Input -> Input
step = (position . column) %~ (<> pos1)

scanUnknown :: Char -> Input -> Except e (Token, Input)
scanUnknown ch s =
  let p = s ^. position . path
      r = s ^. position . row
      c = s ^. position . column
      !token = Token (TokenPos p (r, r <> pos1) (c, c <> pos1)) (TkOther ch)
  in return (token, step s)

scanIdImpl :: Input -> Except String (String, Input)
scanIdImpl s
  | null (s ^. text) = return ("", s)
  | otherwise =
    let (c:_) = s ^. text
    in if Char.isAlphaNum c
         then scanIdImpl (s & step & text %~ tail) >>= \(cs, s') ->
                return (c : cs, s')
         else return ("", s)

scanId :: Char -> Input -> Except String (TokenPos, T.Text, Input)
scanId x s =
  scanIdImpl (step s) >>= \(xs, s') ->
    let p = s ^. position . path
        r = s ^. position . row
        r' = s' ^. position . row
        c = s ^. position . column
        c' = s' ^. position . column
    in return (TokenPos p (r, r') (c, c'), T.pack $ x : xs, s')

scanVarId :: Char -> Input -> Except String (Token, Input)
scanVarId x s =
  scanId x s >>= \(pos, txt, s') -> return (Token pos (TkVarId txt), s')

scanConId :: Char -> Input -> Except String (Token, Input)
scanConId x s =
  scanId x s >>= \(pos, txt, s') -> return (Token pos (TkConId txt), s')

scanNum :: Char -> Input -> Except String (Token, Input)
scanNum = scanUnknown

scanToken :: Char -> Input -> Except String (Token, Input)
scanToken x s =
  case Char.generalCategory x of
    Char.LowercaseLetter -> scanVarId x s
    Char.UppercaseLetter -> scanConId x s
    -- Char.TitlecaseLetter -> fail
    -- Char.ModifierLetter -> fail
    -- Char.OtherLetter -> fail
    -- Char.NonSpacingMark -> fail
    -- Char.SpacingCombiningMark -> fail
    -- Char.EnclosingMark -> fail
    Char.DecimalNumber -> scanNum x s
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
    _ -> scanUnknown x s

main :: IO ()
main =
  let x = scanSimple . T.pack $ "facR ( n ) :: Int → Int"
  in case x of
       (Right _) -> print x >> return ()
       (Left err) -> putStrLn err
