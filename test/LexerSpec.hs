{-# LANGUAGE OverloadedStrings #-}


-- |
-- Module      : LexerSpec
-- Description : Tests for Lexer
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : MIT
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental

module LexerSpec where

import Data.Either
import Test.Hspec
import Text.Megaparsec.Pos (unsafePos)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Types
import Lexer

main :: IO ()
main = hspec spec

showPretty :: Either LexError [Token] -> IO ()
showPretty (Left err) = P.putDoc . P.pretty $ err
showPretty (Right xs) = mapM_ print xs

tkPos :: (Int, Int) -> (Int, Int) -> TokenPos
tkPos (r, r') (c, c') = let i2p = unsafePos . fromIntegral
                         in TokenPos "" (i2p r, i2p r') (i2p c, i2p c')

spec :: Spec
spec = do
  describe "scanSimple" $ do
    it "Recognises variable identifiers" $
      do scanSimple "   abc2 " `shouldBe`
         --          12345678
           Right [Token (tkPos (1, 1) (4, 8)) (TkId "abc2")]
         scanSimple "// hello\n\
                    \ ψα2β //     \r\n\f" `shouldBe`
         --          123456789
           Right [Token (tkPos (2, 2) (2, 6)) (TkId "ψα2β")]
    it "Recognises keywords" $
      do scanSimple " return a; // returns an a " `shouldBe`
         --          12345678
           Right [ Token (tkPos (1, 1) (2,   8)) (TkKeyword KwReturn)
                 , Token (tkPos (1, 1) (9,  10)) (TkId "a")
                 , Token (tkPos (1, 1) (10, 11)) (TkPunctuation PtSemiColon)
                 ]
         scanSimple " returna; // does not return a " `shouldBe`
         --          123456789
           Right [ Token (tkPos (1, 1) (2,  9)) (TkId "returna")
                 , Token (tkPos (1, 1) (9, 10)) (TkPunctuation PtSemiColon)
                 ]
         scanSimple " if( " `shouldBe`
         --          12345678
           Right [ Token (tkPos (1, 1) (2,  4)) (TkKeyword KwIf)
                 , Token (tkPos (1, 1) (4,  5)) (TkPunctuation PtLParen)
                 ]
         scanSimple " if (True){\n\
                    \}else{;} " `shouldBe`
         --          123456789012345
           Right [ Token (tkPos (1, 1) (2,   4)) (TkKeyword KwIf)
                 , Token (tkPos (1, 1) (5,   6)) (TkPunctuation PtLParen)
                 , Token (tkPos (1, 1) (6,  10)) (TkBool True)
                 , Token (tkPos (1, 1) (10, 11)) (TkPunctuation PtRParen)
                 , Token (tkPos (1, 1) (11, 12)) (TkPunctuation PtLBrace)
                 , Token (tkPos (2, 2) (1,   2)) (TkPunctuation PtRBrace)
                 , Token (tkPos (2, 2) (2,   6)) (TkKeyword KwElse)
                 , Token (tkPos (2, 2) (6,   7)) (TkPunctuation PtLBrace)
                 , Token (tkPos (2, 2) (7,   8)) (TkPunctuation PtSemiColon)
                 , Token (tkPos (2, 2) (8,   9)) (TkPunctuation PtRBrace)
                 ]
    it "Recognises integer literals" $
      do scanSimple " ψα2β+ 00432-;//     " `shouldBe`
         --          123456789   13
           Right [ Token (tkPos (1, 1) (2,   6)) (TkId "ψα2β")
                 , Token (tkPos (1, 1) (6,   7)) (TkOperator OpPlus)
                 , Token (tkPos (1, 1) (8,  13)) (TkInt 432)
                 , Token (tkPos (1, 1) (13, 14)) (TkOperator OpMinus)
                 , Token (tkPos (1, 1) (14, 15)) (TkPunctuation PtSemiColon)
                 ]
    it "Detects overflow in integer literals" $
      do scanSimple " α + 11123456789876543211123  " `shouldBe`
         --          123456789
         -- maxBount::Int  9223372036854775807
           Left (LexError (SourcePos "" (unsafePos 1) (unsafePos 26))
                          "Detected overflow in 'Int' literal.")
         scanSimple " α + 1123456789876543211  " `shouldBe`
         --               9223372036854775807
         --          123456789
           Right [ Token (tkPos (1, 1) (2,  3)) (TkId "α")
                 , Token (tkPos (1, 1) (4,  5)) (TkOperator OpPlus)
                 , Token (tkPos (1, 1) (6, 25)) (TkInt 1123456789876543211)
                 ]

