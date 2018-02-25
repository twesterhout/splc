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


source1 =
  "main()::->Void\n\
  \{\n\
  \    if( True )// hello world!\n\
  \    {\n\
  \        /* Tralala! sdf\n\
  \        Int a = 10;\n\
  \        */\n\
  \        Int a = 10;\n\
  \        print(a);\n\
  \    }\n\
  \    else\n\
  \    {\n\
  \        print(100);\n\
  \    }\n\
  \}// sdf;lkj"

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
           Right [Token (tkPos (1, 1) (4, 8)) (TkVarId "abc2")]
         scanSimple "// hello\n\
                    \ ψα2β //     \r\n\f" `shouldBe`
         --          123456789
           Right [Token (tkPos (2, 2) (2, 6)) (TkVarId "ψα2β")]
    it "Recognises integer literals" $
      do scanSimple " ψα2β+ 00432-;//     " `shouldBe`
         --          123456789   13
           Right [ Token (tkPos (1, 1) (2, 6)) (TkVarId "ψα2β")
                 , Token (tkPos (1, 1) (6, 7)) (TkOther '+')
                 , Token (tkPos (1, 1) (8, 13)) (TkInt 432)
                 , Token (tkPos (1, 1) (13, 14)) (TkOther '-')
                 , Token (tkPos (1, 1) (14, 15)) (TkOther ';')]
    it "Detects overflow in integer literals" $
      do scanSimple " α + 11123456789876543211123  " `shouldBe`
         --          123456789
         -- maxBount::Int  9223372036854775807
           Left (LexError (SourcePos "" (unsafePos 1) (unsafePos 26))
                          "Detected overflow in 'Int' literal.")
         scanSimple " α + 1123456789876543211  " `shouldBe`
         --               9223372036854775807
         --          123456789
           Right [ Token (tkPos (1, 1) (2, 3)) (TkVarId "α")
                 , Token (tkPos (1, 1) (4, 5)) (TkOther '+')
                 , Token (tkPos (1, 1) (6, 25)) (TkInt 1123456789876543211)]
    it "Dummy" $
      do let x = scanSimple source1
         isRight x `shouldBe` True

