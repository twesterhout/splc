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
import Lexer

main :: IO ()
main = hspec spec


source1 =
  "main()::->Void\n\
  \{\n\
  \    if( True )\n\
  \    {\n\
  \        Int a = 10;\n\
  \        print(a);\n\
  \    }\n\
  \    else\n\
  \    {\n\
  \        print(100);\n\
  \    }\n\
  \}"

showPretty :: Either String [Token] -> IO ()
showPretty (Left err) = putStrLn err
showPretty (Right xs) = mapM_ print xs


spec :: Spec
spec = do
  describe "scanSimple" $ do
    it "Recognises variables and types" $
      do let x = scanSimple source1
         showPretty x
         isRight x `shouldBe` True
