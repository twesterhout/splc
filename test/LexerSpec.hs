{-# LANGUAGE OverloadedStrings #-}


-- |
-- Module      : LexerSpec
-- Description : Tests for Lexer
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : MIT
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental

module LexerSpec where

import Test.Hspec
import Lexer

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "scanSimple" $ do
    it "Recognises variables and types" $
      do let x = scanSimple "facR ( n ) :: Int → Int"
         print x
