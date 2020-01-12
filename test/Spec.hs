{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Use isNothing" -}

import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           Data.Maybe
import           Data.Char
import           Lib

main :: IO ()
main = hspec $ do
  describe "Functor" $ do
    it "can penetrate the Parser with fmap" $ do
      runParser (fmap pred (charP 'n')) "nice" `shouldBe` Just ("ice", 'm')

  describe "charP" $ do
    it "parses a single character" $ do
      runParser (charP 'n') "n" `shouldBe` Just ("", 'n')
    it "is nothing" $ runParser (charP 'n') "hello" `shouldBe` Nothing
    it "parses any single character" $ property $ \case
      ""         -> runParser (charP 'a') [] == Nothing
      s@(x : xs) -> runParser (charP x) s == Just (xs, x)

  describe "stringP" $ do
    it "matches a string" $ do
      runParser (stringP "null") "null" `shouldBe` Just ("", "null")
    it "returns the remainders" $ do
      runParser (stringP "null") "null, otherstuff"
        `shouldBe` Just (", otherstuff", "null")
    it "is nothing" $ do
      runParser (stringP "nil") "null" `shouldBe` Nothing

  describe "spanP" $ do
    it "matches a string with a predicate" $ do
      runParser (spanP isDigit) "123hello" `shouldBe` Just ("hello", "123")

  describe "jsonNull" $ do
    it "matches null" $ do
      runParser jsonNull "null" `shouldBe` Just ("", JsonNull)

  describe "jsonBool" $ do
    it "matches true" $ do
      runParser jsonBool "true" `shouldBe` Just ("", JsonBool True)
    it "matches false" $ do
      runParser jsonBool "false" `shouldBe` Just ("", JsonBool False)

  describe "jsonNumber" $ do
    it "matches numbers" $ do
      runParser jsonNumber "123hello" `shouldBe` Just ("hello", JsonNumber 123)
    it "isNothing" $ do
      runParser jsonNumber "hello" `shouldBe` Nothing

  describe "jsonString" $ do
    it "matches numbers" $ do
      runParser jsonString "\"hello\"" `shouldBe` Just ("", JsonString "hello")
    it "isNothing" $ do
      runParser jsonString "\"hello" `shouldBe` Nothing
      runParser jsonString "hello\"" `shouldBe` Nothing
