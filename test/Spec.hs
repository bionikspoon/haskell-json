{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Use isNothing" -}

import           Test.Hspec
import           Test.QuickCheck                ( property )
import           Control.Exception              ( evaluate )
import           Data.Char                      ( isDigit )
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
    it "can be empty" $ do
      runParser jsonString "\"\"" `shouldBe` Just ("", JsonString "")

    it "isNothing" $ do
      runParser jsonString "\"hello" `shouldBe` Nothing
      runParser jsonString "hello\"" `shouldBe` Nothing

  describe "jsonArray" $ do
    it "can be empty" $ do
      runParser jsonArray "[]" `shouldBe` Just ("", JsonArray [])
    it "can contain simple values" $ do
      runParser jsonArray "[null,null,null]"
        `shouldBe` Just ("", JsonArray [JsonNull, JsonNull, JsonNull])
    it "can contain strings" $ do
      runParser jsonArray "[\"a\",\"b\",\"c\"]" `shouldBe` Just
        ("", JsonArray [JsonString "a", JsonString "b", JsonString "c"])
    it "can handle spaces around the comma" $ do
      runParser jsonArray "[null , null , null]"
        `shouldBe` Just ("", JsonArray [JsonNull, JsonNull, JsonNull])
    it "can handle spaces around the brackets" $ do
      runParser jsonArray "[ null , null , null ]"
        `shouldBe` Just ("", JsonArray [JsonNull, JsonNull, JsonNull])
    it "can be polymorphic" $ do
      runParser
          jsonArray
          "[ true , false , null, \"hello\", 123, [ \"nested\" ], {\"test\": \"success\"} ]"
        `shouldBe` Just
                     ( ""
                     , JsonArray
                       [ JsonBool True
                       , JsonBool False
                       , JsonNull
                       , JsonString "hello"
                       , JsonNumber 123
                       , JsonArray [JsonString "nested"]
                       , JsonObject [("test", JsonString "success")]
                       ]
                     )
  describe "jsonArray" $ do
    it "can be empty" $ do
      runParser jsonObject "{}" `shouldBe` Just ("", JsonObject [])
    it "can have key value pairs" $ do
      runParser jsonObject "{\"test\": \"success\", \"hello\": \"world\"}"
        `shouldBe` Just
                     ( ""
                     , JsonObject
                       [ ("test" , JsonString "success")
                       , ("hello", JsonString "world")
                       ]
                     )
    it "can handle spaces" $ do
      runParser jsonObject "{ \"hello\" :  true    }"
        `shouldBe` Just ("", JsonObject [("hello", JsonBool True)])
    it "supports json values" $ do
      runParser
          jsonObject
          "{\"test\": [{\"id\": 13, \"name\": \"Joe Sixpack\"}, {\"id\": 17, \"name\": \"Jane User\"}]}"
        `shouldBe` Just
                     ( ""
                     , JsonObject
                       [ ( "test"
                         , JsonArray
                           [ JsonObject
                             [ ("id"  , JsonNumber 13)
                             , ("name", JsonString "Joe Sixpack")
                             ]
                           , JsonObject
                             [ ("id"  , JsonNumber 17)
                             , ("name", JsonString "Jane User")
                             ]
                           ]
                         )
                       ]
                     )
