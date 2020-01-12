module Lib where

import           Data.Char
import           Control.Applicative

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Int
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

newtype Parser a = Parser { runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap fn (Parser p) = Parser $ \input -> do
    (input', a) <- p input
    Just (input', fn a)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input' , fn) <- p1 input
    (input'', a ) <- p2 input'
    Just (input'', fn a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser fn where
  fn (y : ys) | y == x = Just (ys, x)
  fn _                 = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP fn = Parser
  $ \input -> let (token, input') = span fn input in Just (input', token)

spanPR :: (Char -> Bool) -> Parser String
spanPR = notNull . spanP

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  s <- p input
  if null (snd s) then Nothing else Just s

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool =
  (JsonBool True <$ stringP "true") <|> (JsonBool False <$ stringP "false")

jsonNumber :: Parser JsonValue
jsonNumber = f <$> spanPR isDigit where f ds = JsonNumber $ read ds

jsonString :: Parser JsonValue
jsonString = JsonString <$> (charP '"' *> spanP (/= '"') <* charP '"')

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> ws *> pairs <* ws <* charP '}') where
  pairs = sepBy (ws *> charP ',' <* ws) pair
  pair =
    (\key _ value -> (key, value))
      <$> (charP '"' *> spanP (/= '"') <* charP '"')
      <*> (ws *> charP ':' <* ws)
      <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue =
  jsonNull
    <|> jsonBool
    <|> jsonNumber
    <|> jsonString
    <|> jsonArray
    <|> jsonObject
