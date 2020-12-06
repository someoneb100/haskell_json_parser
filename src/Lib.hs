module Lib where

import Data.List
import Data.Char
import Control.Applicative


data JsonValue = JsonNull
               | JsonBool   Bool
               | JsonNumber Float
               | JsonString String
               | JsonArray  [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving Eq

instance Show JsonValue where
        show JsonNull       = "null"
        show (JsonBool b)   = show b
        show (JsonNumber f) = if (fromIntegral rf :: Float) == f then show rf else show f
                               where rf = round f
        show (JsonString s) = s
        show (JsonArray xs) = '[' : (intercalate ", " $ map show xs) ++ "]"
        show (JsonObject o) = '{' : (intercalate ", " $ map sp o) ++ "}"
                                where sp (name, val) = name ++ " : " ++ (show val)




newtype Parser a = Parser {
                          runParser :: String -> Maybe (String, a)
                          }

instance Functor Parser where
        fmap f (Parser p) = Parser $ \input -> do
                                     (input', val) <- p input
                                     Just (input', f val)

instance Applicative Parser where
        pure x = Parser $ \input -> Just(input, x)
        (Parser p1) <*> (Parser p2) =  Parser $ \input -> do
                                                (input', f) <- p1 input
                                                (input'', val) <- p2 input'
                                                Just (input'', f val)

instance Alternative Parser where
        empty = Parser $ \_ -> Nothing
        (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input




takeChar :: Parser Char
takeChar = Parser $ \input -> if null input then Nothing else Just (tail input, head input)

parseChar :: Char -> Parser Char
parseChar x = Parser f
        where f [] = Nothing
              f (y:ys)
                | y == x = Just (ys, y)
                | otherwise = Nothing

parseString :: String -> Parser String
parseString s = sequenceA $ map parseChar s

parseSpan :: (Char -> Bool) -> Parser String
parseSpan f = Parser $ \input -> let (val, rest) = span f input
                                 in Just (rest, val)

parseSpace :: Parser String
parseSpace = parseSpan isSpace

parseNotNull :: Parser [a] -> Parser [a]
parseNotNull (Parser p) = Parser $ \input -> do
                                   (input', xs) <- p input
                                   if null xs then Nothing else Just (input', xs)

parseSign :: Parser String
parseSign = "" <$ parseString "+" <|> parseString "-" <|> pure ""

parseInt :: Parser String
parseInt = (++) <$> parseSign <*> (parseNotNull $ parseSpan isDigit)

parseFloat :: Parser String
parseFloat = (++) <$> parseInt <*> (optional <|> pure "")
               where optional = (:) <$> (parseChar '.' <|> parseChar 'e') <*> parseInt


parseSeparator :: Parser a -> Parser b -> Parser [b]
parseSeparator sep elem = (:) <$> elem <*> many (sep *> elem) <|> pure []



jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ parseString "null"

jsonBool :: Parser JsonValue
jsonBool = (JsonBool True) <$ parseString "true" 
       <|> (JsonBool False) <$ parseString "false"

jsonNumber :: Parser JsonValue
jsonNumber = fmap jint parseFloat
        where jint = JsonNumber . read

jsonString :: Parser JsonValue
jsonString = parseChar '"' *> jstr <* parseChar '"'
        where jstr = fmap JsonString $ parseSpan (/= '"')


jsonArray :: Parser JsonValue
jsonArray = fmap JsonArray $ parseChar '[' *> parseSpace *> jarray <* parseSpace <* parseChar ']'
        where jarray = parseSeparator (parseSpace *> parseChar ',' <* parseSpace) jsonValue

jsonObject :: Parser JsonValue
jsonObject = fmap JsonObject $ parseChar '{' *> parseSpace *> jobj <* parseSpace <* parseChar '}'
        where jobj = parseSeparator sep val
                where sep = parseSpace *> parseChar ',' <* parseSpace
                      val = (\key _ value -> (key, value)) <$> str <*> colon <*> jsonValue
                              where colon = parseSpace *> parseChar ':' <* parseSpace
                                    str   = fmap show jsonString
