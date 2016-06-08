module SimpleParser where

import SimpleDefs
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readFloat, readHex, readOct)
import Data.Complex (Complex((:+)))
import Data.Char (toLower)
import Data.Ratio (Rational, (%))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

{-
parseChar :: Parser LispVal
parseChar = do string "#\\"
               c <- many1 letter
               return $ Char $ case map toLower c of
                   "space" -> ' '
                   "newline" -> '\n'
                   [x] -> x
-}

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ parseEscape <|> (noneOf "\\\"")
                 char '"'
                 return $ String x

parseEscape :: Parser Char
parseEscape = do char '\\'
                 c <- oneOf "\\\"nrt"
                 return $ case c of
                     '\\' -> c
                     '\"' -> c
                     'n' -> '\n'
                     'r' -> '\r'
                     't' -> '\t'

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many $ letter <|> digit <|> symbol
               let atom = first : rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False 
                 _ -> Atom atom

parseSimpleNumber :: Parser LispVal
parseSimpleNumber = many1 digit >>= return . Number . read

{-
parseRadixNumber :: Parser LispVal
parseRadixNumber = char '#' >>
                (   parseDecimal
                <|> parseOctal
                <|> parseHexadecimal
                <|> parseBinary
                )

parseDecimal, parseHexadecimal, parseBinary, parseOctal :: Parser LispVal
parseDecimal = do char 'd'
                  number <- many1 digit
                  return . Number . read $ number

parseHexadecimal = do char 'x'
                      number <- many $ digit <|> (oneOf "abcdef")
                      return . Number . fst . head . readHex $ number

parseBinary = do char 'b'
                 number <- many1 . oneOf $ "01"
                 return . Number . readBin $ number

parseOctal = do char 'o'
                number <- many1 . oneOf $ "01234567"
                return . Number . fst . head . readOct $ number

readBin :: String -> Integer
readBin bin = fst $ foldl (\(n, ix) x -> (n + (num x) * (2 ^ ix), ix - 1)) 
                          (0, (length bin) - 1)
                          bin where
                  num '0' = 0
                  num '1' = 1
-}
parseNumber :: Parser LispVal
parseNumber = parseSimpleNumber -- <|> parseRadixNumber

{- 
parseFloat :: Parser LispVal
parseFloat = do before <- many1 digit
                char '.'
                after <- many1 digit
                return . Float . fst . head . readFloat $ before ++ '.' : after

parseRational :: Parser LispVal
parseRational = do before <- fmap read $ many1 digit
                   char '/'
                   after <- fmap read $ many1 digit
                   return . Rational $ (before % after)

parseComplex :: Parser LispVal
parseComplex = do real <- fmap toFloat $ parseFloat <|> parseSimpleNumber
                  char '+'
                  imag <- fmap toFloat $ parseFloat <|> parseSimpleNumber
                  char 'i'
                  return . Complex $ (real :+ imag) where
                      toFloat (Float x) = x
                      toFloat (Number x) = fromInteger x
-}

parseQuoted :: Parser LispVal 
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     char '.'
                     many1 spaces
                     tail <- parseExpr
                     return $ DottedList head tail


parseExpr :: Parser LispVal
parseExpr = parseAtom
        -- <|> parseBool
        -- <|> parseChar
        <|> parseString
        -- <|> parseComplex
        -- <|> parseRational
        -- <|> parseFloat
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x
