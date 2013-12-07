module Interpreter.Parsing where

import Interpreter.Common
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Complex
import Data.Ratio
import Control.Monad.Error (throwError)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
    
readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseQuasiQuoted
        <|> parseUnQuote
        <|> try parseComplex
        <|> try parseRatio
        <|> try parseReal
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x
        
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"
        
spaces :: Parser ()
spaces = skipMany1 space

parseBool :: Parser LispVal
parseBool = do
        char '#'
        (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

escapedChars :: Parser Char
escapedChars = do
        char '\\'
        x <- oneOf "\\\"nrt"
        return $ case x of
                '\\' -> x
                '"'  -> x
                'n'  -> '\n'
                'r'  -> '\r'
                't'  -> '\t'

parseString :: Parser LispVal
parseString = do
        char '"'
        x <- many $ noneOf "\"\\" <|> escapedChars
        char '"'
        return $ String x

parseChar :: Parser LispVal
parseChar = parseCharacter <|> parseCharName
                        
parseCharacter :: Parser LispVal
parseCharacter = do
        try $ string "#\\"
        (letter <|> digit <|> symbol) >>= return . Char

parseCharName :: Parser LispVal
parseCharName = do
        try $ string "#\\"
        x <- string "space" <|> string "newline"
        return $ case x of
                        "space"  -> Char ' '
                        "newline"-> Char '\n'  
                
parseAtom :: Parser LispVal
parseAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $ Atom atom
        
parseReal :: Parser LispVal
parseReal = do
        x <- many1 digit
        char '.'
        y <- many1 digit
        return $ Real (fst.head$readFloat (x++"."++y))
   
toDouble :: LispVal -> Double
toDouble(Real f) = f
toDouble(Number n) = fromIntegral n

parseRatio :: Parser LispVal
parseRatio = do
        x <- many1 digit
        char '/'
        y <- many1 digit
        return $ Ratio ((read x) % (read y))
                       
parseComplex :: Parser LispVal
parseComplex = do
        re <- (try parseReal <|> parseDigital1)
        imSign <- oneOf "+-"
        im <- do {x<-(try parseReal <|> parseDigital1); i<-char 'i'; return x}
              <|> do {char 'i'; return $ Number 1}
        return $ Complex $ toDouble re :+ (if imSign=='+' then 1 else -1) * toDouble im
                  
parseNumber :: Parser LispVal
parseNumber = parseDigital1
          <|> parseDigital2
          <|> parseHex
          <|> parseOct
          <|> parseBin

parseNumberWithbase :: String -> Parser Char -> (String -> Integer)-> Parser LispVal
parseNumberWithbase prefix parser reader = do
        try $ string prefix
        many1 parser >>= (return . Number . reader)
         
parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= (return . Number . read)

parseDigital2 :: Parser LispVal
parseDigital2 = parseNumberWithbase "#d" digit read

parseHex :: Parser LispVal
parseHex = parseNumberWithbase "#x" digit hex2dig

parseOct :: Parser LispVal
parseOct = parseNumberWithbase "#o" digit oct2dig

parseBin :: Parser LispVal
parseBin = parseNumberWithbase "#b" digit bin2dig

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2*digint + (if x=='0' then 0 else 1) in
                         bin2dig' old xs

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
        head <- endBy parseExpr spaces
        tail <- char '.' >> spaces >> parseExpr
        return $ DottedList head tail
        
parseQuoted :: Parser LispVal
parseQuoted = do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
        char '`'
        x <- parseExpr
        return $ List [Atom "quasiquote", x]
        
parseUnQuote :: Parser LispVal
parseUnQuote = do
        char ','
        x <- parseExpr
        return $ List [Atom "unquote", x]