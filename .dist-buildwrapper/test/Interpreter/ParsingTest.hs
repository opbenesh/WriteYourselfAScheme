{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Interpreter.ParsingTest where
import           Interpreter.Common
import           Interpreter.Parsing

import           Test.Framework
import           Test.Framework.TestTypes      (Assertion)
import           Text.ParserCombinators.Parsec (ParseError, Parser, parse)

assertParsingSuccess :: String -> Assertion
assertParsingSuccess input = case parse parseExpr "lisp" input of
        Left err -> assertFailure $ "No match: " ++ show err
        Right _ -> return ()

assertParsingFailure :: String -> Assertion
assertParsingFailure input = case parse parseExpr "lisp" input of
        Left _ -> return ()
        Right _ -> assertFailure $ "Accepted illegal string " ++ input

getParsedExpr :: String -> Either ParseError LispVal
getParsedExpr = parse parseExpr "lisp"

test_number = assertParsingSuccess "123"
test_hex = assertParsingSuccess "#x123A"

test_float = assertParsingSuccess "123.001"

test_bool = assertParsingSuccess "#t"

test_char = assertParsingSuccess "#\\newline"

test_complex = case getParsedExpr "1+i" of
        Right (Complex _) -> return ()
        _ -> assertFailure ""

test_list1 = case getParsedExpr "(1 2 3)" of
        Right (List _) -> return ()
        _ -> assertFailure ""

test_list2 = case getParsedExpr "()" of
        Right (List _) -> return ()
        _ -> assertFailure ""

