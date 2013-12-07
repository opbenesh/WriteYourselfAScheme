{-# LANGUAGE ExistentialQuantification #-}
module Main where

import           Control.Monad
import           Interpreter.Common
import           Interpreter.Evaluating
import           Interpreter.Parsing
import           Interpreter.REPL
import           System.Environment
import           Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args