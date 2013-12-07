module Interpreter.Common where

import           Control.Monad.Error
import           Data.Complex
import           Data.List
import           Text.ParserCombinators.Parsec (ParseError)
import Data.IORef (IORef)
import System.IO (Handle)

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ErrorT LispError IO

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Real Double
             | Complex (Complex Double)
             | Ratio Rational
             | String String
             | Bool Bool
             | Char Char
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), 
                      body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

instance Show LispVal where
        show (Atom s) = s
        show (List xs) = "(" ++ (unwords . map show) xs ++ ")"
        show (DottedList xs x) = "(" ++ (unwords . map show) xs ++ "." ++ show x ++ ")"
        show (Number n) = show n
        show (Real f) = show f
        show (Complex c) = show c
        show (Ratio r) = show r
        show (String s) = "\"" ++ s ++ "\""
        show (Bool b) = if b then "#t" else "#f"
        show (Char c) = "#\\" ++ (show c) ++ "\""
        show (PrimitiveFunc _) = "<primitive>"
        show (Func {params = args, vararg = varargs, body = body, closure = env}) = 
                "(lambda (" ++ unwords (map show args) ++ 
                (case varargs of 
                        Nothing -> ""
                        Just arg -> " . " ++ arg) ++ ") ...)" 
        show (Port _) = "<IO port>"
        show (IOFunc _) = "<IO primitive>"

data LispError = NumArgs Integer [LispVal]
        | TypeMismatch String LispVal
        | Parser ParseError
        | BadSpecialForm String LispVal
        | NotFunction String String
        | UnboundVar String String
        | Default String

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected  ++ " args; found values " ++ (unwords . map show) found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type ThrowsError = Either LispError

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val    

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


