module Main where

import SimpleDefs
import SimpleParser
import SimpleEvaluator
import SimpleError
import System.Environment (getArgs)
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec (parse)
import Control.Monad.Error.Class (throwError)

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runREPL
            1 -> evalAndPrintExpr . head $ args
            otherwise -> putStrLn "Program takes only 0 or 1 argument"

runREPL :: IO ()
runREPL = untill_ ((==) "quit") (readPrompt "Lisp>> ") evalAndPrintExpr

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

evalExpr :: String -> IO String
evalExpr expr = return . extractValue . trapError $ liftM show $ readExpr expr >>= eval

evalAndPrintExpr :: String -> IO ()
evalAndPrintExpr expr = evalExpr expr >>= putStrLn
