module Main where

import SimpleDefs
import SimpleParser
import SimpleEvaluator
import SimpleError
import SimpleEnv
import System.Environment (getArgs)
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec (parse)
import Control.Monad.Error.Class (throwError)

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runREPL
            1 -> runOne $ head args
            otherwise -> putStrLn "Program takes only 0 or 1 argument"

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrintExpr expr

runREPL :: IO ()
runREPL = nullEnv >>= untill_ ((==) "quit") (readPrompt "Lisp >> ") . evalAndPrintExpr

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

evalExpr :: Env -> String -> IO String
evalExpr env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrintExpr :: Env -> String -> IO ()
evalAndPrintExpr env expr = evalExpr env expr >>= putStrLn
