{-# LANGUAGE ExistentialQuantification #-}

module SimpleDefs where

import Data.IORef
import System.Console.Readline (readline, addHistory)
import Control.Monad.Error.Class
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            -- | Float Float
            -- | Rational Rational
            -- | Complex (Complex Float)
            -- | Char Char
            | String String
            | Bool Bool
        deriving (Eq)

instance Show LispVal where
  show (String str) = "\"" ++ str ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Number num) = show num
  show (Atom atm) = atm
  show (List lst) = "(" ++ unWordsList lst ++ ")"
  show (DottedList lst val) = "(" ++ unWordsList lst ++ " . " ++ show val ++ ")"

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (Parser parseError) = "Parse error at: " ++ show parseError
  show (TypeMismatch expected found) = "Couldn't match type " ++ expected ++ " with ecountered " ++ show found
  show (NotFunction message func) = message ++ ": " ++ func
  show (NumArgs expected found ) = "Expected " ++ (show expected) ++ " args: found values " ++ (unWordsList found)

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type Env = IORef [(String, IORef LispVal)]

type ThrowsError = Either LispError

data Unpacker = forall a . Eq a => LispUnpacker (LispVal -> ThrowsError a)

type IOThrowsError = ErrorT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

unpackersList :: [Unpacker]
unpackersList = [LispUnpacker unpackNum,
                 LispUnpacker unpackBool,
                 LispUnpacker unpackString]

unWordsList :: [LispVal] -> String
unWordsList = unwords . map show

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum x@(String str) = let lst = reads str :: [(Integer, String)] in
                         if null lst
                         then throwError $ TypeMismatch "Number" x
                         else return $ fst . head $ lst
unpackNum nan = throwError $ TypeMismatch "Number" nan

unpackString :: LispVal -> ThrowsError String
unpackString (String str) = return str
unpackString nas = throwError $ TypeMismatch "String" nas

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool x@(String str)
  | str == "True" = return True
  | str == "False" = return False
  | otherwise = throwError $ TypeMismatch "Boolean" x
unpackBool nab = throwError $ TypeMismatch "Boolean" nab

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (LispUnpacker f) = do a <- f x
                                       b <- f y
                                       return $ a == b
                                    `catchError` (const . return $ False)

readPrompt :: String -> IO String
readPrompt prompt = do str <- readline prompt
                       case str of
                         Nothing -> return "quit"
                         (Just str) -> addHistory str >> return str

untill_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
untill_ pred prompt action = do x <- prompt
                                if pred x
                                then return ()
                                else action x >> untill_ pred prompt action
