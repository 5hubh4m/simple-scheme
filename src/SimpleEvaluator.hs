module SimpleEvaluator where

import SimpleDefs
import SimpleParser
import SimpleError
import SimpleEnv
import Control.Monad (mapM, liftM)
import Control.Monad.Error.Class

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ x@(String _) = return x
eval _ x@(Number _) = return x
eval _ x@(Bool _) = return x
eval env (Atom ref) = getVar env ref
eval _ (List [Atom "quote", x]) = return x
eval env (List [Atom "if", pred, conseq, alt]) = do result <- eval env pred
                                                    if result == Bool False
                                                    then eval env alt
                                                    else eval env conseq
eval env (List [Atom "define", Atom ref, val]) = eval env val >>= defineVar env ref
eval env (List [Atom "set!", Atom ref, val]) = eval env val >>= setVar env ref
eval env (List ((Atom func):args)) = mapM (eval env) args >>= (liftThrows . apply func)
--eval env x@(List xs) = mapM (eval env) xs >>= return . List
--eval env (DottedList xs y) = do lst <- mapM (eval env) xs
--                                dot <- eval env y
--                                return $ DottedList lst dot
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" func)
  ($ args)
  (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", add),
              ("-", sub),
              ("*", numBinOp (*)),
              ("/", numBinOp div),
              ("remainder", numBinOp rem),
              ("quotient", numBinOp quot),
              ("mod", numBinOp mod),
              ("<", boolOp unpackNum (<)),
              ("<=", boolOp unpackNum (<=)),
              (">", boolOp unpackNum (>)),
              (">", boolOp unpackNum (>=)),
              ("=", boolOp unpackNum (==)),
              ("/=", boolOp unpackNum (/=)),
              ("&&", boolOp unpackBool (&&)),
              ("||", boolOp unpackBool (||)),
              ("string=?", boolOp unpackString (==)),
              ("string?", boolOp unpackString (>)),
              ("string<=?", boolOp unpackString (<=)),
              ("string>=?", boolOp unpackString (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eq),
              ("eqv?", eq),
              ("equal?", equal)]

add, sub :: ([LispVal] -> ThrowsError LispVal)
add [] = throwError $ NumArgs 2 []
add [x] = unpackNum x >>= return . Number
add xs = numBinOp (+) xs

sub [] = throwError $ NumArgs 2 []
sub [x] = unpackNum x >>= return . Number . ((-) 0)
sub xs = numBinOp (-) xs

boolOp :: (LispVal -> ThrowsError a)
       -> (a -> a -> Bool)
       -> ([LispVal] -> ThrowsError LispVal)
boolOp _ _ [] =  throwError $ NumArgs 2 []
boolOp _ _ x@[_] =  throwError $ NumArgs 2 x
boolOp unpack f args
  | length args /= 2 = throwError $ NumArgs 2 args
  | otherwise = do left <- unpack $ args !! 0
                   right <- unpack $ args !! 1
                   return . Bool $ left `f` right

numBinOp :: (Integer -> Integer -> Integer)
         -> ([LispVal] -> ThrowsError LispVal)
numBinOp _ [] = throwError $ NumArgs 2 []
numBinOp _ x@[_] = throwError $ NumArgs 2 x
numBinOp f args = do xs <- mapM unpackNum args
                     return . Number $ foldl1 f xs

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArgs] = throwError $ TypeMismatch "List" badArgs
car badArgs = throwError $ NumArgs 1 badArgs

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList (_ : x : xs) y] = return $ DottedList (x:xs) y
cdr [DottedList [x] y] = return y
cdr [badArgs] = throwError $ TypeMismatch "List" badArgs
cdr badArgs = throwError $ NumArgs 1 badArgs

cons :: [LispVal] -> ThrowsError LispVal
cons (x : [List xs]) = return $ List (x : xs)
cons (x : [DottedList xs y]) = return $ DottedList (x:xs) y
cons [x, y] = return $ DottedList [x] y
cons badArgs = throwError $ NumArgs 2 badArgs

eq :: [LispVal] -> ThrowsError LispVal
eq [List xs, List ys] = do lst <- mapM (\(a, b) -> eq [a, b] >>= unpackBool)
                                       (zip xs ys)
                           return . Bool $ and lst
eq [DottedList xs x, DottedList ys y] = eq [List (x : xs), List (y : ys)]
eq [Number a, Number b] = return . Bool $ a == b
eq [String a, String b] = return . Bool $ a == b
eq [Bool a, Bool b] = return . Bool $ a == b
eq [Atom a, Atom b] = return . Bool $ a  == b
eq [_, _] = return $ Bool False
eq badArgs = throwError $ NumArgs 2 badArgs

equal :: [LispVal] -> ThrowsError LispVal
equal [x, y] = do primitiveEquals <- liftM or $ mapM (unpackEquals x y)
                                                     unpackersList

                  eqvEquals <- eq [x, y] >>= unpackBool
                  return . Bool $ (primitiveEquals || eqvEquals)
equal badArgList = throwError $ NumArgs 2 badArgList
