module SimpleEnv where

import SimpleDefs
import SimpleError
import Data.IORef
import Control.Monad (mapM)
import Control.Monad.Error.Class
import Control.Monad.Error

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action)
  >>= return . extractValue


isBound :: Env -> String -> IO Bool
isBound envRef ref = do env <- readIORef envRef
                        return $ case lookup ref env of
                          Nothing -> False
                          (Just _) -> True

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef ref = do env <- liftIO $ readIORef envRef
                       case lookup ref env of
                         Nothing -> throwError
                           $ UnboundVar "Getting an unbound variable" ref
                         (Just val) -> liftIO . readIORef $ val

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef ref val = do env <- liftIO $ readIORef envRef
                           case lookup ref env of
                             Nothing -> throwError
                               $ UnboundVar "Getting an unbound variable" ref
                             (Just itm) -> liftIO $ writeIORef itm val
                           return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef ref val = do defined <- liftIO $ isBound envRef ref
                              if defined
                              then setVar envRef ref val
                              else liftIO $ do env <- liftIO $ readIORef envRef
                                               refVal <- newIORef val
                                               liftIO $ writeIORef envRef ((ref, refVal) : env)
                                               return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef vars = do env <- readIORef envRef
                          bindings <- mapM (\(ref, val) ->
                                             do refVal <- newIORef val
                                                return (ref, refVal)) vars
                          refBindings <- newIORef $ bindings ++ env
                          return refBindings
