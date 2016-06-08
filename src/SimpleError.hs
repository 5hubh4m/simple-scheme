{-# LANGUAGE FlexibleContexts #-}

module SimpleError where

import SimpleDefs
import SimpleParser
import Control.Monad.Error.Class

trapError action = catchError action $ return . show

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
