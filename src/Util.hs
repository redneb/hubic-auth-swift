module Util
    ( errorIO
    ) where

import Control.Monad.IO.Class
import Control.Exception

errorIO :: MonadIO m => String -> m a
errorIO = liftIO . throwIO . ErrorCall
{-# INLINE errorIO #-}
