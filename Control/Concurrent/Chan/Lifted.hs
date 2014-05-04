{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{- |
Module      :  Control.Concurrent.Chan.Lifted
Copyright   :  Liyang HU, Bas van Dijk
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental

This is a wrapped version of "Control.Concurrent.Chan" with types
generalised from 'IO' to all monads in 'MonadBase'.

'Chan.unGetChan' and 'Chan.isEmptyChan' are deprecated in @base@, therefore
they are not included here. Use 'Control.Concurrent.STM.TVar' instead.
-}

module Control.Concurrent.Chan.Lifted
    ( Chan
    , newChan
    , writeChan
    , readChan
    , dupChan
    , getChanContents
    , writeList2Chan
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.Chan ( Chan )
import qualified Control.Concurrent.Chan as Chan
import System.IO ( IO )

-- from base-unicode-symbols:
import Prelude ( (.) )

-- from transformers-base:
import Control.Monad.Base ( MonadBase, liftBase )

#include "inlinable.h"

--------------------------------------------------------------------------------
-- * Chans
--------------------------------------------------------------------------------

-- | Generalized version of 'Chan.newChan'.
newChan :: MonadBase IO m => m (Chan a)
newChan = liftBase Chan.newChan
{-# INLINABLE newChan #-}

-- | Generalized version of 'Chan.writeChan'.
writeChan :: MonadBase IO m => Chan a -> a -> m ()
writeChan chan = liftBase . Chan.writeChan chan
{-# INLINABLE writeChan #-}

-- | Generalized version of 'Chan.readChan'.
readChan :: MonadBase IO m => Chan a -> m a
readChan = liftBase . Chan.readChan
{-# INLINABLE readChan #-}

-- | Generalized version of 'Chan.dupChan'.
dupChan :: MonadBase IO m => Chan a -> m (Chan a)
dupChan = liftBase . Chan.dupChan
{-# INLINABLE dupChan #-}

-- | Generalized version of 'Chan.getChanContents'.
getChanContents :: MonadBase IO m => Chan a -> m [a]
getChanContents = liftBase . Chan.getChanContents
{-# INLINABLE getChanContents #-}

-- | Generalized version of 'Chan.writeList2Chan'.
writeList2Chan :: MonadBase IO m => Chan a -> [a] -> m ()
writeList2Chan chan = liftBase . Chan.writeList2Chan chan
{-# INLINABLE writeList2Chan #-}

