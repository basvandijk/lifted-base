{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{- |
Module      :  Control.Concurrent.QSemN.Lifted
Copyright   :  Liyang HU, Bas van Dijk
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental

This is a wrapped version of "Control.Concurrent.QSemN" with types
generalised from 'IO' to all monads in 'MonadBase'.
-}

module Control.Concurrent.QSemN.Lifted
    ( QSemN
    , newQSemN
    , waitQSemN
    , signalQSemN
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.QSemN ( QSemN )
import qualified Control.Concurrent.QSemN as QSemN
import Data.Int ( Int )
import System.IO ( IO )

-- from base-unicode-symbols:
import Prelude ( (.) )

-- from transformers-base:
import Control.Monad.Base ( MonadBase, liftBase )

#include "inlinable.h"

--------------------------------------------------------------------------------
-- * QSemNs
--------------------------------------------------------------------------------

-- | Generalized version of 'QSemN.newQSemN'.
newQSemN :: MonadBase IO m => Int -> m QSemN
newQSemN = liftBase . QSemN.newQSemN
{-# INLINABLE newQSemN #-}

-- | Generalized version of 'QSemN.waitQSemN'.
waitQSemN :: MonadBase IO m => QSemN -> Int -> m ()
waitQSemN sem = liftBase . QSemN.waitQSemN sem
{-# INLINABLE waitQSemN #-}

-- | Generalized version of 'QSemN.signalQSemN'.
signalQSemN :: MonadBase IO m => QSemN -> Int -> m ()
signalQSemN sem = liftBase . QSemN.signalQSemN sem
{-# INLINABLE signalQSemN #-}

