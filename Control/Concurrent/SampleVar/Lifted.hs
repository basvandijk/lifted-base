{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{- |
Module      :  Control.Concurrent.SampleVar.Lifted
Copyright   :  Liyang HU, Bas van Dijk
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental

This is a wrapped version of "Control.Concurrent.SampleVar" with types
generalised from 'IO' to all monads in 'MonadBase'.
-}

module Control.Concurrent.SampleVar.Lifted
    ( SampleVar
    , newEmptySampleVar
    , newSampleVar
    , emptySampleVar
    , readSampleVar
    , writeSampleVar
    , isEmptySampleVar
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.SampleVar ( SampleVar )
import qualified Control.Concurrent.SampleVar as SampleVar
import Data.Bool ( Bool )
import System.IO ( IO )
import Prelude ( (.) )

-- from transformers-base:
import Control.Monad.Base ( MonadBase, liftBase )

#include "inlinable.h"

--------------------------------------------------------------------------------
-- * SampleVars
--------------------------------------------------------------------------------

-- | Generalized version of 'SampleVar.newEmptySampleVar'.
newEmptySampleVar :: MonadBase IO m => m (SampleVar a)
newEmptySampleVar = liftBase SampleVar.newEmptySampleVar
{-# INLINABLE newEmptySampleVar #-}

-- | Generalized version of 'SampleVar.newSampleVar'.
newSampleVar :: MonadBase IO m => a -> m (SampleVar a)
newSampleVar = liftBase . SampleVar.newSampleVar
{-# INLINABLE newSampleVar #-}

-- | Generalized version of 'SampleVar.emptySampleVar'.
emptySampleVar :: MonadBase IO m => SampleVar a -> m ()
emptySampleVar = liftBase . SampleVar.emptySampleVar
{-# INLINABLE emptySampleVar #-}

-- | Generalized version of 'SampleVar.readSampleVar'.
readSampleVar :: MonadBase IO m => SampleVar a -> m a
readSampleVar = liftBase . SampleVar.readSampleVar
{-# INLINABLE readSampleVar #-}

-- | Generalized version of 'SampleVar.writeSampleVar'.
writeSampleVar :: MonadBase IO m => SampleVar a -> a -> m ()
writeSampleVar sv = liftBase . SampleVar.writeSampleVar sv
{-# INLINABLE writeSampleVar #-}

-- | Generalized version of 'SampleVar.isEmptySampleVar'.
isEmptySampleVar :: MonadBase IO m => SampleVar a -> m Bool
isEmptySampleVar = liftBase . SampleVar.isEmptySampleVar
{-# INLINABLE isEmptySampleVar #-}
