{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{- |
Module      :  Data.IORef
Copyright   :  Liyang HU, Bas van Dijk
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental

This is a wrapped version of "Data.IORef" with types
generalised from 'IO' to all monads in 'MonadBase'.
-}

module Data.IORef.Lifted
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    , modifyIORef
#if MIN_VERSION_base(4,6,0)
    , modifyIORef'
#endif
    , atomicModifyIORef
#if MIN_VERSION_base(4,6,0)
    , atomicModifyIORef'
    , atomicWriteIORef
#endif
    , mkWeakIORef
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.IORef ( IORef )
import qualified Data.IORef as R
import System.IO ( IO )
import System.Mem.Weak ( Weak )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers-base:
import Control.Monad.Base ( MonadBase, liftBase )

-- from monad-control:
import Control.Monad.Trans.Control ( MonadBaseControl, liftBaseDiscard )

#include "inlinable.h"

--------------------------------------------------------------------------------
-- * IORefs
--------------------------------------------------------------------------------

-- | Generalized version of 'R.newIORef'.
newIORef ∷ MonadBase IO m ⇒ a → m (IORef a)
newIORef = liftBase ∘ R.newIORef
{-# INLINABLE newIORef #-}

-- | Generalized version of 'R.readIORef'.
readIORef ∷ MonadBase IO m ⇒ IORef a → m a
readIORef = liftBase ∘ R.readIORef
{-# INLINABLE readIORef #-}

-- | Generalized version of 'R.writeIORef'.
writeIORef ∷ MonadBase IO m ⇒ IORef a → a → m ()
writeIORef r = liftBase ∘ R.writeIORef r
{-# INLINABLE writeIORef #-}

-- | Generalized version of 'R.modifyIORef'.
modifyIORef ∷ MonadBase IO m ⇒ IORef a → (a → a) → m ()
modifyIORef r = liftBase ∘ R.modifyIORef r
{-# INLINABLE modifyIORef #-}

-- | Generalized version of 'R.atomicModifyIORef'.
atomicModifyIORef ∷ MonadBase IO m ⇒ IORef a → (a → (a, b)) → m b
atomicModifyIORef r = liftBase ∘ R.atomicModifyIORef r
{-# INLINABLE atomicModifyIORef #-}

#if MIN_VERSION_base(4,6,0)
-- | Generalized version of 'R.modifyIORef''.
modifyIORef' ∷ MonadBase IO m ⇒ IORef a → (a → a) → m ()
modifyIORef' r = liftBase ∘ R.modifyIORef' r
{-# INLINABLE modifyIORef' #-}

-- | Generalized version of 'R.atomicModifyIORef''.
atomicModifyIORef' ∷ MonadBase IO m ⇒ IORef a → (a → (a, b)) → m b
atomicModifyIORef' r = liftBase ∘ R.atomicModifyIORef' r
{-# INLINABLE atomicModifyIORef' #-}

-- | Generalized version of 'R.atomicWriteIORef'.
atomicWriteIORef ∷ MonadBase IO m ⇒ IORef a → a → m ()
atomicWriteIORef r = liftBase ∘ R.atomicWriteIORef r
#endif

-- | Generalized version of 'R.mkWeakIORef'.
--
-- Note any monadic side effects in @m@ of the \"finalizer\" computation
-- are discarded.
mkWeakIORef ∷ MonadBaseControl IO m ⇒ IORef a → m () → m (Weak (IORef a))
mkWeakIORef = liftBaseDiscard ∘ R.mkWeakIORef
{-# INLINABLE mkWeakIORef #-}
