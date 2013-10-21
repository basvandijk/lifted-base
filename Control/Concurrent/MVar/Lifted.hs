{-# LANGUAGE CPP
           , UnicodeSyntax
           , NoImplicitPrelude
           , FlexibleContexts
           , TupleSections #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{- |
Module      :  Control.Concurrent.MVar.Lifted
Copyright   :  Bas van Dijk
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental

This is a wrapped version of "Control.Concurrent.MVar" with types generalized
from 'IO' to all monads in either 'MonadBase' or 'MonadBaseControl'.
-}

module Control.Concurrent.MVar.Lifted
    ( MVar.MVar
    , newEmptyMVar
    , newMVar
    , takeMVar
    , putMVar
    , readMVar
    , swapMVar
    , tryTakeMVar
    , tryPutMVar
    , isEmptyMVar
    , withMVar
    , modifyMVar_
    , modifyMVar
#if MIN_VERSION_base(4,6,0)
    , modifyMVarMasked_
    , modifyMVarMasked
#endif
#if MIN_VERSION_base(4,6,0)
    , mkWeakMVar
#else
    , addMVarFinalizer
#endif
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( Bool(False, True) )
import Data.Function ( ($) )
import Data.Functor  ( fmap )
import Data.IORef    ( newIORef, readIORef, writeIORef )
import Data.Maybe    ( Maybe )
import Control.Monad ( return, when )
import System.IO     ( IO )
import           Control.Concurrent.MVar  ( MVar )
import qualified Control.Concurrent.MVar as MVar
import Control.Exception ( onException
#if MIN_VERSION_base(4,3,0)
                         , mask, mask_
#else
                         , block, unblock
#endif
                         )
#if MIN_VERSION_base(4,6,0)
import System.Mem.Weak ( Weak )
#endif

#if __GLASGOW_HASKELL__ < 700
import Control.Monad ( (>>=), (>>), fail )
#endif

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers-base:
import Control.Monad.Base ( MonadBase, liftBase )

-- from monad-control:
import Control.Monad.Trans.Control ( MonadBaseControl
                                   , control
                                   , liftBaseOp
                                   , liftBaseDiscard
                                   )

#include "inlinable.h"

--------------------------------------------------------------------------------
-- * MVars
--------------------------------------------------------------------------------

-- | Generalized version of 'MVar.newEmptyMVar'.
newEmptyMVar ∷ MonadBase IO m ⇒ m (MVar a)
newEmptyMVar = liftBase MVar.newEmptyMVar
{-# INLINABLE newEmptyMVar #-}

-- | Generalized version of 'MVar.newMVar'.
newMVar ∷ MonadBase IO m ⇒ a → m (MVar a)
newMVar = liftBase ∘ MVar.newMVar
{-# INLINABLE newMVar #-}

-- | Generalized version of 'MVar.takeMVar'.
takeMVar ∷ MonadBase IO m ⇒ MVar a → m a
takeMVar = liftBase ∘ MVar.takeMVar
{-# INLINABLE takeMVar #-}

-- | Generalized version of 'MVar.putMVar'.
putMVar ∷ MonadBase IO m ⇒ MVar a → a → m ()
putMVar mv x = liftBase $ MVar.putMVar mv x
{-# INLINABLE putMVar #-}

-- | Generalized version of 'MVar.readMVar'.
readMVar ∷ MonadBase IO m ⇒ MVar a → m a
readMVar = liftBase ∘ MVar.readMVar
{-# INLINABLE readMVar #-}

-- | Generalized version of 'MVar.swapMVar'.
swapMVar ∷ MonadBase IO m ⇒ MVar a → a → m a
swapMVar mv x = liftBase $ MVar.swapMVar mv x
{-# INLINABLE swapMVar #-}

-- | Generalized version of 'MVar.tryTakeMVar'.
tryTakeMVar ∷ MonadBase IO m ⇒ MVar a → m (Maybe a)
tryTakeMVar = liftBase ∘ MVar.tryTakeMVar
{-# INLINABLE tryTakeMVar #-}

-- | Generalized version of 'MVar.tryPutMVar'.
tryPutMVar ∷ MonadBase IO m ⇒ MVar a → a → m Bool
tryPutMVar mv x = liftBase $ MVar.tryPutMVar mv x
{-# INLINABLE tryPutMVar #-}

-- | Generalized version of 'MVar.isEmptyMVar'.
isEmptyMVar ∷ MonadBase IO m ⇒ MVar a → m Bool
isEmptyMVar = liftBase ∘ MVar.isEmptyMVar
{-# INLINABLE isEmptyMVar #-}

-- | Generalized version of 'MVar.withMVar'.
withMVar ∷ MonadBaseControl IO m ⇒ MVar a → (a → m b) → m b
withMVar = liftBaseOp ∘ MVar.withMVar
{-# INLINABLE withMVar #-}

-- | Generalized version of 'MVar.modifyMVar_'.
modifyMVar_ ∷ (MonadBaseControl IO m) ⇒ MVar a → (a → m a) → m ()
modifyMVar_ mv = modifyMVar mv ∘ (fmap (, ()) ∘)
{-# INLINABLE modifyMVar_ #-}

-- | Generalized version of 'MVar.modifyMVar'.
modifyMVar ∷ (MonadBaseControl IO m) ⇒ MVar a → (a → m (a, b)) → m b

#if MIN_VERSION_base(4,3,0)
modifyMVar mv f = control $ \runInIO → mask $ \restore → do
    aborted ← newIORef True
    let f' x = do
        (x', a) ← f x
        liftBase $ mask_ $ do
          writeIORef aborted False
          MVar.putMVar mv x'
        return a
    x ← MVar.takeMVar mv
    stM ← restore (runInIO (f' x)) `onException` MVar.putMVar mv x
    abort ← readIORef aborted
    when abort $ MVar.putMVar mv x
    return stM
#else
modifyMVar mv f = control $ \runInIO -> block $ do
    aborted ← newIORef True
    let f' x = do
        (x', a) ← f x
        liftBase $ block $ do
          writeIORef aborted False
          MVar.putMVar mv x'
        return a
    x ← MVar.takeMVar mv
    stM ← unblock (runInIO (f' x)) `onException` MVar.putMVar mv x
    abort ← readIORef aborted
    when abort $ MVar.putMVar mv x
    return stM
#endif
{-# INLINABLE modifyMVar #-}

#if MIN_VERSION_base(4,6,0)
-- | Generalized version of 'MVar.modifyMVarMasked_'.
modifyMVarMasked_ ∷ (MonadBaseControl IO m) ⇒ MVar a → (a → m a) → m ()
modifyMVarMasked_ mv = modifyMVarMasked mv ∘ (fmap (, ()) ∘)
{-# INLINABLE modifyMVarMasked_ #-}

-- | Generalized version of 'MVar.modifyMVarMasked'.
modifyMVarMasked ∷ (MonadBaseControl IO m) ⇒ MVar a → (a → m (a, b)) → m b
modifyMVarMasked mv f = control $ \runInIO → mask_ $ do
    aborted ← newIORef True
    let f' x = do
        (x', a) ← f x
        liftBase $ do
          writeIORef aborted False
          MVar.putMVar mv x'
        return a
    x ← MVar.takeMVar mv
    stM ← runInIO (f' x) `onException` MVar.putMVar mv x
    abort ← readIORef aborted
    when abort $ MVar.putMVar mv x
    return stM
{-# INLINABLE modifyMVarMasked #-}
#endif

#if MIN_VERSION_base(4,6,0)
-- | Generalized version of 'MVar.mkWeakMVar'.
--
-- Note any monadic side effects in @m@ of the \"finalizer\" computation are
-- discarded.
mkWeakMVar ∷ MonadBaseControl IO m ⇒ MVar a → m () → m (Weak (MVar a))
mkWeakMVar = liftBaseDiscard ∘ MVar.mkWeakMVar
{-# INLINABLE mkWeakMVar #-}
#else
-- | Generalized version of 'MVar.addMVarFinalizer'.
--
-- Note any monadic side effects in @m@ of the \"finalizer\" computation are
-- discarded.
addMVarFinalizer ∷ MonadBaseControl IO m ⇒ MVar a → m () → m ()
addMVarFinalizer = liftBaseDiscard ∘ MVar.addMVarFinalizer
{-# INLINABLE addMVarFinalizer #-}
#endif
