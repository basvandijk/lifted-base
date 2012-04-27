{-# LANGUAGE CPP, UnicodeSyntax, NoImplicitPrelude, FlexibleContexts, RankNTypes #-}

{- |
Module      :  Control.Concurrent.Lifted
Copyright   :  Bas van Dijk
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental

This is a wrapped version of 'Control.Concurrent' with types generalized
from @IO@ to all monads in either 'MonadBase' or 'MonadBaseControl'.
-}

module Control.Concurrent.Lifted
    ( -- * Concurrent Haskell
      ThreadId

      -- * Basic concurrency operations
    , myThreadId
    , fork
#if MIN_VERSION_base(4,4,0)
    , forkWithUnmask
#endif
    , killThread
    , throwTo

#if MIN_VERSION_base(4,4,0)
      -- ** Threads with affinity
    , forkOn
    , forkOnWithUnmask
    , getNumCapabilities
    , threadCapability
#endif

      -- * Scheduling
    , yield

      -- ** Blocking
      -- ** Waiting
    , threadDelay
    , threadWaitRead
    , threadWaitWrite

      -- * Communication abstractions
    , module Control.Concurrent.MVar.Lifted
    , module Control.Concurrent.Chan.Lifted
    , module Control.Concurrent.QSem.Lifted
    , module Control.Concurrent.QSemN.Lifted
    -- TODO: , module Control.Concurrent.SampleVar.Lifted

      -- * Merging of streams
    , merge
    , nmerge

      -- * Bound Threads
    , forkOS
    , isCurrentThreadBound
    , runInBoundThread
    , runInUnboundThread
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool          ( Bool )
import Data.Int           ( Int )
import Data.Function      ( ($) )
import System.IO          ( IO )
import System.Posix.Types ( Fd )
import Control.Exception  ( Exception )

import           Control.Concurrent ( ThreadId )
import qualified Control.Concurrent as C

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers-base:
import Control.Monad.Base ( MonadBase, liftBase )

-- from monad-control:
import Control.Monad.Trans.Control ( MonadBaseControl, liftBaseOp_, liftBaseDiscard )

#if MIN_VERSION_base(4,4,0)
import Control.Monad.Trans.Control ( liftBaseWith )
import Control.Monad               ( void )
#endif

-- from lifted-base (this package):
import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Chan.Lifted
import Control.Concurrent.QSem.Lifted
import Control.Concurrent.QSemN.Lifted

#include "inlinable.h"


--------------------------------------------------------------------------------
-- Control.Concurrent
--------------------------------------------------------------------------------

-- | Generalized version of 'C.myThreadId'.
myThreadId ∷ MonadBase IO m ⇒ m ThreadId
myThreadId = liftBase C.myThreadId
{-# INLINABLE myThreadId #-}

-- | Generalized version of 'C.forkIO'.
--
-- Note that, while the forked computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in 'IO'.
fork ∷ MonadBaseControl IO m ⇒ m () → m ThreadId
fork = liftBaseDiscard C.forkIO
{-# INLINABLE fork #-}

#if MIN_VERSION_base(4,4,0)
-- | Generalized version of 'C.forkIOWithUnmask'.
--
-- Note that, while the forked computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in 'IO'.
forkWithUnmask ∷ MonadBaseControl IO m ⇒ ((∀ a. m a → m a) → m ()) → m ThreadId
forkWithUnmask f = liftBaseWith $ \runInIO →
                     C.forkIOWithUnmask $ \unmask →
                       void $ runInIO $ f $ liftBaseOp_ unmask
{-# INLINABLE  forkWithUnmask #-}
#endif

-- | Generalized version of 'C.killThread'.
killThread ∷ MonadBase IO m ⇒ ThreadId → m ()
killThread = liftBase ∘ C.killThread
{-# INLINABLE  killThread #-}

-- | Generalized version of 'C.throwTo'.
throwTo ∷ (MonadBase IO m, Exception e) ⇒ ThreadId → e → m ()
throwTo tid e = liftBase $ C.throwTo tid e
{-# INLINABLE throwTo #-}

#if MIN_VERSION_base(4,4,0)
-- | Generalized version of 'C.forkOn'.
--
-- Note that, while the forked computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in 'IO'.
forkOn ∷ MonadBaseControl IO m ⇒ Int → m () → m ThreadId
forkOn = liftBaseDiscard ∘ C.forkOn
{-# INLINABLE forkOn #-}

-- | Generalized version of 'C.forkOnWithUnmask'.
--
-- Note that, while the forked computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in 'IO'.
forkOnWithUnmask ∷ MonadBaseControl IO m ⇒ Int → ((∀ a. m a → m a) → m ()) → m ThreadId
forkOnWithUnmask cap f = liftBaseWith $ \runInIO →
                           C.forkOnWithUnmask cap $ \unmask →
                             void $ runInIO $ f $ liftBaseOp_ unmask
{-# INLINABLE forkOnWithUnmask #-}

-- | Generalized version of 'C.getNumCapabilities'.
getNumCapabilities ∷ MonadBase IO m ⇒ m Int
getNumCapabilities = liftBase C.getNumCapabilities
{-# INLINABLE getNumCapabilities #-}

-- | Generalized version of 'C.threadCapability'.
threadCapability ∷ MonadBase IO m ⇒ ThreadId → m (Int, Bool)
threadCapability = liftBase ∘ C.threadCapability
{-# INLINABLE threadCapability #-}
#endif

-- | Generalized version of 'C.yield'.
yield ∷ MonadBase IO m ⇒ m ()
yield = liftBase C.yield
{-# INLINABLE yield #-}

-- | Generalized version of 'C.threadDelay'.
threadDelay ∷ MonadBase IO m ⇒ Int → m ()
threadDelay = liftBase ∘  C.threadDelay
{-# INLINABLE threadDelay #-}

-- | Generalized version of 'C.threadWaitRead'.
threadWaitRead ∷ MonadBase IO m ⇒ Fd → m ()
threadWaitRead = liftBase ∘ C.threadWaitRead
{-# INLINABLE threadWaitRead #-}

-- | Generalized version of 'C.threadWaitWrite'.
threadWaitWrite ∷ MonadBase IO m ⇒ Fd → m ()
threadWaitWrite = liftBase ∘ C.threadWaitWrite
{-# INLINABLE threadWaitWrite #-}

-- | Generalized version of 'C.mergeIO'.
merge ∷ MonadBase IO m ⇒ [a] → [a] → m [a]
merge xs ys = liftBase $ C.mergeIO xs ys
{-# INLINABLE merge #-}

-- | Generalized version of 'C.nmergeIO'.
nmerge ∷ MonadBase IO m ⇒ [[a]] → m [a]
nmerge = liftBase ∘ C.nmergeIO
{-# INLINABLE nmerge #-}

-- | Generalized version of 'C.forkOS'.
--
-- Note that, while the forked computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in 'IO'.
forkOS ∷ MonadBaseControl IO m ⇒ m () → m ThreadId
forkOS = liftBaseDiscard C.forkOS
{-# INLINABLE forkOS #-}

-- | Generalized version of 'C.isCurrentThreadBound'.
isCurrentThreadBound ∷ MonadBase IO m ⇒ m Bool
isCurrentThreadBound = liftBase C.isCurrentThreadBound
{-# INLINABLE isCurrentThreadBound #-}

-- | Generalized version of 'C.runInBoundThread'.
runInBoundThread ∷ MonadBaseControl IO m ⇒ m a → m a
runInBoundThread = liftBaseOp_ C.runInBoundThread
{-# INLINABLE runInBoundThread #-}

-- | Generalized version of 'C.runInUnboundThread'.
runInUnboundThread ∷ MonadBaseControl IO m ⇒ m a → m a
runInUnboundThread = liftBaseOp_ C.runInUnboundThread
{-# INLINABLE runInUnboundThread #-}
