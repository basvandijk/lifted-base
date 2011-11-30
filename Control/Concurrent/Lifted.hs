{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, FlexibleContexts, RankNTypes #-}

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
    , forkWithUnmask
    , killThread
    , throwTo

      -- ** Threads with affinity
    , forkOn
    , forkOnWithUnmask
    , getNumCapabilities
    , threadCapability

      -- * Scheduling
    , yield

      -- ** Blocking
      -- ** Waiting
    , threadDelay
    , threadWaitRead
    , threadWaitWrite

      -- * Communication abstractions
    , module Control.Concurrent.MVar.Lifted
    -- TODO: , module Control.Concurrent.Chan.Lifted
    -- TODO: , module Control.Concurrent.QSem.Lifted
    -- TODO: , module Control.Concurrent.QSemN.Lifted
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
import Control.Monad      ( void )
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
import Control.Monad.Trans.Control ( MonadBaseControl, liftBaseControl, liftBaseOp_ )

-- from lifted-base (this package):
import Control.Concurrent.MVar.Lifted


--------------------------------------------------------------------------------
-- Control.Concurrent
--------------------------------------------------------------------------------

-- | Generalized version of 'C.myThreadId'.
myThreadId ∷ MonadBase IO m ⇒ m ThreadId
myThreadId = liftBase C.myThreadId

-- | Generalized version of 'C.forkIO'.
--
-- Note any monadic side-effects in @m@ of the forked computation will not be
-- visible in the resulting computation.
fork ∷ MonadBaseControl IO m ⇒ m () → m ThreadId
fork m = liftBaseControl $ \runInIO →
           C.forkIO $ void $ runInIO m

-- | Generalized version of 'C.forkIOWithUnmask'.
--
-- Note any monadic side-effects in @m@ of the forked computation will not be
-- visible in the resulting computation.
forkWithUnmask ∷ MonadBaseControl IO m ⇒ ((∀ α. m α → m α) → m ()) → m ThreadId
forkWithUnmask f = liftBaseControl $ \runInIO →
                     C.forkIOWithUnmask $ \unmask →
                       void $ runInIO $ f $ liftBaseOp_ unmask

-- | Generalized version of 'C.killThread'.
killThread ∷ MonadBase IO m ⇒ ThreadId → m ()
killThread = liftBase ∘ C.killThread

-- | Generalized version of 'C.throwTo'.
throwTo ∷ (MonadBase IO m, Exception e) ⇒ ThreadId → e → m ()
throwTo tid e = liftBase $ C.throwTo tid e

-- | Generalized version of 'C.forkOn'.
--
-- Note any monadic side-effects in @m@ of the forked computation will not be
-- visible in the resulting computation.
forkOn ∷ MonadBaseControl IO m ⇒ Int → m () → m ThreadId
forkOn cap m = liftBaseControl $ \runInIO →
                 C.forkOn cap $ void $ runInIO m

-- | Generalized version of 'C.forkOnWithUnmask'.
--
-- Note any monadic side-effects in @m@ of the forked computation will not be
-- visible in the resulting computation.
forkOnWithUnmask ∷ MonadBaseControl IO m ⇒ Int → ((∀ α. m α → m α) → m ()) → m ThreadId
forkOnWithUnmask cap f = liftBaseControl $ \runInIO →
                           C.forkOnWithUnmask cap $ \unmask →
                             void $ runInIO $ f $ liftBaseOp_ unmask

-- | Generalized version of 'C.getNumCapabilities'.
getNumCapabilities ∷ MonadBase IO m ⇒ m Int
getNumCapabilities = liftBase C.getNumCapabilities

-- | Generalized version of 'C.threadCapability'.
threadCapability ∷ MonadBase IO m ⇒ ThreadId → m (Int, Bool)
threadCapability = liftBase ∘ C.threadCapability

-- | Generalized version of 'C.yield'.
yield ∷ MonadBase IO m ⇒ m ()
yield = liftBase C.yield

-- | Generalized version of 'C.threadDelay'.
threadDelay ∷ MonadBase IO m ⇒ Int → m ()
threadDelay = liftBase ∘  C.threadDelay

-- | Generalized version of 'C.threadWaitRead'.
threadWaitRead ∷ MonadBase IO m ⇒ Fd → m ()
threadWaitRead = liftBase ∘ C.threadWaitRead

-- | Generalized version of 'C.threadWaitWrite'.
threadWaitWrite ∷ MonadBase IO m ⇒ Fd → m ()
threadWaitWrite = liftBase ∘ C.threadWaitWrite

-- | Generalized version of 'C.mergeIO'.
merge ∷ MonadBase IO m ⇒ [α] → [α] → m [α]
merge xs ys = liftBase $ C.mergeIO xs ys

-- | Generalized version of 'C.nmergeIO'.
nmerge ∷ MonadBase IO m ⇒ [[α]] → m [α]
nmerge = liftBase ∘ C.nmergeIO

-- | Generalized version of 'C.forkOS'.
--
-- Note any monadic side-effects in @m@ of the forked computation will not be
-- visible in the resulting computation.
forkOS ∷ MonadBaseControl IO m ⇒ m () → m ThreadId
forkOS m = liftBaseControl $ \runInIO →
             C.forkOS $ void $ runInIO m

-- | Generalized version of 'C.isCurrentThreadBound'.
isCurrentThreadBound ∷ MonadBase IO m ⇒ m Bool
isCurrentThreadBound = liftBase C.isCurrentThreadBound

-- | Generalized version of 'C.runInBoundThread'.
runInBoundThread ∷ MonadBaseControl IO m ⇒ m α → m α
runInBoundThread = liftBaseOp_ C.runInBoundThread

-- | Generalized version of 'C.runInUnboundThread'.
runInUnboundThread ∷ MonadBaseControl IO m ⇒ m α → m α
runInUnboundThread = liftBaseOp_ C.runInUnboundThread
