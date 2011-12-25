{-# LANGUAGE CPP, UnicodeSyntax, NoImplicitPrelude, FlexibleContexts #-}

{- |
Module      :  Control.Concurrent.MVar.Lifted
Copyright   :  Bas van Dijk
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental

This is a wrapped version of 'Control.Concurrent.MVar' with types generalized
from @IO@ to all monads in either 'MonadBase' or 'MonadBaseControl'.
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
    , addMVarFinalizer
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool           ( Bool )
import Data.Function       ( ($) )
import Data.Maybe          ( Maybe(Nothing, Just) )
import Data.Tuple          ( snd )
import Control.Monad       ( return, liftM )
import System.IO           ( IO )
import           Control.Concurrent.MVar  ( MVar )
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as E


#if __GLASGOW_HASKELL__ < 700
import Control.Monad ( (>>=), (>>), fail )
#endif

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers-base:
import Control.Monad.Base ( MonadBase, liftBase )

-- from monad-control:
import Control.Monad.Trans.Control ( MonadBaseControl, getValueM
                                   , control, liftBaseOp, liftBaseDiscard
                                   )

#if MIN_VERSION_base(4,3,0)
import Control.Monad ( void )
#else
import Data.Functor ( Functor, fmap )
import Data.Function ( const )
void ∷ Functor f ⇒ f α → f ()
void = fmap (const ())
#endif

#include "inlinable.h"

--------------------------------------------------------------------------------
-- * MVars
--------------------------------------------------------------------------------

-- | Generalized version of 'MVar.newEmptyMVar'.
newEmptyMVar ∷ MonadBase IO m ⇒ m (MVar α)
newEmptyMVar = liftBase MVar.newEmptyMVar
{-# INLINABLE newEmptyMVar #-}

-- | Generalized version of 'MVar.newMVar'.
newMVar ∷ MonadBase IO m ⇒ α → m (MVar α)
newMVar = liftBase ∘ MVar.newMVar
{-# INLINABLE newMVar #-}

-- | Generalized version of 'MVar.takeMVar'.
takeMVar ∷ MonadBase IO m ⇒ MVar α → m α
takeMVar = liftBase ∘ MVar.takeMVar
{-# INLINABLE takeMVar #-}

-- | Generalized version of 'MVar.putMVar'.
putMVar ∷ MonadBase IO m ⇒ MVar α → α → m ()
putMVar mv x = liftBase $ MVar.putMVar mv x
{-# INLINABLE putMVar #-}

-- | Generalized version of 'MVar.readMVar'.
readMVar ∷ MonadBase IO m ⇒ MVar α → m α
readMVar = liftBase ∘ MVar.readMVar
{-# INLINABLE readMVar #-}

-- | Generalized version of 'MVar.swapMVar'.
swapMVar ∷ MonadBase IO m ⇒ MVar α → α → m α
swapMVar mv x = liftBase $ MVar.swapMVar mv x
{-# INLINABLE swapMVar #-}

-- | Generalized version of 'MVar.tryTakeMVar'.
tryTakeMVar ∷ MonadBase IO m ⇒ MVar α → m (Maybe α)
tryTakeMVar = liftBase ∘ MVar.tryTakeMVar
{-# INLINABLE tryTakeMVar #-}

-- | Generalized version of 'MVar.tryPutMVar'.
tryPutMVar ∷ MonadBase IO m ⇒ MVar α → α → m Bool
tryPutMVar mv x = liftBase $ MVar.tryPutMVar mv x
{-# INLINABLE tryPutMVar #-}

-- | Generalized version of 'MVar.isEmptyMVar'.
isEmptyMVar ∷ MonadBase IO m ⇒ MVar α → m Bool
isEmptyMVar = liftBase ∘ MVar.isEmptyMVar
{-# INLINABLE isEmptyMVar #-}

-- | Generalized version of 'MVar.withMVar'.
withMVar ∷ MonadBaseControl IO m ⇒ MVar α → (α → m β) → m β
withMVar = liftBaseOp ∘ MVar.withMVar
{-# INLINABLE withMVar #-}

-- | Generalized version of 'MVar.modifyMVar_'.
modifyMVar_ ∷ (MonadBaseControl IO m, MonadBase IO m) ⇒ MVar α → (α → m α) → m ()

-- | Generalized version of 'MVar.modifyMVar'.
modifyMVar ∷ (MonadBaseControl IO m, MonadBase IO m) ⇒ MVar α → (α → m (α, β)) → m β

#if MIN_VERSION_base(4,3,0)
modifyMVar_ mv f = void $ control $ \runInIO →
                     E.mask $ \restore → do
                       old ← MVar.takeMVar mv
                       stM ← restore (runInIO (f old))
                               `E.onException` MVar.putMVar mv old
                       case getValueM stM of
                         Nothing  → MVar.putMVar mv old
                         Just new → MVar.putMVar mv new
                       return stM

modifyMVar mv f = liftM snd $ control $ \runInIO →
                    E.mask $ \restore → do
                      old ← MVar.takeMVar mv
                      stM ← restore (runInIO (f old))
                              `E.onException` MVar.putMVar mv old
                      case getValueM stM of
                        Nothing       → MVar.putMVar mv old
                        Just (new, _) → MVar.putMVar mv new
                      return stM
#else
modifyMVar_ mv f = void $ control $ \runInIO →
                     E.block $ do
                       old ← MVar.takeMVar mv
                       stM ← E.unblock (runInIO (f old))
                               `E.onException` MVar.putMVar mv old
                       case getValueM stM of
                         Nothing  → MVar.putMVar mv old
                         Just new → MVar.putMVar mv new
                       return stM

modifyMVar mv f = liftM snd $ control $ \runInIO →
                    E.block $ do
                      old ← MVar.takeMVar mv
                      stM ← E.unblock (runInIO (f old))
                              `E.onException` MVar.putMVar mv old
                      case getValueM stM of
                        Nothing       → MVar.putMVar mv old
                        Just (new, _) → MVar.putMVar mv new
                      return stM
#endif
{-# INLINABLE modifyMVar_ #-}
{-# INLINABLE modifyMVar #-}

-- | Generalized version of 'MVar.addMVarFinalizer'.
--
-- Note any monadic side effects in @m@ of the \"finalizer\" computation are
-- discarded.
addMVarFinalizer ∷ MonadBaseControl IO m ⇒ MVar α → m () → m ()
addMVarFinalizer = liftBaseDiscard ∘ MVar.addMVarFinalizer
{-# INLINABLE addMVarFinalizer #-}
