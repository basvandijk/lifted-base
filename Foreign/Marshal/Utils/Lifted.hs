{-# LANGUAGE CPP
           , FlexibleContexts
           , UnicodeSyntax
           , NoImplicitPrelude
  #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{- |
Module      :  Foreign.Marshal.Utils.Lifted
Copyright   :  Bas van Dijk, Anders Kaseorg, Michael Steele
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental
Portability :  non-portable (extended exceptions)

This is a wrapped version of "Foreign.Marshal.Utils" with types generalized
from 'IO' to all monads in either 'MonadBase' or 'MonadBaseControl'.
-}

module Foreign.Marshal.Utils.Lifted
  ( with
  ) where

import qualified Foreign as F
import System.IO     ( IO )

-- from monad-control:
import Control.Monad.Trans.Control ( MonadBaseControl
                                   , liftBaseOp )

-- |Generalized version of 'F.with'.
--
-- Note:
--
-- * When the \"f\" computations throw exceptions
--   any monadic side effects in @m@ will be discarded.
with ∷ (MonadBaseControl IO m, F.Storable a)
     ⇒ a       -- ^ computation to run first (\"acquire resource\")
     → (F.Ptr a → m b) -- ^ computation to run last (\"release resource\")
     → m b
with val = liftBaseOp (F.with val)
{-# INLINEABLE with #-}
