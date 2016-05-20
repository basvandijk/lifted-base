{-# LANGUAGE DeriveDataTypeable, RankNTypes #-}

module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude hiding (catch)
import Control.Exception ( Exception, SomeException, throwIO )
import qualified Control.Exception as E ( mask, bracket, bracket_ )
import Data.Typeable
import Control.Monad (join)

-- from criterion:
import Criterion.Main

-- from transformers:
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

-- from monad-peel:
import qualified Control.Exception.Peel as MP
import qualified Control.Monad.IO.Peel  as MP

-- from monad-control:
import qualified Control.Monad.Trans.Control as MC

-- from lifted-base:
import qualified Control.Exception.Lifted as MC


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ b "bracket"   benchBracket  MP.bracket   MC.bracket
  , b "bracket_"  benchBracket_ MP.bracket_  MC.bracket_
  , b "catch"     benchCatch    MP.catch     MC.catch
  , b "try"       benchTry      MP.try       MC.try

  , bgroup "mask"
    [ bench "monad-peel"    $ whnfIO $ benchMask mpMask
    , bench "monad-control" $ whnfIO $ benchMask MC.mask
    ]

  , bgroup "liftIOOp"
    [ bench "monad-peel"    $ whnfIO $ exe $ MP.liftIOOp   (E.bracket nop (\_ -> nop))
                                                           (\_ -> nop)
    , bench "monad-control" $ whnfIO $ exe $ MC.liftBaseOp (E.bracket nop (\_ -> nop))
                                                           (\_ -> nop)
    ]

  , bgroup "liftIOOp_"
    [ bench "monad-peel"    $ whnfIO $ exe $ MP.liftIOOp_   (E.bracket_ nop nop) nop
    , bench "monad-control" $ whnfIO $ exe $ MC.liftBaseOp_ (E.bracket_ nop nop) nop
    ]
  ]

b name bnch peel mndCtrl = bgroup name
  [ bench "monad-peel"    $ whnfIO $ bnch peel
  , bench "monad-control" $ whnfIO $ bnch mndCtrl
  ]

--------------------------------------------------------------------------------
-- Monad stack
--------------------------------------------------------------------------------

type M a = ReaderT Int (StateT Bool (WriterT String (MaybeT IO))) a

type R a = IO (Maybe ((a, Bool), String))

runM :: Int -> Bool -> M a -> R a
runM r s m = runMaybeT (runWriterT (runStateT (runReaderT m r) s))

exe :: M a -> R a
exe = runM 0 False


--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

benchBracket   bracket   = exe $              bracket  nop (\_ -> nop)  (\_ -> nop)
benchBracket_  bracket_  = exe $              bracket_ nop nop          nop
benchCatch     catch     = exe $ catch throwE (\E -> nop)
benchTry       try       = exe $ try throwE :: R (Either E ())

benchMask :: (((forall a. M a -> M a) -> M ()) -> M ()) -> R ()
benchMask mask = exe $ mask $ \restore -> nop >> restore nop >> nop


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

nop :: Monad m => m ()
nop = return ()

data E = E deriving (Show, Typeable)

instance Exception E

throwE :: MonadIO m => m ()
throwE = liftIO $ throwIO E

mpMask :: MP.MonadPeelIO m => ((forall a. m a -> m a) -> m b) -> m b
mpMask f = do
  k <- MP.peelIO
  join $ liftIO $ E.mask $ \restore -> k $ f $ MP.liftIOOp_ restore
