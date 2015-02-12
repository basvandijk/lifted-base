{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts #-}

-- from base:
#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif
import Data.IORef
import Data.Maybe
import Data.Typeable (Typeable)

-- from transformers-base:
import Control.Monad.Base (liftBase)

-- from transformers:
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Except

import Control.Monad.Trans.State
import qualified Control.Monad.Trans.RWS as RWS

-- from monad-control:
import Control.Monad.Trans.Control (MonadBaseControl)

-- from lifted-base (this package):
import Control.Exception.Lifted

-- from test-framework:
import Test.Framework (defaultMain, testGroup, Test)

 -- from test-framework-hunit:
import Test.Framework.Providers.HUnit

-- from hunit:
import Test.HUnit hiding (Test)


main :: IO ()
main = defaultMain
    [ testSuite "IdentityT" runIdentityT
    , testSuite "ListT" $ fmap head . runListT
    , testSuite "MaybeT" $ fmap fromJust . runMaybeT
    , testSuite "ReaderT" $ flip runReaderT "reader state"
    , testSuite "WriterT" runWriterT'
    , testSuite "ExceptT" runExceptT'
    , testSuite "StateT" $ flip evalStateT "state state"
    , testSuite "RWST" $ \m -> runRWST' m "RWS in" "RWS state"
    , testCase "ExceptT throwE" case_throwE
    , testCase "WriterT tell" case_tell
    ]
  where
    runWriterT' :: Functor m => WriterT [Int] m a -> m a
    runWriterT' = fmap fst . runWriterT
    runExceptT' :: Functor m => ExceptT String m () -> m ()
    runExceptT' = fmap (either (const ()) id) . runExceptT
    runRWST' :: (Monad m, Functor m) => RWS.RWST r [Int] s m a -> r -> s -> m a
    runRWST' m r s = fmap fst $ RWS.evalRWST m r s

testSuite :: MonadBaseControl IO m => String -> (m () -> IO ()) -> Test
testSuite s run = testGroup s
    [ testCase "finally" $ case_finally run
    , testCase "catch" $ case_catch run
    , testCase "bracket" $ case_bracket run
    , testCase "bracket_" $ case_bracket_ run
    , testCase "onException" $ case_onException run
    ]

ignore :: IO () -> IO ()
ignore x =
    catch x go
  where
    go :: SomeException -> IO ()
    go _ = return ()

data Exc = Exc
    deriving (Show, Typeable)
instance Exception Exc

one :: Int
one = 1

case_finally :: MonadBaseControl IO m => (m () -> IO ()) -> Assertion
case_finally run = do
    i <- newIORef one
    ignore
        (run $ (do
            liftBase $ writeIORef i 2
            error "error") `finally` (liftBase $ writeIORef i 3))
    j <- readIORef i
    j @?= 3

case_catch :: MonadBaseControl IO m => (m () -> IO ()) -> Assertion
case_catch run = do
    i <- newIORef one
    run $ (do
        liftBase $ writeIORef i 2
        throw Exc) `catch` (\Exc -> liftBase $ writeIORef i 3)
    j <- readIORef i
    j @?= 3

case_bracket :: MonadBaseControl IO m => (m () -> IO ()) -> Assertion
case_bracket run = do
    i <- newIORef one
    ignore $ run $ bracket
        (liftBase $ writeIORef i 2)
        (\() -> liftBase $ writeIORef i 4)
        (\() -> liftBase $ writeIORef i 3)
    j <- readIORef i
    j @?= 4

case_bracket_ :: MonadBaseControl IO m => (m () -> IO ()) -> Assertion
case_bracket_ run = do
    i <- newIORef one
    ignore $ run $ bracket_
        (liftBase $ writeIORef i 2)
        (liftBase $ writeIORef i 4)
        (liftBase $ writeIORef i 3)
    j <- readIORef i
    j @?= 4

case_onException :: MonadBaseControl IO m => (m () -> IO ()) -> Assertion
case_onException run = do
    i <- newIORef one
    ignore $ run $ onException
        (liftBase (writeIORef i 2) >> error "ignored")
        (liftBase $ writeIORef i 3)
    j <- readIORef i
    j @?= 3
    ignore $ run $ onException
        (liftBase $ writeIORef i 4)
        (liftBase $ writeIORef i 5)
    k <- readIORef i
    k @?= 4

case_throwE :: Assertion
case_throwE = do
    i <- newIORef one
    Left "throwE" <- runExceptT $
        (liftBase (writeIORef i 2) >> throwE "throwE")
        `finally`
        (liftBase $ writeIORef i 3)
    j <- readIORef i
    j @?= 3

case_tell :: Assertion
case_tell = do
    i <- newIORef one
    ((), w) <- runWriterT $ bracket_
        (liftBase (writeIORef i 2) >> tell [1 :: Int])
        (liftBase (writeIORef i 4) >> tell [3])
        (liftBase (writeIORef i 3) >> tell [2])
    j <- readIORef i
    j @?= 4
    w @?= [2]

    ((), w') <- runWriterT $ bracket
        (liftBase (writeIORef i 5) >> tell [5 :: Int])
        (const $ liftBase (writeIORef i 7) >> tell [7])
        (const $ liftBase (writeIORef i 6) >> tell [6])
    j' <- readIORef i
    j' @?= 7
    w' @?= [5, 6]
