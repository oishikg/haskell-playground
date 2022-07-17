module MonadTransformers
    (

    ) where

-- import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad (liftM)

{- | In this module, we explore:
1. MaybeT
2. ExceptT / EitherT
3. ReaderT

Next, we look at an application of monad transformers using the
ReaderT monad as an example.
-}

-- | MaybeT monad transformer

newtype MyMaybeT m a =
    MyMaybeT { runMyMaybeT :: m (Maybe a)
             }

instance (Functor m) => Functor (MyMaybeT m) where
    fmap :: (a -> b) -> MyMaybeT m a -> MyMaybeT m b
    fmap f (MyMaybeT mma) = MyMaybeT $ fmap (fmap f) mma

instance (Applicative m) => Applicative (MyMaybeT m) where
    pure :: a -> MyMaybeT m a
    pure = MyMaybeT . pure . pure

    -- unfoldMaybeAp :: m (Maybe a -> Maybe b)
    -- Since:
    -- fmap :: (x -> y) -> f x  -> f y
    -- (<*>) :: Maybe (a -> b) -> (Maybe a -> Maybe b)
    -- fmap (<*>) :: f (Maybe (a -> b)) -> (Maybe a -> Maybe b)
    -- Substituting f with m gives us the desired result
    (<*>) :: MyMaybeT m (a -> b) -> MyMaybeT m a -> MyMaybeT m b
    (<*>) (MyMaybeT mab) (MyMaybeT ma) =
        let unfoldMaybeAp = fmap (<*>) mab
        in MyMaybeT $ unfoldMaybeAp <*> ma

instance (Monad m) => Monad (MyMaybeT m) where
    return :: a -> MyMaybeT m a
    return = pure

    -- This solution isn't elegant, but it gets to the root of
    -- why monad transformers are needed; we can't simply use the
    -- Monad instance of Maybe to implement the solution, because
    -- that would've implied that for any two monads m1, m2, we would
    -- be able to define a monad (m1 m2). However, monads cannot be
    -- generally nested, so we have to rely on the actual
    -- implementation of the inner monad (in this case, the Maybe
    -- monad)
    (>>=) :: MyMaybeT m a -> (a -> MyMaybeT m b) -> MyMaybeT m b
    (>>=) (MyMaybeT ma) amb = MyMaybeT $
        ma >>= \maybeA ->
                   case maybeA of
                       Nothing -> return Nothing
                       Just a -> runMyMaybeT $ amb a

{- | MaybeT monad in practice:



-}

-- First attempt:

-- getUserName :: MyMaybeT IO String
-- getUserName = do
--     putStrLn "Enter your name. If you do not wish to do so, enter 0."
--     input <- getLine
--     if input == "0" -- fail the computation
--     then ...

-- Issue: All the computations take place in the IO monad; how do
-- we get the result into the MyMaybeT monad?

ioGetUserName :: IO (Maybe String)
ioGetUserName = do
    putStrLn "Enter your name. If you do not wish to do so, enter 0."
    input <- getLine
    if input == "0" -- fail the computation
    then pure Nothing
    else pure $ Just input

-- This works, but then why go though all the trouble of defining
-- monad transformers?

-- Introducing the MonadTrans class:
-- https://hackage.haskell.org/package/transformers-0.6.0.4/docs/Control-Monad-Trans-Class.html#v:lift
instance MonadTrans MyMaybeT where
    lift :: (Monad m) => m a -> MyMaybeT m a
    lift ma = MyMaybeT $ ma >>= pure . Just
    -- library implementation:
    -- MyMaybeT . liftM Just

-- Next attempt
-- getUserName :: MyMaybeT IO String
-- getUserName = do
--     lift $ putStrLn "Enter your name. If you do not wish to do so, enter 0."
--     input <- lift getLine
--     if input == "0" -- fail the computation
--     then pure Nothing
--     else pure $ Just input

-- This attempt fails to compile as well; the issue is:
-- The Maybe value is implicit in the monad transformer type, so
-- we cannot just pure it; we need one more piece of machinery

-- Introducing our own MonadError class:
class (Monad m) => MonadError m e where
    throwError :: e -> m a

-- This represents an interface for monads that can implicitly handle
-- errors

-- MonadError instance for MyMaybeT
instance (Monad m) => MonadError (MyMaybeT m) String where
    throwError :: e -> MyMaybeT m a
    throwError _ = MyMaybeT $ pure Nothing

-- This enables us to "throw errors" by short-ciruciting the
-- computation with a Nothing value

-- Final attempt at defining getUserName
getUserName :: MyMaybeT IO String
getUserName = do
    lift $ putStrLn "Enter your name. If you do not wish to do so, enter 0."
    input <- lift getLine
    if input == "0"
    -- TODO: Investigate why the type annotations are needed
    then throwError @(MyMaybeT IO) @String "dummy message"
    else pure input
