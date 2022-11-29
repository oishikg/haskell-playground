{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IntroducingMonadTransformers where

import Control.Monad (liftM,ap)
import Data.Kind(Type)
import Data.Bifunctor (first)

-- Nesting functors and applicatives

newtype Compose f g a = Compose { runCompose :: f (g a) }

composetTestVal1 :: Compose [] Maybe Integer
composetTestVal1 = Compose [Just 1, Just 2, Just 3]

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap f (Compose fga) = Compose $ fmap (fmap f) fga -- functors are closed under composition

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ pure $ pure a

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (<*>) (Compose fgab) (Compose fga) = Compose $ fmap (<*>) fgab <*> fga
    -- Reasoning:
    -- :t (fmap (<*>) fgab) = f (g a -> g b)
    -- Now apply a value of this type to a value of type f (g a) to get f ( g b )

-- Nesting monads is not possible in this way. Hence custom monad transformers have to be defined

{-  The following exploration of monad transformers has been taken from:
https://www.seas.upenn.edu/~cis5520/21fa/lectures/stub/10-transformers/Transformers.html
-}

-- Motivating example: Language of simple division expressions + evaluators

data Expr
    = Val Int
    | Div Expr Expr
    deriving (Show)

-- Implement evaluators for expressions

-- Unsafe evaluator
unsafeEval :: Expr -> Int
unsafeEval (Val v) = v
unsafeEval (Div e1 e2) = unsafeEval e1 `div` unsafeEval e2

-- ghci> show $ unsafeEval okExpr
-- "2"
okExpr :: Expr
okExpr = Div (Div (Val 4) (Val 2)) (Val 1)

-- ghci> unsafeEval errExpr
-- *** Exception: divide by zero
errExpr :: Expr
errExpr = Val 2 `Div` (Val 1 `Div` Val 3)

-- Evaluator with maybe monad: handles divide by 0 error by representing it as a Nothing
-- ghci> show $ evalMaybe okExpr
-- "Just 2"
-- ghci> show $ evalMaybe errExpr
-- "Nothing"
-- ghci>
evalMaybe :: Expr -> Maybe Int
evalMaybe (Val n) = return n
evalMaybe (Div e1 e2) = do
    r1 <- evalMaybe e1
    r2 <- evalMaybe e2
    if r2 == 0
      then Nothing
      else return $ r1 `div` r2

-- Evaluator with either monad: handles divide by 0 error by returning a Left value with precise information
-- about the error
-- ghci> show $ evalEither okExpr
-- "Right 2"
-- ghci> show $ evalEither errExpr
-- "Left \"Error dividing Val 2 by Div (Val 1) (Val 3)\""
evalEither :: Expr -> Either String Int
evalEither (Val n) = return n
evalEither (Div e1 e2) = do
    r1 <- evalEither e1
    r2 <- evalEither e2
    if r2 == 0
      then Left $ errorS e1 e2
      else return $ r1 `div` r2

errorS :: Show a => a -> a -> String
errorS y m = "Error dividing " ++ show y ++ " by " ++ show m

-- Implement profiler for expressions

-- Implement the state type; we could've imported the library function from
-- Control.Monad.Trans.State.Lazy, but this is an opportunity to practice
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State sas) = State $ first f . sas

instance Applicative (State s) where
    pure val = State (val,)

    (<*>) (State sfs) (State sas) = State $ \s ->
        let (f, s1) = sfs s
            (a, s2) = sas s1
        in (f a, s2)

instance Monad (State s) where
    return = pure

    (>>=) (State sas) aSb = State $ \s ->
        let (a, s1) = sas s
        in runState (aSb a) s1

get' :: State s s
get' = State $ \s -> (s, s)

put' :: s -> State s ()
put' newState = State $ const ((), newState)

-- Next, profiler type and a helper function
type Prof a = State Int a

tickProf :: Prof ()
tickProf = do
    currState <- get'
    put' $ currState + 1

-- An evaluator that profiles the number of divisions
evalProf :: Expr -> Prof Int
evalProf (Val n) = return n
evalProf (Div e1 e2) = do
    r1 <- evalProf e1
    r2 <- evalProf e2
    tickProf
    return $ r1 `div` r2

-- ghci> goProf okExpr
-- "value: 2, count: 2"
-- But divide by 0 error will occur now
goProf :: Expr -> String
goProf e = "value: " ++ show x ++ ", count: " ++ show s
  where
    (x,s) = runState (evalProf e) 0 :: (Int, Int)

-- Monad transformers are the solution to incorporate the functionality of different
-- monads; note the following steps:

-- 1. Define typeclasses to describe monads with particular features

class Monad m => MonadError e m where
    throwError :: e -> m a

instance MonadError s (Either s) where
    throwError = Left

class Monad m => MonadState s m where
    get :: m s

    put :: s -> m ()

instance MonadState s (State s) where
    get = get'

    put = put'

tickStateInt :: (MonadState Int m) => m ()
tickStateInt = do
    currentState :: Int <- get
    put $ currentState + 1

-- 2. Define an eval function that does the profiling and catches the div by zero error

evalExprFinallyTagless :: (MonadError String m, MonadState Int m) => Expr -> m Int
evalExprFinallyTagless (Val n) = pure n
evalExprFinallyTagless (Div e1 e2) = do
    r1 <- evalExprFinallyTagless e1
    r2 <- evalExprFinallyTagless e2
    if r2 == 0
      then throwError $ errorS r1 r2
      else do
          tickStateInt
          pure $ r1 `div` r2

-- Creating a mega monad (as opposed to a transformer)

newtype Mega a = Mega { runMega :: Int -> Either String (a, Int) }

-- How is this possible if Monad is a subclass of applicative is a
-- subclass of functor?
instance Monad Mega where
    return :: a -> Mega a
    return a = Mega $ \s -> Right (a, s)

    (>>=) :: Mega a -> (a -> Mega b) -> Mega b
    ma >>= fmb = Mega $ \s0 -> do -- take advantage of Monad instance of Either
        (a1, s1) <- runMega ma s0
        runMega (fmb a1) s1

instance Applicative Mega where
    pure = return
    (<*>) = ap

instance Functor Mega where
    fmap = liftM

instance MonadError String Mega where
  throwError :: String -> Mega a
  throwError = Mega . const . Left

instance MonadState Int Mega where
  get   = Mega $ \s -> Right (s, s)

  put x = Mega $ const $ Right ((), x)

-- Function to evaluate a Mega computation
evalMega :: Expr -> String
evalMega e = case runMega (evalExprFinallyTagless e) 0 of
    Left err ->
        "The following error occurred: " ++ err
    Right (res, numDivs) ->
        "Result " ++ show res ++ " obtained with " ++ show numDivs ++ " divisions"

-- Issue with this approach: despite having defined MonadError and
-- MonadState instances for State and Either, we had to define
-- separate instances for Mega monad. We need an approach that
-- composes these instances.

-- Monad transformer for exceptions: nest exception (either) monad with any monad stack
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

-- Define monad, applicative, and functor instances by using the corresponding instances for the
-- parameterised monad
instance Monad m => Monad (ExceptT e m) where
    return :: a -> ExceptT e m a
    return = ExceptT . return . return

    (>>=) :: ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
    (>>=) (ExceptT mea) aExTmb = ExceptT $ mea >>= \eitherA ->
        case eitherA of
            Left err -> return $ Left err
            Right a -> runExceptT $ aExTmb a

instance Monad m => Applicative (ExceptT e m) where
    pure = return
    (<*>) = ap

instance Monad m => Functor (ExceptT e m) where
    fmap = liftM

-- Define MonadError instance for ExceptT
instance Monad m => MonadError e (ExceptT e m) where
    throwError :: e -> ExceptT e m a
    throwError = ExceptT . return . Left

-- Monad transformer for state: nest state monad with any monad stack

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-- Define monad, applicative, and functor instances by using the corresponding instances for the
-- parameterised monad
instance Monad m => Monad (StateT s m) where
    return :: a -> StateT s m a
    return val = StateT $ \s -> return (val, s)

    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    (>>=) (StateT sma) asmb = StateT $ \s0 ->
        sma s0 >>= \(a, s1) -> runStateT (asmb a) s1

instance Monad m => Applicative (StateT s m) where
    pure  = return
    (<*>) = ap

instance Monad m => Functor (StateT s m) where
    fmap  = liftM

-- Define MonadState instance for StateT
instance Monad m => MonadState s (StateT s m) where
    get :: StateT s m s
    get = StateT $ \s -> return (s, s)

    put :: s -> StateT s m ()
    put newState = StateT $ const $ return ((), newState)

-- From the lecture notes:

-- If m is a monad, then StateT s m is a state monad (i.e. an instance of MonadState)
-- If m is a monad, then ExceptT e m is an error monad (i.e. an instance of MonadError)

-- But, what about StateT s (ExceptT e m)? We know it is a state monad by the above.
-- But, we'd also like it to be an error monad.

-- In other words, we need the following "pass through" properties to hold:

-- If m is a state monad, then ExceptT e m is still a state monad
-- If m is an error monad, then StateT Int m is still an error monad

-- This "passing through" property is enabled by "lifting" the operations that can be performed
-- on a monad m into the monad t m (where t is the transformer monad)

-- from Control.Monad.Trans (among other places)
class MonadTrans (t :: (Type -> Type) -> Type -> Type) where
    lift :: Monad m => m a -> t m a

-- ExceptT instance of MonadTrans
instance MonadTrans (ExceptT e) where
    lift :: Monad m => m a ->  ExceptT e m a
    lift ma = ExceptT $ return <$> ma

-- StateT instance of MonadTrans
instance MonadTrans (StateT t) where
    lift :: Monad m => m a ->  StateT s m a
    lift ma = StateT $ \s -> (, s) <$> ma

-- Why is this useful?

-- Defining MonadError instance for StateT monad
instance MonadError e m => MonadError e (StateT s m) where
    -- throw error using MonadError instance of monad being transformed, and then lift computation
    -- into StateT monad
    throwError :: e -> StateT s m a
    throwError = lift . throwError

-- Defining MonadState instance for ExceptT monad
instance MonadState s m => MonadState s (ExceptT e m) where
    get :: ExceptT e m s
    get = lift get

    put :: s -> ExceptT e m ()
    put = lift . put

-- Putting it all together:

-- Recall evalExprFinallyTagless; this function specifies how to evaluate a monadic computation
-- where the monad is an instance of both the MonadError and MonadState type class. Since:

-- 1. (ExceptT e m) is an instance of MonadError and MonadState (via the MonadTrans class),
-- we should be able to represent the evaluation of Expr as ExceptT computations

evalAsExceptT :: Expr -> ExceptT String (State Int) Int
evalAsExceptT = evalExprFinallyTagless

-- 2. (StateT e m) is an instance of MonadState and MonadError (via the MonadTrans class),
-- we should be able to represent the evaluation of Expr as MonadState computations

evalAsStateT :: Expr -> StateT Int (Either String) Int
evalAsStateT = evalExprFinallyTagless

-- We have created 2 different interpreters for Expr, one that interprets it as an ExceptT
-- computation, and one that evaluates it as a StateT computation. Run these interpreters:

runExceptTExprInterpreter :: Expr -> IO ()
runExceptTExprInterpreter expr =
    let stateComp = runExceptT $ evalAsExceptT expr
        (eCompResult, numDiv) = runState stateComp 0
    in case eCompResult of
            Left err ->
                putStrLn $ "Computation failed with following error: " <> err
            Right val ->
                putStrLn $ "Computation succeeded with result: " <> show val <> " with #divs: " <> show numDiv

runStateTExprInterpreter :: Expr -> IO ()
runStateTExprInterpreter expr =
   let eitherComp = runStateT (evalAsStateT expr) 0
   in case eitherComp of
        Left err ->
            putStrLn $ "Computation failed with following error: " <> err
        Right (val, numDiv) ->
            putStrLn $ "Computation succeeded with result: " <> show val <> " with #divs: " <> show numDiv

-- ghci> runExceptTExprInterpreter okExpr
-- Computation succeeded with result: 2 with #divs: 2

--  ghci>  runExceptTExprInterpreter errExpr
-- Computation failed with following error: Error dividing 2 by 0
