module ReaderMonad
    (

    ) where

{- | With a general understanding of @Monad@ and @IO@ behind us, we are in a position to explore intermediate
Haskell concepts. We start this exploration with @Reader@

Let us first define a type alias to the @Reader@ type, @MyReader@
-}
newtype MyReader r a = MyReader { runMyReader :: r -> a }

data Foo = Foo {foo1 :: Int, foo2 :: String} deriving (Show, Eq)


{-| @MyReader@ is thus a @newtype@ wrapper around a function that takes a value of type @r@ and returns a value
of type @a@. It might not straightaway be clear why we would define the type in this manner, so let us
take some time playing around with and exploring the type.

As usual, let us start by defining the @Functor@ instance:
-}
instance Functor (MyReader r) where
    fmap :: (a -> b) -> MyReader r a -> MyReader r b
    fmap f (MyReader ra) = MyReader $ f . ra -- \r -> f $ ra r

instance Applicative (MyReader r) where
    pure :: a -> MyReader r a
    pure val = MyReader $ const val

    (<*>) :: MyReader r (a -> b) -> MyReader r a -> MyReader r b
    (<*>) (MyReader rab) (MyReader ra) = MyReader $ \r -> let a = ra r
                                                              ab = rab r
                                                              in ab a

-- rab r $ ra r
-- ::: (a -> b) $ (a)
-- ::: b

-- MyReader r ab ::: r -> (a -> b)
-- MyReader r a ::: r -> a
-- MyReader r b :::  r -> b

instance Monad (MyReader r) where
    return :: a -> MyReader r a
    return = pure

    (>>=) :: MyReader r a -> (a -> MyReader r b) -> MyReader r b
    -- (>>=) (MyReader ra) arb = MyReader $ \r -> let a = ra r
    --                                            in (runMyReader $ arb a) r

    (>>=) (MyReader ra) arb = MyReader $ \r -> let a = ra r
                                                   MyReader rb = arb a
                                               in rb r


-- runMyReader :: MyReader r b -> r -> b
-- arb :: (a -> MyReader r b)
-- arb a :: MyReader r b
-- runMyReader $ arb a :: (MyReader r b -> r -> b) (MyReader r b) ::: r -> b



-- MyReader r a ::: r -> a
-- (a -> MyReader r b) ::: a -> (r -> b)
-- MyReader r b ::: r -> b

-- | To retrieve the environment
askMyReader :: MyReader r r
askMyReader = MyReader id

-- | To modify the environment, and then pass the modified environment to the reader computation
localMyReader :: (r -> r) -> MyReader r a -> MyReader r a
localMyReader modifyEnv (MyReader ra) = MyReader $ \r -> let newEnv = modifyEnv r
                                                         in ra newEnv

-- | Experiments
data SomeEnv = SomeEnv
    { sdgToInr      :: !Int
    , sdgToEur      :: !Int
    , sdgToUsd      :: !Int
    , sdgToRealLink :: !String --(www.get-exchange-rate-sgd-real.com)
    } deriving (Show, Eq)

ourEnv :: SomeEnv
ourEnv = SomeEnv
    { sdgToInr = 50
    , sdgToEur = 3
    , sdgToUsd = 2
    , sdgToRealLink = "www.get-exchange-rate-sgd-real.com"
    }

getStockPriceInr :: Int -> MyReader SomeEnv Int
getStockPriceInr stockPrice = do
    env <- askMyReader
    let priceInInr = stockPrice * 4 * sdgToInr env
    return priceInInr

-- -- Undo the do-notation
getStockPriceInr' :: Int -> MyReader SomeEnv Int
getStockPriceInr' stockPrice =
    askMyReader >>= (\env -> return $ stockPrice * 4 * sdgToInr env)

-- -- Inline the bind oeprator
someReaderComputation'' :: Int -> MyReader SomeEnv Int
someReaderComputation'' stockPrice =
    MyReader $ \env -> let a = runMyReader askMyReader env
                           rb = (\env -> return $ stockPrice * 4 * sdgToInr env) a
                       in runMyReader rb env

getStockPriceUsd :: Int -> MyReader SomeEnv Int
getStockPriceUsd stockPrice = do
    env <- askMyReader
    let priceInUsd = stockPrice * 4 * sdgToUsd env
    return priceInUsd

--  The following code doesn't work because we are trying to perfomr @IO@ computations
-- in a @MyReader@ context

-- getStockPriceReal :: Int -> MyReader SomeEnv Int
-- getStockPriceReal stockPrice = do
--     env <- askMyReader
--     let link = sdgToRealLink env
--     sdgToReal <- getRate link -- Doesn't work!
--     let priceInReal = stockPrice * 4 * sdgToReal
--     return priceInReal

-- getRate :: String -> IO Int
-- getRate _ = pure 3

putThoseReaderComputationsTogether :: Int -> MyReader SomeEnv (Int, Int)
putThoseReaderComputationsTogether stockPrice = do
   inr <- getStockPriceInr stockPrice
   usd <- getStockPriceUsd stockPrice
   return (inr, usd)

-- putThoseReaderComputationsTogether' :: Int -> MyReader SomeEnv (Int, Int)
-- putThoseReaderComputationsTogether' stockPrice =
--     someReaderComputation stockPrice >>= (\somePrice -> anotherReaderComputation stockPrice >>= (\anotherPrice -> pure (somePrice, anotherPrice)))

runReaderComputation :: SomeEnv -> MyReader SomeEnv (Int, Int) -> (Int, Int)
runReaderComputation env action = runMyReader action env
