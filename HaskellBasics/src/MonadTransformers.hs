module MonadTransformers
    (

    ) where

{- | Nesting functors and applicatives -}
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

{- | 10. Nesting monads? A motivation for monad transformers -}

-- Unfortunately, this cannot be done for monads

{- | 11. MaybeT -}

newtype MyMaybeT m a = MyMaybeT { runMyMaybeT :: m (Maybe a) }

mmtTestVal1 :: MyMaybeT Maybe Integer
mmtTestVal1 = MyMaybeT $ Just (Just 3)
