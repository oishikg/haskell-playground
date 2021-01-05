{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures    #-}
module MonadsAndTransformers
  (
  ) where

{- | 1. Types

Haskell is a statically typed language, i.e., all expressions in Haskell are associated with
some type. At a rudimentary level, the Haskell type system is similar to the type systems of
other statically typed languages like Java or OCaml. However, unlike these type systems,
the Haskell type system is more complex and sophisticated. This is evidenced, for example, by
the higher-kinded types, dependant types, type contstraints etc, features that are generally
not found in mainstream languages.

Types in Haskell can broadly be categorized as follows:

(1) Primitive datatypes: These are types like @Int@ or @Bool@ that represent the /primitive/
values in the the language (i.e., types that cannot be further decomposed into more primitive
types).

(2) Algebraic datatypes: These are types of the form

@
data T
    = C1 t11 t12 ...
    | C2 t21 t22 ...
    ...
    | Cm tm1 tm2 ...
@

which are constructed through a combination of user defined constructors, primitive types, and
other user defined Haskell types. Generally speaking, algebraic datatypes are a combination
of product types: -}

data SomeProductType = SomeProductType Int String

-- | or sum types

data SomeType
   = C1
   | C2
   | C3

-- | or a combination of both

data AlgebraicDt
    = F1 Int String
    | F2 Bool
    | F3

{-| (3) Parameterized datatypes: These are types that can be parameterized with other Haskell types.
These are of the form:

@
data T t1 t2 t3 ... tn = ...
@

(4) Recursive datatypes, in which the type constructors refer to the very type they are
constructing. The most common example of this is a list:

@
data List Int
    = Nil
    | Cons Int (List Int)
@

If you have worked with a statically typed functional programming language like OCaml before,
then these types should be familiar to you.
-}

{- | 2. Kinds

Let us now move to the realm of kinds. A kind refers to the types /of a type/_, and Haskell
provides extensive machinery to manipulate values at the kind level. Most of this machinery,
while interesting, is not required for basic Haskell usage in a production environment. However,
having a basic understanding of kinds is essential to understanding concepts like functors/
applicatives / monads in Haskell.

Try to think of kinds the same way as you would think of types. A type, roughly speaking,
refers to a collection of /values/ that have some common property. Thus, for example,
the type @List Int@ refers to all those Haskell values that have the following property:

the values is constructed using either the @Nil@ or the @Cons@ constructor. In the latter case,
the @Cons@ constructor takes two arguments: the first one is an integer, and the second one
is another @List Int@ value.

Likewise, a kind is a collection of /types/ that have some common property. Based on this
common property, we can divide types into the following kinds:

(1) Monotypes: These are types (or type constructors) that are /not/ parameterized by other Haskell types. That is,
they do not have any type variables in their definition. Examples of monotypes include
@Int@, @Bool@, etc. Monotypes have kind @*@, and can be thought of as /primitive/ values
at the type level.

(2) Higher-order types: These are types (or type constructors) that /are/ parameterized by
other Haskell types. Examples of such types include @Maybe@, @Either@, @[]@ etc. Such types
have kind @* -> ... -> *@, depending on the number of type parameters the type constructor
takes as arguments.

Naturally, there is much to unpack in these definitions. Let us attempt to do so with the
following questions:
--}

-- | Question 2.1: Why is @Int@ a monotype, i.e., why is it of kind @*@?

-- | Question 2.2: Consider the type @AlgebraicDt@ that we define above. What is its kind?

{- | Question 2.3: Consider the definition of the @Maybe@ datatype (look this up using
Hoogle). The @Maybe@ type constructor has kind @* -> *@. You can verify this by running
the following command in ghci:

@
λ> :k Maybe
Maybe :: * -> *
@

Can you explain why this is the case?
-}

{-| Question 2.4: Consider the @Either@ datatype. What is the kind of this datatype? Why
does it have this kind?
-}

-- | Question 2.5: Now consider the the following type:

data MyList a
    = MyNil
    | MCons a (MyList a)

{-|
It has kind @* -> *@ because the type parameter @a@ must be a monotype in order to be representable
as a list element. GHC will throw an error if we try to pass the @MyList@ type constructor a
higher kinded type (try this out).

This tells us something fundamental about types of kind @*@: valid Haskell expressions at
the /value/ level must have a type of kind @*@. Can you explain why? (Hint: Try writing out
a Haskell value of type @Maybe@ or @Either@)
-}

-- | Question 2.6: Explain why all functions in Haskell are of kind @*@?

-- | Question 2.7: Consider the following type

data MyT m a = MyT { runMyT :: m a }

-- | What should the kind of type @MyT@ be?


{-| 3. Type classes

In the previous section, we attempted to understand kinds by referring to common properties
that types should have. With typeclasses, we extend this idea grouping types on the basis of
some notion of commonality.

Specifically, a type class groups together types for which one or more common operations
specified by the type class definition can be defined. Consider, for instance, the following
type class definition:
-}

class MySum (a :: *) where
    (<+>) :: a -> a -> a

{- | This type class definition /specifies/ the common operation, i.e., the @(<+>)@ operation.
Thus, all types for which we can define the @(<+>)@ operation belong to the typeclass @MySum@.
Of course, these definitions don't write themselves (well, you could derive them using GHC's
deriving machinery, but let's not consider that for now). Thus, we define /instances/ of
the type class as follows:
-}

instance MySum Int where
    (<+>) a b = a + b -- Note that the second @+@ operator refers to the in-built addition operator for integers

instance MySum Bool where
    (<+>) False False = False
    (<+>) True False  = True
    (<+>) False True  = True
    (<+>) True True   = True

{- | Question 3.1: Define a @String@ instance of the @MySum@ type class and explain why
you have defined it this way.
-}

{- | Question 3.2: Define a type class named 'MyLength'. This type class should specify
two operations:

(1) @myLength@: this operation captures some notion of length of the data type concerned
(2) @isEmpty@: this operation reflects some notion of emptiness for the data type concerned

Explain the type signatures you have chosen for the specified operations.
-}

{- | Question 3.3: Try to define a @MyLength@ instance for @Maybe@ and @[]@. Note that you
/cannot/ parameterize the @Maybe@ and @[]@ type constructors with any type (Hint: If you are
stuck with this, you probably need to redefine the type class).

-}

{- | Solving question 3.3 is key to understanding type classes like Functor, Alternative,
Monad, Traversable, Foldable etc.

A final note on Haskell type classes: if you have worked with OCaml before, this may appear
similar OCaml's module system, or Java's interface-class system (note: in Java, instances of
classes really refer to objects of a class that implements an interface). The power of the
type class system in Haskell, however, lies in the fact that we can ... (revealing this
will give away the answer to 3.3, so think about this yourself!)
-}

{- | 4. Monoids -}

{- | 5. Functor -}

{- | 6. Applicative }

{- | 7. Monad -}

{- | 8. Reader -}

{- | 9. State -}

{- | 10. Nesting functors and applicatives -}

{- | 11. Nesting monads? A motivation for monad transformers -}

{- | 12. Monad Transformers -}

{- | 13. ReaderT -}

{- | 14. StateT -}



data Tree a
    = Leaf
    | Node (Tree a) a (Tree a)

{-
λ> :k Tree
Tree :: * -> *
λ> :t Leaf
Leaf :: Tree a
λ> :t Node
Node :: Tree a -> a -> Tree a -> Tree a
λ>
-}

-- data MyEither error success
--     = Left error
--     | Right success

{-
λ> :k MyEither
MyEither :: * -> * -> *
λ> :k (MyEither String)
(MyEither String) :: * -> *
λ> :k (MyEither String Int)
(MyEither String Int) :: *
λ>
-}

-- | Typeclasses

-- | Monoids

-- | Functor
data MyMaybe a
   = MyNothing
   | MyJust a
   deriving (Show)

instance Functor (MyMaybe) where
    fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
    fmap f MyNothing  = MyNothing
    fmap f (MyJust a) = MyJust $ f a

data MyEither a b
    = MyLeft a
    | MyRight b
    deriving (Show)

instance Functor (MyEither String) where
    fmap :: (a -> b)
         -> MyEither String a -- f
         -> MyEither String b -- (MyRight a)
    fmap f (MyLeft str) = MyLeft str
    fmap f (MyRight a)  = MyRight $ f a

foo :: (Int -> Int) -> Either String Int -> Either String Int
foo f (Left str) = Left str
foo f (Right x)  = Right $ f x

foo' :: (Int -> Int) -> Either String Int -> Either String Int
foo' f eitherVal = fmap f eitherVal


-- | Type wrapping a function that reads from a fixed environment
-- newtype MyReader r a = MyReader { unMyReader :: r -> a }



-- | Functor
-- instance Functor (MyReader r) where
--    fmap :: (a -> b) -> MyReader r a -> MyReader r b
--    fmap f (MyReader ra) =














































-- | Functor: What does this intuitively mean?
-- instance Functor (MyReader r) where
--   fmap :: (a -> b) -> MyReader r a -> MyReader r b
--   fmap f (MyReader ra) = MyReader $ f . ra

-- -- | Applicative: What does intuitively mean?
-- instance Applicative (MyReader r) where
--   pure :: a -> MyReader r a
--   pure a = MyReader $ const a

--   (<*>) :: MyReader r (a -> b) -> MyReader r a -> MyReader r b
--   (<*>) (MyReader rab) (MyReader ra) = MyReader $ \r -> rab r $ ra r

-- -- | Monad: What does intuitively mean?
-- instance Monad (MyReader r) where
--   return :: a -> MyReader r a
--   return = pure

--   (>>=) :: MyReader r a -> (a -> MyReader r b) -> MyReader r b
--   (>>=) (MyReader ra) farb = MyReader $
--       \r -> let MyReader rb = farb $ ra r -- observe how the environment is threaded
--             in rb r

-- -- | Digression into composing types
-- newtype Compose f g a = Compose (f (g a)) deriving Show

-- -- | Some generic types
-- newtype T1 a = T1 { unT1 :: a } deriving Show

-- instance Functor T1 where
--   fmap :: (a -> b) -> T1 a -> T1 b
--   fmap f (T1 a) = T1 $ f a

-- instance Applicative T1 where
--   pure :: a -> T1 a
--   pure a = T1 $ a

--   (<*>) :: T1 (a -> b)

-- newtype T2 a = T2 { unT2 :: a } deriving Show
