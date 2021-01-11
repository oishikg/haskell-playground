{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures    #-}
module MonadsAndTransformers
  (
  ) where

{- | 1. Types

Roughly speaking, a type in some programming language refers to a collection of values in that
language which can be grouped together. It helps to think of types as being roughly analogous
to sets in mathematics.

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

Let us now move to the realm of kinds. A kind refers to the types /of a type/, and Haskell
provides extensive machinery to manipulate values at the kind level. Most of this machinery,
while interesting, is not required for Haskell usage in a production environment. However,
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

(1) Monotypes: These are types (or type constructors) that are /not/ parameterized by other Haskell
types. That is, they do not have any type variables in their definition. Examples of monotypes
include @Int@, @Bool@, etc. Monotypes have kind @*@, and can be thought of as /primitive/ values
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

newtype MyT m a = MyT { runMyT :: m a }

-- | What should the kind of type @MyT@ be?


{-| 3. Type classes

In the previous section, we attempted to understand kinds by referring to common properties
that types should have. With typeclasses, we extend this idea of grouping types on the basis of
some notion of commonality.

A Haskell type class specifies one or more operations which must adhere to certain properties.
All Haskell types for which these operations can be define are said to be /instances/ of the
type class. Thus, a type class groups together all types which can define the operations that
it (the typeclass) specifies in its own defintion. For instance, consider the following type
class definition:
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

{- | 4. Semigroups and Monoids -}

{- | Semigroups and Monoids are concepts that arise in the area of abstract mathematics. A
semigroup refers to a set with an associative binary operation defined on its elements. That is,
some set S is a semigroup is for all elements a, b, c \in S, there exists some binary operator
* such that:

a * (b * c) = (a * b) [law of associativity]

GHC defines a the @Semigroup@ typeclass based on this exact definition. That is, the @Semigroup@
typeclass specifies a binary associative operator (<>) that must satisfy the law of
associativity. Take a look at the hackage documentation of @Semigroup@ for more details:

https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Semigroup

All Haskell types for which this associative binary operation can be defined are instances of
the @Semigroup@ typeclass. Consider the following example:
-}

newtype MyInt = MyInt { unMyInt :: Int } deriving (Show, Eq)

instance Semigroup MyInt where
    (<>) (MyInt v1) (MyInt v2) = MyInt $ v1 + v2

{-| In this above example, @MyInt@ is an instance of the @Semigroup@ type class because we
can define the @<>@ operation for this type.
-}

-- | Question 4.1: Does the definition satisfy the law of associativity? If so, explain why.

-- | Question 4.2: Consider the following @Option@ type (isomorphic to the @Maybe@ type):

data Option a
    = None
    | Some a
    deriving (Show, Eq)

{- | 4.2.1: Why do we need to constrain the instance definition of @Option a@ such that
@a@ is also a @Semigroup@

   4.2.2: Does the following definition satisfy the law of associativity? Why so?
-}
instance Semigroup a => Semigroup (Option a) where
    (<>) (Some a) (Some b) = Some $ a <> b
    (<>) (Some a) None     = Some a
    (<>) None (Some b)     = Some b
    (<>) None None         = None

{- | 4.2.3: Can you think of another way to define the above instance such that the law of
associativity holds? [Note: When you try to define this second instance, GHC will complain
because it only allows types to define one instance of some typeclass. As a workaround, define
a newtype wrapper around the @Option@ type, and then define your instance for this newtype
wrapper.]
-}

{- | Now that we have a fairly clear understanding of what a @Semigroup@ is, let us move on to
monoids. In abstract algebra, a monoid is a semigroup with an identity element. That is, a set
S is a monoid iff:

(1) S is a semigroup with some associative binary operator (*)
(2) There exists an identity element id such that for any element v in S, the following
properties hold:

(a) id * v = v [left identity]
(b) v * id = v [right identity]

To transpose this mathematical definition to Haskell, the @Monoid@ typeclass in Haskell does
the following:
(1) Constrains types that can derive the @Monoid@ instance by requiring them to have a
@Semigroup@ instance
(2) Specifies an identity element for the type that obeys the identity laws listes above

For more details, see https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Monoid
-}

-- | 4.3: Define @Monoid@ instances for @MyInt@ and @MyOption@. Do you think the latter is possible?

{- | 4.4: It is not without reason that CIS194 devotes an entire lecture to @Monoid@. Monoids
occur very commonly in Haskell's type system, and the interface exposed by the @Monoid@ typeclass
is userful in many situations. Suppose we define some type:

@
data MySpecialString = MySpecialString { unMySpecialString :: !String }
@

Can you explain what the advantages would be of defining @Semigroup@ and @Monoid@ instances
(as opposed to defining an append function and an empty value for @MySpecialString@ values in
a some module)?
-}

{- | 4.5: @Semigroup@ is considered a /superclass/ of @Monoid@. What do you think this means? -}

{- | 5. Functor -}

{- | So far, we have discussed types and kinds, transitioned into type classes, and discussed
two typeclasses that are commonly used in Haskell: @Semigroup@ and @Monoid@. We now have the
tools and vocabulary required to tackle the @Functor@ type class.

We first take a look at the documentation for @Functor@:
https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Functor

and note three key points:

(1) @Functor@ specifies the commonly used @fmap@ operation, often aliased by the @<$>@ operator.

(2) The @fmap@ operation should satisfy the Functor Identity Law:
fmap id == id

(3) The @fmap@ operation should satisft the Functor Composition Law:
fmap (f . g) = fmap f . fmap g

Since maps are most commonly seen in the context of lists, let us start off by considering the
@MyList@ type that we defined in the section on Kinds:
-}

-- | 5.1: As a warm up, define @Show@ and @Eq@ instances for @MyList@.
instance (Show a) => Show (MyList a) where
    show = undefined

instance (Eq a) => Eq (MyList a) where
    (==) xs ys = undefined

-- | 5.2: Define a @Functor@ instance for @MyList@.
instance Functor MyList where
    fmap = undefined

{- | 5.3: Compare the instance definitions for @Show@ and @Eq@ with @Functor@. For some reason,
we define the @Functor@ instance for @MyList@ and /not/ @MyList a@. Intuitively, this makes
sense because when we map a function over a list, we don't really need to know what the
exact type of the elements in the list is, /as long as/ we know that the function being
mapped takes values of that type as input.

Given this consideration, what is the /kind/ of types for which a @Functor@ instance can be
defined? Can you explain how the /kind/ of the type connects to the discussion in the
paragraph above?
-}

{- | 5.4: Prove that your @Functor@ instance satisfies the Identity and Composition properties.
-}

-- | 5.5  Consider the following type, isometric to the @Either@ type in Haskell:
data MyEither a b
    = MyLeft a
    | MyRight b
    deriving (Show, Eq)

{- | As a short digression, spend some time understanding the relevance of defining a type like
this:

https://en.wikipedia.org/wiki/Result_type

Now that you have a better understanding of how this type might be used, can you define a
@Functor@ instance for @MyEither@?
-}
instance Functor (MyEither a) where
    fmap = undefined

{- | 5.5: Why is the instance defined for @MyEither a@ and not @MyEither@? Explain this in terms
of kinds, and in more intuitive terms (as we explained for lists above).
-}


{- | The power of the @Functor@ type class in Haskell lies in the following:

(1) It allows programmers to define maps for multiple data structures using a single data type
class. In comparison, in OCaml or Elm, a separate @map@ function has to be defined for every
data structure that is a Functor.
(2) ..... (if I write this, I will give away the answer to one of the questions above! See if
you can figure it our, otherwise we can discuss this)
-}

{- | 6. Applicative -}

{- | The @Applicative@ type class is fairly complex from a theoretical perspective. Consequently,
we will avoid a discussion of the same in these notes. However, to get an overview of the
properties that are expected to hold for instances of @Applicative@, take a look at the
documentation:

https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Applicative

The key points to note with regards to @Applicative@ are as follows:

(1) It is a subclass of @Functor@, i.e., all instances of @Applicative@ must necessarily have
@Functor@ instances.
(2) @Applicative@ allows us to apply a function to values contained in some structure, when
the function /itself/ is embedded in the same structure.
(3) @Applicative@ is defined for types that have the same kind as @Functor@ instances.
-}

-- | 6.1: Why is (3) the case?

{- | 6.2: Write functor  and applicative instances for @Option@.
Make sure to define the type signatures of the functions. We have enabled the
@InstanceSigs@ pragma for this purpose.
-}
instance Functor Option where
    fmap = undefined


instance Applicative Option where
    pure = undefined

    (<*>) = undefined


{- | 6.3: Here's a small application of the @Applicative@ instance of the @Option@ type:

Suppose we have 2 computations that return an @Option@ value, and we would like to put these
values into pairs. The naive way to do this is as follows:
-}
optionsToPairNaive :: Option a -> Option b -> Option (a, b)
optionsToPairNaive ma mb = case (ma, mb) of
    (Some a, Some b) -> Some (a, b)
    _                -> None

-- | 6.3.1: Explain why the return type of @optionsToPairNaive@ is @Option (a, b)@ and not @(a,b)@

-- | 6.3.2: How would you implement the following function:
secretFunction :: (Applicative f)
               => (a -> b -> c)
               -> f a
               -> f b
               -> f c
secretFunction abc fa fb = undefined

{- | 6.3.3: Can you implement @optionsToPairApplicative@, a function that uses @secretFunction@
the @Applicative@ structure of @Option@ to implement a one line solution for our problem.
-}
optionsToPairApplicative :: Option a -> Option b -> Option (a, b)
optionsToPairApplicative ma mb = undefined

{- | 6.4: The @secretFunction@ you just implemented is /lifting/ the function with type signature
@a -> b -> c@ into the computations represented by @f a@ and @f b@. The power applicative
lies in the fact that this pattern can be extended indefinitely. On that note, could
you define the following function?
-}
secretFunctionLift4 :: (Applicative f)
                    => (a1 -> a2 -> a3 -> a4 -> r)
                    -> f a1
                    -> f a2
                    -> f a3
                    -> f a4
                    -> f r
secretFunctionLift4 = undefined

{- | 6.5: If you recall exercises 10 and 11 from CIS 194, we saw similar patterns when implementing
parsers. The idea in the case of parsers is also the same; the @Parser@ type represents a
computation (that of parsing), and we would like to lift some function that constructs our
final data type into these functions.

On that note, consider the following data type:

@
data Person = Person
    { personName :: String
    , personAge  :: Int
    }
@

Suppose we have some @Parser@ type (as defined in the CIS194 exercises), and this type has
an @Applicative@ instance. We then define the following parsers:

@
nameParser :: Parser String
nameParser = ...

ageParser :: Parser Int
ageParser = ...
@

to parse name and age respectively.

Given some raw string @str :: String@, how would you parse this into a value of type @Person@?
You can write this answer in the comments since we don't actually have the required machinery
for the @Parser@ type.
-}

-- | 6.6: Explain why exactly an @Applicative@ parser allows us to write a solution for 6.5.

{- | Writing individual instances of parsers is generally not too difficult. I would
encourage you to take a look at the instances for other standard Haskell types like
@Either@ and @[]@ to develop a better intuition for what @Applicative@s might look like.

The difficulty with applicative (as with much of Haskell's machinery) is to cleary understand
how it could be applied, and how exactly it fits specific use cases. Consequently, we have spent
much of this section to building this intuition, and this was of course the reason why
CIS194 spends two exercises going over applicative parsers.
-}

{- | 7. Monad -}

{- | 8. Reader -}

-- newtype MyReader r a = MyReader { unMyReader :: r -> a }

-- instance Functor (MyReader r) where
--     fmap :: (a -> b) -> MyReader r a -> MyReader r b
--     fmap f (MyReader ra) = MyReader $ f . ra

-- instance Applicative (MyReader r) where
--     pure :: a -> MyReader r a
--     pure = MyReader . const

--     (<*>) :: MyReader r (a -> b) -> MyReader r a -> MyReader r b
--     (<*>) (MyReader rab) (MyReader ra) = MyReader $ \r -> rab r $ ra r



{- | 9. Nesting functors and applicatives -}

{- | 10. Nesting monads? A motivation for monad transformers -}

{- | 11. ReaderT -}

{- | 12. State and StateT -}




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

-- instance Functor (MyMaybe) where
--     fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
--     fmap f MyNothing  = MyNothing
--     fmap f (MyJust a) = MyJust $ f a

-- data MyEither a b
--     = MyLeft a
--     | MyRight b
--     deriving (Show)

-- instance Functor (MyEither String) where
--     fmap :: (a -> b)
--          -> MyEither String a -- f
--          -> MyEither String b -- (MyRight a)
--     fmap f (MyLeft str) = MyLeft str
--     fmap f (MyRight a)  = MyRight $ f a

-- foo :: (Int -> Int) -> Either String Int -> Either String Int
-- foo f (Left str) = Left str
-- foo f (Right x)  = Right $ f x

-- foo' :: (Int -> Int) -> Either String Int -> Either String Int
-- foo' = fmap


-- -- | Type wrapping a function that reads from a fixed environment
-- -- newtype MyReader r a = MyReader { unMyReader :: r -> a }



-- -- | Functor
-- -- instance Functor (MyReader r) where
-- --    fmap :: (a -> b) -> MyReader r a -> MyReader r b
-- --    fmap f (MyReader ra) =














































-- -- | Functor: What does this intuitively mean?
-- -- instance Functor (MyReader r) where
-- --   fmap :: (a -> b) -> MyReader r a -> MyReader r b
-- --   fmap f (MyReader ra) = MyReader $ f . ra

-- -- -- | Applicative: What does intuitively mean?
-- -- instance Applicative (MyReader r) where
-- --   pure :: a -> MyReader r a
-- --   pure a = MyReader $ const a

-- --   (<*>) :: MyReader r (a -> b) -> MyReader r a -> MyReader r b
-- --   (<*>) (MyReader rab) (MyReader ra) = MyReader $ \r -> rab r $ ra r

-- -- -- | Monad: What does intuitively mean?
-- -- instance Monad (MyReader r) where
-- --   return :: a -> MyReader r a
-- --   return = pure

-- --   (>>=) :: MyReader r a -> (a -> MyReader r b) -> MyReader r b
-- --   (>>=) (MyReader ra) farb = MyReader $
-- --       \r -> let MyReader rb = farb $ ra r -- observe how the environment is threaded
-- --             in rb r

-- -- -- | Digression into composing types
-- -- newtype Compose f g a = Compose (f (g a)) deriving Show

-- -- -- | Some generic types
-- -- newtype T1 a = T1 { unT1 :: a } deriving Show

-- -- instance Functor T1 where
-- --   fmap :: (a -> b) -> T1 a -> T1 b
-- --   fmap f (T1 a) = T1 $ f a

-- -- instance Applicative T1 where
-- --   pure :: a -> T1 a
-- --   pure a = T1 $ a

-- --   (<*>) :: T1 (a -> b)

-- -- newtype T2 a = T2 { unT2 :: a } deriving Show
