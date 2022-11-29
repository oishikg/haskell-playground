{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module ServantDemo.TypeFamilies
  (
  ) where

import           Data.Kind    (Type)
import           GHC.TypeLits (Symbol)

{-| Before we start talking about servant, we want to briefly understand type families. There are two articles
that I found helpful in understanding this topic:

For a detailed overview by Vladislav Zavialov:
https://serokell.io/blog/type-families-haskell#type-constructor-flavours

For an easier and more application based overview (more relevant to us) by Monday Morning Haskell:
https://mmhaskell.com/blog/2019/2/4/why-haskell-v-type-families

Key points:
- Two kinds of type families: closed type families and open type families
- Closed type families: Are functions at the type level, and map types to other types closed type families are
useful when dealing with GADTs, but they are beyond the scope of this discussion. If interested, check out
lectures 4, 5, and 6 from this repository:

https://github.com/i-am-tom/haskell-exercises

The following example has been taken from the above haskell exercises. This serves only as a quick overview of
closed type families.
-}

-- | Value level datatype declaration; elevanted to kind level using @DataKinds@ extension; @Nat@ is a kind, and
-- @'Z@ and @S'@ are types of kind @Nat@
data Nat = Z | S Nat deriving (Show)

-- | Standard value level function
add :: Nat -> Nat -> Nat
add  Z    y = y
add (S x) y = S (add x y)

-- | Type level function (closed type family)
type family Add (x :: Nat) (y :: Nat) :: Nat where
    Add  'Z    y = y
    Add ('S x) y = 'S (Add x y)

{- | Let us now consider open type families.

- These are similar to typeclasses at the type level.
- Provides an interface definition with kind and arity, and then multiple types can define an instance of this
type family.
- Allows a p

The following is an example from Zavialov's blog. Note that:

- @Symbol@ is the /kind/ of all type level strings
- @Label@ is a type family that maps a type to some string representation/label of the type. This could be
useful in many cases (serialization, custom show instances etc.)
-}
type family Label (t :: Type) :: Symbol where
    Label Bool   = "Boolean"
    Label Int    = "Number"
    Label Double = "Number"
    Label String = "String"
