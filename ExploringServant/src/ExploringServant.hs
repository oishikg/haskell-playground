{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module ExploringServant
  ( -- * Type-level constructors for DSL
    Get
  , (:<|>)
  , (:>)
  , Capture

  -- * Definitions for server interpretation of DSL
  , Server
  , HasServer (..)

  -- * Example API data
  , MyWebsiteAPI
  , myWebsiteAPIHandler
  ) where

import           Control.Applicative
import           Data.Proxy
import           Data.Time
import           GHC.TypeLits
import           Text.Read

-- | Based off of:  https://www.well-typed.com/blog/2015/11/implementing-a-minimal-version-of-haskell-servant/

{- | Intro

- Servant defines a /type-level/ DSL to specify an API with a well-defined grammar
- This is useful because API specifications in this DSL can then be interpreted in different contexts:
  - To write a type-safe server
  - To generated client
  - To generate documentation

- Today's session covers the following:
  - Define a smaller DSL that lets us define @GET@ APIs
  - Discuss how such APIs can be defined using the DSL
  - Build a server for the API that interprets the API spec in order to (1) route requests to the correct handler
and (2) statically verify that the API handlers have the correct type

-}

-- | The DSL

-- | Type representing a @GET@ endpoint
-- Limitations: No encoding type (use @String@); can be extended by adding parameters to the type constructor
data Get (a :: *)

-- | Type representing a choice between two routes
data a :<|> b = a :<|> b
infixr 8 :<|>

-- | Type representing a nested route: @a@ -> first component of path, @b@ -> rest of the path
data (a :: k) :> (b :: *)
infixr 9 :>

-- | Type representing a component of a route that needs to be /parsed/ and passed to its hanlder as a value of
-- type @a@
data Capture (a :: *)

{- | Example API
Suppose I have a website that does the following:
- Displays some information about myself
- Displays the current local time

I could just build this website on the FE, but for the sake of this contrived example, and in the spirit of
SPAs, let's suppose we want to define an API to fetch this information for this website. Informally speaking,
I need the following endpoints:
(1) A GET endpoint to fetch my information
(2) A GET endpoint parametrized by the time zone

We would then define using our DSL as follows:
-}
type MyWebsiteAPI =
    "my-info" :> Get String
    :<|>
    "current-local-time" :> Capture TimeZone :> Get ZonedTime

{- | Two questions at this point:
  - Why define a DSL at all for this?
  - Why define a type-level DSL for this?

These will become clearer once we interpret the API spec

-}

serve :: HasServer apiLayout
      => Proxy apiLayout
      -> Server apiLayout -- ^ Handler for API
      -> [String] -- ^ Path in HTTP request
      -> IO String
serve p handler path =
    case route p handler path of
        Nothing -> error "Error in HTTP request"
        Just s  -> show <$> s

{-| @Server@ type family: this is the key to guaranting correct handler types at compile time!

- Type family: Functions at the type level to map types to other types
- In this case, we are dealing with open type families
- Why are they useful? Here's a quote from the Haskell wiki (https://wiki.haskell.org/GHC/Type_families):

"Type families permit a program to compute what data constructors it will operate on, rather than having them
fixed statically (as with simple type systems) or treated as opaque unknowns (as with parametrically polymorphic
types)."

In other words, the compiler can, at compile time, /compute/ the expected handler type, given an API specified
in our DSL!

Let's concretize this intuition:
-}

-- | @Server@ type family definition f
type family Server apiLayout :: *;

-- | Mapping our DSL type-level constructors to the expected handler type

-- | E.g. @GET@ my information to display on the website
type instance Server (Get a) = IO a

-- | E.g. @GET@ my information or @GET@ the current local time
type instance Server (a :<|> b) = Server a :<|> Server b

-- | E.g. @GET@ information from the endpoint "my-info" :> Get String
type instance Server ((s :: Symbol) :> b) = Server b

-- | E.g. @GET@ information about the local time from endpoint
-- "current-local-time" :> Capture TimeZone :> Get ZonedTime
type instance Server (Capture a :> b) = a -> Server b

-- | Let's play around with @MyWebsiteAPI@ applying the rules specified by the @Server@ type family to infer
-- its handler type

-- | What about invalid representations of the API? Compiler error because compiler won't be able to compute
-- the type the @serve@ function should expect

-- | Examples of invalid base cases:
-- @Capture Bool@
-- @"get-my" :> "something"@ (but what?)


{- | So now we have some clarity on why the DSL is defined at the type-level. Had we defined the DSL at the
data level, we wouldn't have been able to infer the type of the handler as elegantly (though I imagine we
could still traverse the AST, and elevate each constructor to a corresponding type)
-}

{- | Next, let us consider the @HasServer@ typeclass:

At this point, we have:
- Defined a type-level DSL for API endpoints
- Have discussed how we can use the @Server@ type family to statically verify the type-correctness of the
hanlder for an API

However, we haven't yet discussed how the server would actually run post compilation. For our server to function
properly, it must also be able to /route/ requests to the correct handlers. This is where the @HasServer@
typeclass comes into play: it allows us to map the constructors of our type-level DSL to the correct handler
by exposing an interface that can be used by the different constructors.

--}

-- | Typeclass definition
class HasServer apiLayout where
    route :: Proxy apiLayout
          -> Server apiLayout -- ^ Handler for API component
          -> [String] -- ^ List with path components of the HTTP request
          -> Maybe (IO String)

-- | Instances

-- | For a @GET@ endpoint, if we have traversed the entire path provided by the HTTP request, then we
-- assume that the request is valid, and run the handler; in case there are still components that we have
-- /not/ traversed, then the request is malformed and we return @Nothing@
instance (Show a) => HasServer (Get a) where
    route :: Proxy (Get a) -> IO a -> [String] -> Maybe (IO String)
    route p handler []        = pure $ show <$> handler
    route p handler (x : xs') = Nothing

-- | For an API of the form @a :<|> b@, we try out the handler for @a@, and if this fails, then we try
-- the handler for @b@
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
    route :: Proxy (a :<|> b) -> (Server a :<|> Server b) -> [String] -> Maybe (IO String)
    route p (handlerA :<|> handlerB) pathComponents =
        route (Proxy :: Proxy a) handlerA pathComponents
        <|>
        route (Proxy :: Proxy b) handlerB pathComponents

-- | For an API of the form @s :> r@, where @s@ is a type-level string (and not a parameter), we see the real
-- power of routing an API defined as a Haskell type; the idea is that we want to check, for a given API and
-- path, whether the current path component matches the type-level string denoting a path component; if this
-- is the case, we continue to traverse the remaining path with the rest of the API (@r@); if not, the request
-- is malformed, and we return a @Nothing@ (which would resolve to a 404 error)

-- | For example, when suppose we have the API @"info" :> my-info :> Get String@, and the path in the request is
-- ["info", "my-info"]; the @route@ function will recursively traverse the API spec, first matching the type-level
-- string "info" with the first component of the HTTP request path, and so on and so forth until the @Get@
-- constructor is reached, at which point the handler is run
instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
    route :: Proxy (s :> r) -> Server r -> [String] -> Maybe (IO String)
    route p handlerR [] = Nothing
    route p handlerR (x : xs') =
        if x == symbolVal (Proxy :: Proxy s)
        then route (Proxy :: Proxy r) handlerR xs'
        else Nothing

-- | The last case to consider is similar to the previous case: an API with a parameter that needs to parsed;
-- in this case, we attempt to parse the current path componenet of the HTTP request, pass it to the handler,
-- and continue the routing process; however, if the path in the HTTP request is empty, then the request is
-- once again malformed, and we throw an error

instance (Read a, HasServer r) => HasServer (Capture a :> r) where
    route :: Proxy (Capture a :> r) -> (a -> Server r) -> [String] -> Maybe (IO String)
    route p handlerR [] = Nothing
    route p handlerR (x : xs') =
        case readMaybe x of
            Just a  -> route (Proxy :: Proxy r) (handlerR a) xs'
            Nothing -> Nothing

{- | Time to test our server!-}

-- | Let's first define our handlers
getMyInfoHandler :: IO String
getMyInfoHandler = pure "Here's some information about me: asdfhokjbboasdfhkjasbjkodf"

getLocalTimeHandler :: TimeZone -> IO ZonedTime
getLocalTimeHandler tz = utcToZonedTime tz <$> getCurrentTime

myWebsiteAPIHandler :: Server MyWebsiteAPI
myWebsiteAPIHandler = getMyInfoHandler :<|> getLocalTimeHandler

-- | At this point, some of this notation should start looking familiar to the notation you see when
-- you define APIs in your projects! We simply subsitute the @:<|>@ operator with product types, but the
-- idea is still the same

getMyInfoAction :: IO String
getMyInfoAction = serve (Proxy :: Proxy MyWebsiteAPI) myWebsiteAPIHandler ["my-info"]

getCurrentTimeUtcAction :: IO String
getCurrentTimeUtcAction = serve (Proxy :: Proxy MyWebsiteAPI) myWebsiteAPIHandler ["current-local-time", "UTC"]

getCurrentTimeNycAction :: IO String
getCurrentTimeNycAction = serve (Proxy :: Proxy MyWebsiteAPI) myWebsiteAPIHandler ["current-local-time", "EST"]

malformedRequestAction :: IO String
malformedRequestAction = serve (Proxy :: Proxy MyWebsiteAPI) myWebsiteAPIHandler ["non-existent-path"]
