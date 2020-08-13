module ReverseListCps
    ( testReverse
    , rev
    , revCps
    , revWithAcc
    , revWithAccCps
    , revNoel
    , revListWithHughList
    , revListUsingFunctions
    , revListUsingFunctionsCps
    , revListCopyOfNoel
    , revListCopyOfNoelInlinedKConst
    ) where


-- | Unit tests
testReverse :: ([Int] -> [Int]) -> Bool
testReverse candidate =
  let ts0 = [1, 2, 3, 4]
      ts0Rev = [4, 3, 2, 1]
      r0 = (candidate ts0 == ts0Rev)

      ts1 = [4, 2, 6, 8]
      ts1Rev = [8, 6, 2, 4]
      r1 = (candidate ts1 == ts1Rev)

      ts2 = [2, 5, 1]
      ts2Rev = [1, 5, 2]
      r2 = (candidate ts2 == ts2Rev)

      ts3 = [7, 6, 2, 0]
      ts3Rev = [0, 2, 6, 7]
      r3 = (candidate ts3 == ts3Rev)
  in r0 && r1 && r2 && r3


-- | We first write the standard implementations of reverse and their
-- CPS equivalents to warm up

-- | Standard reversal without accumulator
rev :: [a] -> [a]
rev []        = []
rev (x : xs') = rev xs' ++ [x]

-- Sanity Check:
-- testReverse rev

-- | Reversal without accumulator in CPS
revCps :: [a] -> [a]
revCps xs = revCpsHelper xs id
  where
    revCpsHelper :: [a] -- ^ list to be reversed
                 -> ([a] -> [a]) -- ^ continuation
                 -> [a]
    revCpsHelper [] k = k []
    revCpsHelper (x : xs') k = revCpsHelper
                                 xs'
                                 (\rs -> k (rs ++ [x]))

-- Sanity Check:
-- testReverse revCps

-- | Standard reversal with accumulator
revWithAcc :: [a] -> [a]
revWithAcc xs = revWithAccHelper xs []
  where
    revWithAccHelper :: [a] -> [a] -> [a]
    revWithAccHelper [] acc        = acc
    revWithAccHelper (x : xs') acc = revWithAccHelper xs' (x : acc)

-- Sanity Check:
-- testReverse revWithAcc

-- | Reversal with accumulator in CPS
revWithAccCps :: [a] -> [a]
revWithAccCps xs = revWithAccCpsHelper xs [] id
  where
    revWithAccCpsHelper :: [a] -- ^ list to be reversed
                        -> [a] -- ^ accumulator
                        -> ([a] -> [a]) -- ^ continuation
                        -> [a]
    revWithAccCpsHelper [] acc k = k acc
    revWithAccCpsHelper (x : xs') acc k =
        revWithAccCpsHelper xs' (x : acc) (\rs -> k rs)

-- Sanity Check:
-- testReverse revWithAccCps

-- | Let us now take a look at Noel's solution
revNoel :: [a] -> [a]
revNoel ls = run (cont []) ls
  where
    run :: (([a] -> [a]) -> [a]) -> [a] -> [a]
    run c []     = c id -- If list is fully consumed, return the reversed list
    run c (x:xs) = run c' xs
      where
        c' = cont $ c f
        f = (x :)

    cont :: [a]               -- ^ consumes a list
         -> ([a] -> [a])      -- ^ Continuation on the reversed list produced
         -> [a]
    cont x f = f x

-- Sanity Check:
-- testReverse revNoel

-- | But what is happening in Noel's solution? It isn't clear to me
-- straightaway, but I get the sense that he is using the partially applied
-- cons function and composing them in the continuation to obtain a
-- O (n) time list reversal

-- | This reminds me of the conecpt of Hughes' lists;
-- here's the paper for reference: https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/lists.pdf
-- This is a fairly powerful idea to implement linear time list appends,
-- and Noel's solution seems to be doing something like this
data HughList a = HughList ([a] -> [a])

hughListToList :: HughList a -> [a]
hughListToList (HughList hls) = hls []

revListWithHughList :: [a] -> [a]
revListWithHughList xs = hughListToList $ revListWithHughListHelper xs
  where
    revListWithHughListHelper :: [a] -> HughList a
    revListWithHughListHelper [] = HughList id
    revListWithHughListHelper (x : xs') =
        let HughList hl = revListWithHughListHelper xs'
        in HughList $ hl . (x :)

-- Sanity Check:
-- testReverse revListWithHughList

-- | But a Hughes' list is simply a wrapper for a function that takes
-- a list and returns a list; let's try and implement this without the
-- wrapper
revListUsingFunctions :: [a] -> [a]
revListUsingFunctions xs = revListUsingFunctionsHelper xs []
  where
    revListUsingFunctionsHelper :: [a] -> ([a] -> [a])
    revListUsingFunctionsHelper [] = id
    revListUsingFunctionsHelper (x : xs') =
        (revListUsingFunctionsHelper xs') . (x :)

-- Sanity Check:
-- testReverse revListUsingFunctions

-- | Now let us implement 'revListUsingFunctions' in CPS
revListUsingFunctionsCps :: [a] -> [a]
revListUsingFunctionsCps xs = revListUsingFunctionsCpsHelper xs kInit []
  where
    revListUsingFunctionsCpsHelper :: [a] -- ^ Initial list
                                   -> (([a] -> [a]) -> ([a] -> [a])) -- ^ Continuation takes a Hughes' list and returns a Hughes' list
                                   -> ([a] -> [a]) -- ^ The helper returns a Hughes' list that is reified by applying it to the empty list
    revListUsingFunctionsCpsHelper [] k = k id
    revListUsingFunctionsCpsHelper (x : xs') k =
        revListUsingFunctionsCpsHelper
          xs'
          (\rs -> k $ rs . (x :))

    kInit :: (([a] -> [a]) -> ([a] -> [a]))
    kInit = id

-- Sanity Check:
-- testReverse revListUsingFunctionsCps

-- |This is starting to look similar to Noel's implementation; but there
-- is something interesting about Noel's implementation; he uses two
-- continuations; one that takes a function and converts it into a list,
-- and one that takes a list and converts into a function; let's see
-- if we can implement something similar
revListTwoConts :: [a] -> [a]
revListTwoConts xs = revListTwoContsHelper xs k1Init k2Init
  where
    revListTwoContsHelper :: [a]
                          -> (([a] -> [a]) -> [a])
                          -> ([a] -> ([a] -> [a]))
                          -> [a]
    revListTwoContsHelper [] k1 k2 = k1 $ k2 []
    revListTwoContsHelper (x : xs') k1 k2 =
        revListTwoContsHelper
          xs'
          (\rfs -> k1 $ rfs . (x :))
          (\rs -> k2 rs)

    k1Init :: ([a] -> [a]) -> [a]
    k1Init f = f []

    k2Init :: [a] -> ([a] -> [a])
    k2Init _ = id

-- Sanity Check:
-- testReverse revListTwoConts

-- | And indeed, this works, and is more elegant than 'revListUsingFunctionsCps', because we don't need to
-- reify the Hughes' list into a list from the calling function; instead,
-- the helper function uses the second continuation to reify the Hughes'
-- list once all the partially applied cons functions with the list
-- elements have been composed in reverse order; write it out by hand
-- to convince yourself!

-- | But this is still not the same as Noel's version; we now write out
-- his version with notation more familiar to us, but preserving the
-- types and continuations
revListCopyOfNoel :: [a] -> [a]
revListCopyOfNoel xs = revListCopyOfNoelHelper kInit xs
  where
    kConst :: [a]
           -> ([a] -> [a])
           -> [a]
    kConst x f = f x

    kInit :: ([a] -> [a])
          -> [a]
    kInit = kConst []

    revListCopyOfNoelHelper :: (([a] -> [a]) -> [a])
                            -> [a]
                            -> [a]
    revListCopyOfNoelHelper k [] = k id
    revListCopyOfNoelHelper k (x : xs') =
        revListCopyOfNoelHelper
          (kConst $ k (x :))
          xs'

-- Sanity Check:
-- testReverse revListCopyOfNoel

-- | What happens now if we inline kConst?
revListCopyOfNoelInlinedKConst :: [a] -> [a]
revListCopyOfNoelInlinedKConst xs = revListCopyOfNoelInlinedKConstHelper kInit xs
  where
    kInit :: ([a] -> [a])
          -> [a]
    kInit f = f []

    revListCopyOfNoelInlinedKConstHelper :: (([a] -> [a]) -> [a])
                                         -> [a]
                                         -> [a]
    revListCopyOfNoelInlinedKConstHelper k [] = k id
    revListCopyOfNoelInlinedKConstHelper k (x : xs') =
        revListCopyOfNoelInlinedKConstHelper
        (\rf -> rf $ k (x :))
        xs'

-- Sanity Check:
-- testReverse revListCopyOfNoel

-- | And this works as well! Once we inline the 'kConst' function, we
-- note that Noel's solution does essentially does the same thing as 'revListTwoConts';
-- note how the base case of 'revListCopyOfNoelInlinedKConstHelper'
-- corresponds to the base case of 'revListTwoConts'; the only difference
-- is that we directly plug in the second continuation, which makes
-- more sense, because it is only used once in the base case

-- | Infact, in the inductive case for 'revListTwoConts', the second
-- continuation is represented as '(\rs -> k2 rs)'; but this is as good as
-- simply passing k2, which reemphasizes the fact that the second
-- continuation was redundant, and could've been done away with as in
-- 'revListCopyOfNoelInlinedKConst'
