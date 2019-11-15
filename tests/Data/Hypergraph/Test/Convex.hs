module Data.Hypergraph.Test.Convex where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Hypergraph
import Data.Hypergraph.Test.Arbitrary

import Data.List (sort)
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map

import Debug.Trace

-------------------------------
-- Tests

tests = testGroup "Data.Hypergraph.Convex"
  [ QC.testProperty "prop_selfMatchConvex" prop_selfMatchConvex
  , QC.testProperty "prop_emptyConvexMatch" prop_emptyConvexMatch
  , QC.testProperty "prop_selfDualComposition" prop_selfDualComposition
  ]

-- | The empty hypergraph always matches (convexly) in every graph.
prop_emptyConvexMatch :: OpenHypergraph Generator -> Property
prop_emptyConvexMatch g =
  let (m:_) = match empty g in nonConvex g m === []

-- TODO: handle all cases
prop_selfMatchConvex :: OpenHypergraph Generator -> Property
prop_selfMatchConvex g =
  let (m:_) = match g g
  in  nonConvex g m === []

-- prop_selfDualComposition generalizes the example of non-convex matching
-- from the original paper of Zanasi, Sobocinski, et al.
--
-- Pattern:
-- 
--   ------------ -------------
--               X
--     |---|----/ \----|---|
--   --| L |           | R |---
--     |---|-----------|---|
--
-- Context:
--
--    |---|---[ M ]---|---|
--  --| L |           | R |---
--    |---|-----------|---|
--
-- There is a match of the pattern in the context, but it is not convex,
-- because there is a directed path (via M) between nodes in the matching.
--
-- To generalize this, we get a random graph (L), and its dual (R), and paste
-- some random graph 'in between' that contains at least one hyperedge.
-- Then this constructed graph will always have one non-convex match.
-- 
-- NOTE: this relies on M having a directed path from its left to its right
-- boundary. To guarantee this, we just specify M as 'one smaller' than the
-- boundaries of L and R
-- 
-- TODO: the implication operator dicards *lots* of cases; shouldn't we always
-- get a match?
prop_selfDualComposition
  :: OpenHypergraph Generator -- ^ The left (and right-) hand sides of the diagram
  -> Property
prop_selfDualComposition left' =
  let (m:_) = match pattern context
  in  isConvex context m === False
  where
    (k, n') = toSize left'
    n = max n' 2
    left = adaptSize k n left'
    right = dual opposite left
    mid   = singleton (Generator 0 (n - 1, n - 1))

    pattern = (identity <> left) → (twist <> identityN (n-1)) → right
    context = left → (mid <> identity) → right
