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
  [ QC.testProperty "prop_selfDualComposition" prop_selfDualComposition
  ]

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
-- boundary. To guarantee this, we just use a singleton generator, but more
-- comprehensive tests could use more complicated hypergraphs.
prop_selfDualComposition
  :: OpenHypergraph Generator -- ^ The left (and right-) hand sides of the diagram
  -> Property
prop_selfDualComposition left' = [] === match pattern context
  where
    (k, n') = toSize left'
    n = max n' 2
    left = adaptSize k n left' -- left is a (k, n) hypergraph
    right = dual opposite left -- right is (n, k)
    mid   = singleton (Generator 0 (n - 1, n - 1)) -- mid is (n - 1, n - 1)

    pattern =
      (identity <> left) → (twist <> identityN (n-1)) → (identity <> right)
    context = left → (mid <> identity) → right
