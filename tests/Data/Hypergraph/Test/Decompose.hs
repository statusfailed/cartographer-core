module Data.Hypergraph.Test.Decompose where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Maybe (fromJust)

import Data.Hypergraph
import Data.Hypergraph.Decompose
import Data.Hypergraph.Test.Matching (graphSize)
import Data.Hypergraph.Test.Arbitrary

-------------------------------
-- Tests

tests = testGroup "Data.Hypergraph.Rewrite"
  [ QC.testProperty "prop_iso" prop_iso
  , QC.testProperty "prop_decomposeSingletons" prop_decomposeSingletons
  , QC.testProperty "prop_decomposePairs" prop_decomposePairs
  , QC.testProperty "prop_decomposeRoundtrip" prop_decomposeRoundtrip
  ]

-- | Equality modulo IDs.
-- Hypergraphs are equal if the first matches at least once in the second,
-- and the graphs are of the same size (TODO: check if this is true!)
(~~) :: OpenHypergraph Generator -> OpenHypergraph Generator -> Bool
g ~~ h =
  (not . Prelude.null . take 1 $ match g h) && (graphSize g == graphSize h)

rebuild :: (Signature a, Eq a) => OpenHypergraph a -> OpenHypergraph a
rebuild = biFoldMap singleton (fromJust . permuteBack) (<>) (→)

-- TODO: stronger case; should be iso under any permutation of edge IDs
prop_iso :: OpenHypergraph Generator -> Bool
prop_iso g = g ~~ g

prop_decomposeSingletons :: Generator -> Bool
prop_decomposeSingletons gen =
  let g = singleton gen in g ~~ rebuild g

prop_decomposePairs :: Generator -> Bool
prop_decomposePairs gen =
  let g = singleton gen
      h = g → g
  in h ~~ rebuild h

prop_decomposeRoundtrip :: OpenHypergraph Generator -> Bool
prop_decomposeRoundtrip g =
  g ~~ rebuild g
