module Data.Hypergraph.Test.Matching where

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

tests = testGroup "Data.Hypergraph.Match"
  [ QC.testProperty "prop_matchSelf" prop_matchSelf
  , QC.testProperty "prop_match00" prop_match00
  , QC.testProperty "prop_matchTwice" prop_matchTwice
  , QC.testProperty "prop_matchSizeEqualsPatternSize"
      prop_matchSizeEqualsPatternSize
  , QC.testProperty "prop_emptyMatchesEverywhere" prop_emptyMatchesEverywhere
  , QC.testProperty "prop_identityMatchesNonEmpty" prop_identityMatchesNonEmpty
  , QC.testProperty "prop_matchSingleton" prop_matchSingleton
  , QC.testProperty "prop_matchCompose" prop_matchCompose
  , QC.testProperty "prop_matchTensor" prop_matchTensor
  {-, QC.testProperty "prop_matchAfter" prop_matchAfter-}
  ]

-------------------------------
-- Utils

-- Size in nodes + edges of a Matching
matchSize :: Matching a -> (Int, Int)
matchSize (Matching w e) = (Bimap.size w, Bimap.size e)

-- Size in nodes + edges of an OpenHypergraph
-- TODO: "size" very overloaded!
graphSize :: OpenHypergraph a -> (Int, Int)
graphSize g = (Bimap.size (connections g), Map.size (signatures g))

-------------------------------
-- Properties

-- | A pattern always matches in itself at least once.
-- It can be more - for example, (g <> g) will match at least twice in (g <> g)
-- because there are two choices for g. However, the matchings are effectively
-- the same.
prop_matchSelf :: OpenHypergraph Generator -> Property
prop_matchSelf a = 1 === (length . take 1 $ matchAll a a)

-- | Matching against (0, 0) generators should work!
prop_match00 :: Property
prop_match00 = let [m] = matchAll zz zz
  in  Bimap.size (_matchingEdges m) === 1
  where zz = singleton $ Generator 0 (0, 0)

-- | NOTE: this test length > 2 because of cases like the following:
--
--    a--R
--    b--R
--
-- where a : (0, 1) and b : (0, 1).
--
-- in this case, if we tensor the graph with itself we get
--
--    a--R
--    b--R
--    a--R
--    b--R
--
-- and there are two (valid) ways to assign the "a" type generators, and two
-- (valid) ways to assign the "b" type generators - giving us 4 matchings!
prop_matchTwice :: OpenHypergraph Generator -> Property
prop_matchTwice a
  =   (not . Data.Hypergraph.null $ a)
  ==> length (take 2 (matchAll a $ a → a)) === 2

prop_matchSizeEqualsPatternSize
  :: OpenHypergraph Generator -> OpenHypergraph Generator -> Property
prop_matchSizeEqualsPatternSize a b =
  let m = head $ matchAll a (a → b)
  in  graphSize a === matchSize m

-- | The empty hypergraph matches in every hypergraph.
prop_emptyMatchesEverywhere :: OpenHypergraph Generator -> Bool
prop_emptyMatchesEverywhere g = not . Prelude.null $ matchAll empty g

-- | The identity wire should match in any non-empty hypergraph exactly as
-- many times as that graph has connections.
prop_identityMatchesNonEmpty :: OpenHypergraph Generator -> Property
prop_identityMatchesNonEmpty g
  =   (not . Data.Hypergraph.null $ g)
  ==> Bimap.size (connections g) == length (matchAll identity g)

-- | A singleton graph appearing within a larger graph should always be
-- matchable.
prop_matchSingleton
  :: OpenHypergraph Generator
  -> Generator
  -> OpenHypergraph Generator -> Bool
prop_matchSingleton a g b = not . Prelude.null $ matchAll s (a → s → b)
  where s = singleton g

-- TODO: problem
--  this is fast:     match a (a → b)
--  this is SLOW:     match b (a → b)
prop_matchAfter
  :: OpenHypergraph Generator -> OpenHypergraph Generator -> Bool
prop_matchAfter a b =
  let msg = "composed size " ++ show (toSize a) ++ ", " ++ show (toSize b) ++
            ", ngens: " ++ show (Map.size (signatures a)) ++
            ", " ++ show (Map.size (signatures b))
      composed = trace msg (a → b)
  in  not . Prelude.null $ matchAll b composed

prop_matchCompose
  :: OpenHypergraph Generator
  -> OpenHypergraph Generator
  -> OpenHypergraph Generator
  -> Bool
prop_matchCompose a b c = not . Prelude.null $ matchAll b (a → b → c)

prop_matchTensor
  :: OpenHypergraph Generator
  -> OpenHypergraph Generator
  -> OpenHypergraph Generator
  -> Bool
prop_matchTensor a b c = not . Prelude.null $ matchAll b (a <> b <> c)
