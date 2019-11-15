{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Hypergraph.Test.Arbitrary where

import Control.Monad
import Data.Semigroup
import Data.Hypergraph as Hypergraph

import Debug.Trace

import Test.QuickCheck

instance (Signature sig, Arbitrary sig) => Arbitrary (OpenHypergraph sig) where
  arbitrary = sized generateSized

-- | Generate a hypergraph with a specific number of generators.
generateSized
  :: (Signature sig, Arbitrary sig) => Int -> Gen (OpenHypergraph sig)
generateSized 0 = return Hypergraph.empty -- give me the base case
generateSized 1 = Hypergraph.singleton <$> arbitrary -- I said the REAL base case.
-- In the general case, split n at a random point (1..n-1), then generate two
-- sub-hypergraphs of size (k < n, n - k).
generateSized n = do
  k <- choose (1,n-1)

  a <- generateSized k
  b <- generateSized (n - k)

  -- NOTE: this heavily favours composition because many monoidal products
  -- cause extremely slow matching.
  frequency [(19, return (a → b)), (1, return (a <> b))]

-------------------------------
-- A Generator type for use in testing.

-- | A test signature to try and capture many behaviours
data Generator = Generator
  { generatorFlag :: Int
  , generatorType :: (Int, Int)
  } deriving(Eq, Ord, Read, Show)

opposite :: Generator -> Generator
opposite (Generator flag (i,o)) = Generator flag (o, i)

instance Signature Generator where
  toSize = generatorType

-- | Generate small signatures, with 2 generators of each type from (
instance Arbitrary Generator where
  arbitrary = Generator <$> choose (0, 1) <*> randomType
    where
      maxPorts = 4
      randomType = do
        k <- choose (1, maxPorts) -- total number of ports
        n <- choose (0, k) -- of which n are inputs
        return (n, k - n)

-- Take integers (k, n) and an OpenHypergraph of size (k', n'), and return a
-- new hypergraph of size (k, n) by prepending and appending generators of size
-- (k, k') and (n', n), respectively.
-- NOTE: beware, this can result in "disconnected" hypergraphs; for example when
-- the supplied hypergraph is of type (0, 0)!
adaptSize :: Int -> Int -> OpenHypergraph Generator -> OpenHypergraph Generator
adaptSize k n g = a → g → b
  where
    (k', n') = toSize g
    a = singleton $ Generator 0 (k, k')
    b = singleton $ Generator 0 (n', n)
