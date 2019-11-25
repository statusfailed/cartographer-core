module Data.Hypergraph.Test
  ( module Data.Hypergraph
  , module Data.Hypergraph.Test.Arbitrary
  , module Type
  , mainTests
  , main
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Hypergraph
import Data.Hypergraph.Test.Arbitrary
import Data.Hypergraph.Test.Type as Type
import Data.Hypergraph.Test.Algebraic as Algebraic
import Data.Hypergraph.Test.Matching as Matching
import Data.Hypergraph.Test.Rewriting as Rewriting
import Data.Hypergraph.Test.Decompose as Decompose
import Data.Hypergraph.Test.Convex as Convex

import Data.Time.Clock
import Control.Monad
import Control.Monad.Logic
import Data.List (foldl')

mainTests :: TestTree
mainTests = testGroup "Data.Hypergraph"
  [ Type.tests
  , Algebraic.tests
  , Matching.tests
  , Rewriting.tests
  , Decompose.tests
  , Convex.tests
  ]

main :: IO ()
main = defaultMain mainTests
