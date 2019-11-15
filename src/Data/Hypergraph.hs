module Data.Hypergraph
  ( module Data.Hypergraph.Type
  , module Data.Hypergraph.Algebraic
  , module Data.Hypergraph.Matching
  , module Data.Hypergraph.Rewriting
  , module Data.Hypergraph.Decompose
  , module Data.Hypergraph.Convex
  ) where

-- Hypergraph type and basic functions
import Data.Hypergraph.Type

-- Safe construction and combination of hypergraphs
import Data.Hypergraph.Algebraic

-- Unsafe construction and modification of hypergraphs.
-- import Data.Hypergraph.Unsafe

-- Hypergraph pattern matching
import Data.Hypergraph.Matching hiding (empty)

-- Rewriting
import Data.Hypergraph.Rewriting

-- Extra traversals
import Data.Hypergraph.Search

-- Decomposition
import Data.Hypergraph.Decompose

-- | Convexity checking
import Data.Hypergraph.Convex
