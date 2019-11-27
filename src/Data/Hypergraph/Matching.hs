module Data.Hypergraph.Matching
  ( match
  , matchAll
  , Matching(..)
  , empty
  ) where

import Control.Monad.Logic
import Control.Monad.Logic.Class

import Data.Hypergraph.Type (OpenHypergraph)

import Data.Hypergraph.Matching.Internal
import Data.Hypergraph.Matching.Convex

match :: (Eq a, MonadLogic f)
  => OpenHypergraph a
  -> OpenHypergraph a
  -> f (Matching a)
match pattern context = do
  matching <- matchNonConvex pattern context
  guard (isConvex context matching)
  return matching

matchAll :: Eq a => OpenHypergraph a -> OpenHypergraph a -> [Matching a]
matchAll pattern context = observeAll $ match pattern context
