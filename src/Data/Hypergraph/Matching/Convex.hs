{-# LANGUAGE TupleSections #-}
module Data.Hypergraph.Matching.Convex
  ( isConvex
  , nonConvex
  ) where

import Control.Monad
import Data.List (unfoldr)
import Data.Maybe
import qualified Data.Bimap as Bimap

import Data.Hypergraph.Type as Hypergraph hiding (empty)
import Data.Hypergraph.Matching.Internal (Matching(..))
import Data.Hypergraph.Search (wires)

-- TODO: remove these imports!
import Data.Hypergraph.Algebraic ((→), permute)
--import Data.Hypergraph.Matching.Internal (matchNonConvex)

-- | Check if a particular 'Matching' is convex.
-- see: Definition 3.10 of https://arxiv.org/pdf/1602.06771.pdf
-- 
-- Algorithm sketch:
--  - Let RHS be the set of RHS boundary nodes
--  - If X is a set of nodes, let forward(X) be the set of hyperedges for which
--    X is on a left boundary of some e in forward(X).
--  - And let step(X) be the set of nodes on the right boundary for all edges
--    e \in forward(X) and e \notin matching.
--  - Let X = RHS.
--  - Loop
--    1. Let X = step(X)
--    2. If X is empty, HALT: convex
--    2. If any X in matching, HALT: non-convex
--    3. Continue

-- | The set of (context) nodes which were matched as the right-hand-side
-- boundary of the matching.
--
-- FIXME: this is a slow for large matchings, because we can't easily look up
-- matched nodes by whether they are on the boundary or not.
rhs :: Matching a -> [Wire Open]
rhs = fmap snd . filter (isBoundary . snd . fst) . Bimap.toList . _matchingWires

-- | The edges for which a node is on the *left* boundary
-- NOTE: in a monogamous hypergraph, this list will only ever be length 0 or 1.
forward :: Matching a -> Wire Open -> [HyperEdgeId]
forward _ (_, (Port Boundary _)) = [] -- TODO: will work, but technically wrong?
forward m (_, (Port (Gen e) _)) = case Bimap.lookupR e (_matchingEdges m) of
  Just _  -> []
  Nothing -> [e]

-- | The set of nodes obtained by following an unmatched edge one step.
step :: OpenHypergraph a -> Matching a -> Wire Open -> [Wire Open]
step g m = snd . wires g . Gen <=< forward m

-- | Return the first matching nodes reachable in a breadth-first search through
-- unmatched portions of the hypergraph
nonConvex :: OpenHypergraph a -> Matching a -> [Wire Open]
nonConvex g m = nonConvex' g m (rhs m)

nonConvex' :: OpenHypergraph a -> Matching a -> [Wire Open] -> [Wire Open]
nonConvex' g m [] = []
nonConvex' g m ws =
  let ws' = ws >>= step g m
  in  case filter inMatching ws' of
    [] -> nonConvex' g m ws'
    xs -> xs
  where
    -- check if this (context) wire is present within the Bimap
    inMatching :: Wire Open -> Bool
    inMatching w = isJust (Bimap.lookupR w $ _matchingWires m)

-- | Is a matching convex?
isConvex
  :: OpenHypergraph a -- ^ Context hypergraph
  -> Matching a
  -> Bool
isConvex g = Prelude.null . nonConvex g
