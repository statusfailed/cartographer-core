{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TupleSections          #-}
module Data.Hypergraph.Matching where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Logic.Class
import Control.Applicative hiding (empty)
import qualified Control.Applicative as A

import Data.Hypergraph.Type as Hypergraph hiding (empty)
import Data.Hypergraph.Search (undirectedDfs)

import Data.List (foldl')
import qualified Data.Map as Map
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

-- | Nondeterministically choose an item from the list.
-- performs about the same as "foldl' interleave mzero" on very large patterns,
-- but several orders of magnitude faster on very small patterns.
choice :: (Functor t, Foldable t, MonadPlus f) => t a -> f a
choice = msum . fmap pure

-- NOTE: doesnt this rule out e.g.,
-- pattern (-BOX-) <> (-BOX-)match in -BOX-BOX- ?
-- NO: because we'd get R0 <-> N1 and N1 <-> L0 ?
data Matching a = Matching
  { _matchingWires :: Bimap (Wire Open) (Wire Open)
  , _matchingEdges :: Bimap HyperEdgeId HyperEdgeId
  } deriving(Eq, Ord, Show)


instance NFData a => NFData (Matching a) where
  rnf (Matching w e) = rnf (Bimap.toList w, Bimap.toList e)

empty :: Matching a
empty = Matching Bimap.empty Bimap.empty

matchAll :: Eq a => OpenHypergraph a -> OpenHypergraph a -> [Matching a]
matchAll pattern context = observeAll $ match pattern context

-- NOTE: it seems about 500x faster in certain cases to just use list, and
-- avoid the 'choice' function. Not sure why this is,
match
  :: (Eq a, MonadLogic f)
  => OpenHypergraph a -> OpenHypergraph a -> f (Matching a)
match pattern context
  | Hypergraph.null pattern = pure empty -- empty pattern => empty matching
  | otherwise = matchDiscrete =<< foldM (matchWire pattern context) empty wires
  where
    wires = undirectedDfs pattern (Bimap.toList $ connections pattern)
    edges = Map.toList (signatures pattern)

    -- Match any remaining edges of type (0, 0)
    -- (i.e., those with no nodes attached, that are missed by matching wires)
    matchDiscrete m = foldM matchEdge m $
      filter (\(e,s) -> Bimap.notMember e (_matchingEdges m)) edges

    -- NOTE: this is quadratic and shouldn't be: don't want to iterate through
    -- all context edges for every single pattern node!
    matchEdge m (eid, sig) = do
      let f (ceid, csig) =
            (csig == sig) && Bimap.notMemberR ceid (_matchingEdges m)
      (c, _) <- choice $ filter f $ Map.toList (signatures context)
      return $ m {
        _matchingEdges = Bimap.insert eid c (_matchingEdges m)
      }

-- | Match a wire from the pattern with a wire in the context.
-- First proposes possible matches (candidates), then checks the match would be
-- consistent, then updates the matching.
matchWire
  :: (Eq a, MonadLogic f)
  => OpenHypergraph a
  -> OpenHypergraph a
  -> Matching a -> Wire Open -> f (Matching a)
matchWire pattern context m w = do
  cw <- candidates pattern context m w
  guard (consistent pattern context m w cw)
  pure (update pattern context m w cw)

-- Try to add a pairing of wires to the matching. New hyperedges may also be
-- added to the matching.
update
  :: OpenHypergraph a
  -> OpenHypergraph a
  -> Matching a
  -> Wire Open
  -> Wire Open
  -> Matching a
update pattern context m pw@(p,q) cw@(p',q') = addEdges . addWires $ m
  where
    addWires x = x { _matchingWires = Bimap.insert pw cw . _matchingWires $ x }
    addEdges = matchPorts q q' . matchPorts p p'

-- | Equate the HyperEdgeIds of two ports in the Matching.
matchPorts :: Port a Open -> Port a Open -> Matching sig -> Matching sig
matchPorts (Port Boundary _) _ m = m
matchPorts _ (Port Boundary _) m = m
matchPorts (Port (Gen a) _) (Port (Gen b) _) m =
  m { _matchingEdges = Bimap.insert a b (_matchingEdges m) }

-- | What are the possible matches for a given wire?
-- We try to exploit the "determined structure" of the hypergraph, by
-- checking if either of the source/target hyperedges is already matched.
candidates
  :: (Eq a, MonadLogic f)
  => OpenHypergraph a
  -> OpenHypergraph a
  -> Matching a -> Wire Open -> f (Wire Open)
candidates pattern context m w
  = case (determined pattern context m w) of
      Just w  -> pure w
      Nothing -> undetermined pattern context m w

-- | Return the context wire uniquely determined by a pattern wire.
-- If no context wire is so determined, return Nothing.
determined
  :: OpenHypergraph a
  -> OpenHypergraph a
  -> Matching a
  -> Wire Open
  -> Maybe (Wire Open)
determined pattern context m w@(s, t) =
  case (counterpart s, counterpart t) of
    (Nothing, Nothing) -> Nothing
    (Just s', Nothing) -> (s',) <$> target s' context
    (Nothing, Just t') -> (,t') <$> source t' context
    (Just s', Just t') -> Just (s', t')

  where
    -- Given a port in the pattern, find its counterpart in the context by
    -- simply looking up the hyperedge ID.
    counterpart :: Port a Open -> Maybe (Port a Open)
    counterpart (Port Boundary _) = Nothing
    counterpart (Port (Gen e) i)  = do
      e' <- Bimap.lookup e (_matchingEdges m)
      return (Port (Gen e') i)

-- If a wire is not uniquely determined by the current matching,
-- this function will search for an appropriate unmatched wire
-- TODO: use an index here. We can index by the "portsMatch" condition- see
-- wiki.
undetermined
  :: (Eq a, MonadLogic f)
  => OpenHypergraph a
  -> OpenHypergraph a
  -> Matching a
  -> Wire Open
  -> f (Wire Open)
undetermined pattern context m w = do
  choice $ filter condition (Bimap.toList $ connections context)
  where condition = consistent pattern context m w

-- | Given a proposed wire matching, check for consistency with the rest of the
-- matching.
consistent
  :: Eq a
  => OpenHypergraph a
  -> OpenHypergraph a
  -> Matching a
  -> Wire Open
  -> Wire Open
  -> Bool
consistent pattern context m a b
  =  portsMatch a b
  && unmatchedContextWire m b
  && edgeTypesMatch pattern context a b -- this is expensive so important to come last

-- | Returns true if a (context) wire is not in the Matching
unmatchedContextWire :: Matching a -> Wire Open -> Bool
unmatchedContextWire m contextWire =
  not $ Bimap.memberR contextWire (_matchingWires m)

portsMatch :: Wire Open -> Wire Open -> Bool
portsMatch (s, t) (s', t') = portMatch s s' && portMatch t t'

-- A boundary port in the pattern can match anything in the context;
-- a generator in the pattern can never match a boundary in the context;
-- two generators ports can only  match if their indices are equal.
portMatch :: Port a Open -> Port a Open -> Bool
portMatch (Port Boundary _) _   = True
portMatch _ (Port Boundary _)   = False
portMatch (Port _ i) (Port _ j) = i == j

edgeTypesMatch
  :: Eq a
  => OpenHypergraph a -> OpenHypergraph a -> Wire Open -> Wire Open -> Bool
edgeTypesMatch pattern context (p, q) (p', q')
  =  edgeTypeMatch pattern context p p'
  && edgeTypeMatch pattern context q q'

-- TODO: NOTE: this is a bit redundant with portMatch above!
edgeTypeMatch
  :: Eq a
  => OpenHypergraph a -> OpenHypergraph a -> Port x Open -> Port x Open -> Bool
edgeTypeMatch pattern context (Port p _) (Port p' _) = maybe False id $ do
  t1 <- edgeType pattern p
  t2 <- edgeType context p'
  return $ case (t1, t2) of
    (Boundary, _)   -> True
    (_, Boundary)   -> False
    (Gen a, Gen b)  -> a == b
