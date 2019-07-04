{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Hypergraph.Decompose where

import Data.List (sort, sortOn, groupBy, unfoldr)
import Data.Ord (Down(..))
import Data.Function (on)

import Data.Hypergraph.Type
import Data.Hypergraph.Search

import Control.Applicative
import Data.Reflection

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Term a
  = TGen a
  | TPermutation [Int] -- arbitrary permutations (including identity)
  deriving(Eq, Ord, Read, Show)

termIdentity :: Int -> Term a
termIdentity n = TPermutation [0..n-1]

-- | A decomposition is a hypergraph represented in the
-- "composition-of-tensors" normal form, e.g.,
--
-- @(a <> b <> c) → (d <> e <> f) → (g ...@
--
data Decomposition a = Decomposition { unDecomposition :: [[a]] }
  deriving(Eq, Ord, Read, Show)

-- | Decompose an acyclic 'OpenHypergraph'
-- NOTE: this is a /really bad/ algorithm complexity-wise.
-- It's probably at least as bad as /O(n^2 log n)/ worst-case.
-- TODO: what happens if graph has cycles?
decomposeAcyclic
  :: (Eq a, Signature a)
  => OpenHypergraph a
  -> Maybe [[Term (Open (HyperEdgeId, a))]]
decomposeAcyclic g = (start:) <$> rest
  where
    rest = fmap (>>= f) . sequence $ unfoldr (step g stop) s0
    f (p, t) = [pure (TPermutation p), t]

    (leftEdges, s0) = initialFrontier g
    start = fmap TGen leftEdges
    stop = (== Boundary)



-- compute the initial frontier: all zero-input hyperedges' direct descendants.
initialFrontier
  :: Signature a
  => OpenHypergraph a
  -> ([Open (HyperEdgeId, a)], [Port Target Open])
initialFrontier g = (edges', targets)
  where
    edges   = filter (isZeroInput . snd)  $ Map.toList (signatures g)
    edges'  = Boundary : fmap Gen edges
    targets = fmap snd . snd . wires g . fmap fst =<< edges'
    isZeroInput sig = fst (toSize sig) == 0

-- this function takes a set of target ports, and returns:
--  1) A permutation and a list of terms in the "vertical slice"
--  2) A list of subsequent target ports to carry to the next iteration
step
  :: (Ord (f HyperEdgeId), Eq a, Applicative f, Traversable f)
  => Hypergraph f a
  -> (f HyperEdgeId -> Bool) -- ugly hack to prevent boundaries causing cycles
  -> [Port Target f]
  -> Maybe (Maybe ([Int], [Term (f (HyperEdgeId, a))]), [Port Target f])
step _ _ [] = Nothing
step g stop frontier = Just ((permutation,) <$> outputs, nextState)
  where
    sorted      = sort $ zip frontier [0..]
    permutation = snd <$> sorted
    grouped     = groupBy ((==) `on` forgetIndex) . fmap fst $ sorted

    -- merged :: [Either (f HyperEdgeId) [Port Target f]]
    merged = fmap (merge g) grouped

    outputs = traverse (output g) merged
    nextState = merged >>= state g stop
  
forgetIndex :: Port a f -> f HyperEdgeId
forgetIndex (Port x _) = x

-- NOTE: this function has some strong assumptions, for example
-- every port in the input list must belong to the same generator,
-- and the input list is empty.
merge
  :: Ord (f HyperEdgeId)
  => Hypergraph f sig
  -> [Port Target f]
  -> Either (f HyperEdgeId) [Port Target f]
merge _ [] = error "Data.Hypergraph.Decompose.merge: empty list"
merge g ps = if hasAllPorts g ps then Left (f (head ps)) else Right ps
  where f (Port x _) = x

-- This is the dodgiest implementation. Also, sort shouldn't really be needed!
hasAllPorts
  :: forall f a sig. (Reifies a PortRole, Ord (f HyperEdgeId))
  => Hypergraph f sig
  -> [Port a f]
  -> Bool
hasAllPorts g ps
  =   not (hasPort g p')
  &&  (fmap toIndex ps == [0..maxIndex])
  where
    -- requires scoped type variables to annotate p' type!
    p' = Port x (maxIndex + 1) :: Port a f
    ((Port x maxIndex):qs) = sortOn Down ps
    toIndex (Port _ i) = i

hasPort
  :: (Reifies a PortRole, Ord (f HyperEdgeId))
  => Hypergraph f sig
  -> Port a f
  -> Bool
hasPort g p = maybe False (const True) (toWire p g)

state
  :: Ord (f HyperEdgeId)
  => Hypergraph f a 
  -> (f HyperEdgeId -> Bool)
  -> Either (f HyperEdgeId) [Port Target f]
  -> [Port Target f]
state g stop = either (\x -> if stop x then [] else targets x) id
  where
    -- direct descendants of a HyperEdge
    {-targets :: f HyperEdgeId -> [Port a f]-}
    targets = fmap snd . snd . wires g

-- failure happens when we can't look up a HyperEdgeId type.
output
  :: forall f a. (Applicative f, Traversable f)
  => Hypergraph f a 
  -> Either (f HyperEdgeId) [Port Target f]
  -> Maybe (Term (f (HyperEdgeId, a)))
output g = either toTerm toPerm
  where
    -- Maybe (f a)
    -- Maybe (Term (f 
    toTerm :: f HyperEdgeId -> Maybe (Term (f (HyperEdgeId, a)))
    toTerm e = fmap (TGen . liftA2 (,) e) (edgeType g e)

    -- Given a list of /n/ ports, convert them to an /n/-way identity
    -- permutation.
    toPerm :: [Port Target f] -> Maybe (Term t)
    toPerm x = Just . TPermutation $ [0..length x - 1]
