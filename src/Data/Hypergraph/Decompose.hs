{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Hypergraph.Decompose where

import Data.List (sort, sortOn, groupBy, unfoldr, foldl')
import Data.Ord (Down(..))
import Data.Function (on)
import Data.Maybe (fromJust)

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

-- e.g.
-- @id === biFoldMap singleton (fromJust . permute) (<>) (→)@
-- NOTE: also kind of unsafe, lots of calls to fromJust
biFoldMap
  :: (Eq sig, Signature sig, Monoid r)
  => (sig -> r)
  -> ([Int] -> r) -- ^ handle permutations of wires
  -> (r -> r -> r) -- ^ monoidal composition
  -> (r -> r -> r) -- ^ sequential composition
  -> OpenHypergraph sig
  -> r
biFoldMap f p m s =
  outer . fmap inner . removeBoundary . fromJust . decomposeAcyclic
  where
    inner = foldl' m mempty . fmap k -- terms into r, then fold up with <>
    outer = foldl' s mempty -- compose folded <> terms together using →
    k (TGen x) = f x
    k (TPermutation x) = p x

    unwrap (TGen Boundary) = []
    unwrap (TGen (Gen (_, x))) = [TGen x]
    unwrap (TPermutation xs) = [TPermutation xs]

    -- removeBoundary :: [[Term (Open (HyperEdgeId, sig))]]-> [[Term sig]]
    removeBoundary = filter (not . Prelude.null) . fmap (>>= unwrap)

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
    rest = fmap (>>= f) . sequence $ unfoldr (step g) s0
    f (p, t) = [pure (TPermutation p), t]

    (leftEdges, s0) = initialFrontier g

    -- unfortunate hack; because "Boundary" edges don't annotate their size,
    -- have to explicitly put in the right number of identity wires.
    start = fmap fixBoundary leftEdges
      where
        fixBoundary Boundary = TPermutation [0..i-1]
        fixBoundary x = TGen x
        (i, _) = toSize g


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
  :: OpenHypergraph a
  -> [Port Target Open]
  -> Maybe (Maybe ([Int], [Term (Open (HyperEdgeId, a))]), [Port Target Open])
step _ [] = Nothing
step g frontier = Just ((permutation,) <$> outputs, nextState)
  where
    sorted      = sort $ zip frontier [0..]
    permutation = snd <$> sorted
    grouped     = groupBy ((==) `on` forgetIndex) . fmap fst $ sorted

    -- another unfortunate hack- propagate target boundary until
    -- there is only the boundary left to place, or else we get a bug
    -- for graphs like this: --o o--
    -- merged :: [Either (f HyperEdgeId) [Port Target f]]
    merged = case fmap (merge g) grouped of
      [Left Boundary] -> [Left Boundary]
      xs -> zipWith unBoundary xs grouped

    unBoundary (Left Boundary) ports = Right ports
    unBoundary x _ = x

    outputs = traverse (output g) merged
    nextState = merged >>= state g stop

    stop = (== Boundary)

  
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
-- NOTE: have to treat boundaries as special - we don't want them to appear
-- until all other generators have been placed (I think?)
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
