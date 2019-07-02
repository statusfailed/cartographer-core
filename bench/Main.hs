{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Main where

import Criterion.Main
import Control.Monad.Logic

import Data.Hypergraph
import Data.Time.Clock
import Data.List (foldl')
import Data.Maybe (fromJust)

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import GHC.Generics
import Control.DeepSeq

data Generator = Generator (Int, Int)
  deriving(Eq, Ord, Read, Show, Generic, NFData)

instance Signature Generator where
  toSize (Generator x) = x

-- force evaluation of both maps in the hypergraph.
graphSize :: OpenHypergraph sig -> (Int, Int)
graphSize g = (Bimap.size (connections g), Map.size (signatures g))

-- force evaluation of a matching by counting matched wires and edges
matchingSize :: Matching a -> (Int, Int)
matchingSize (Matching wires edges) = (Bimap.size wires, Bimap.size edges)

constructThin :: Int -> (Int, Int)
constructThin n = graphSize r
  where
    a = singleton (Generator (2,1))
    b = singleton (Generator (1,1))
    c = singleton (Generator (1,2))
    r = foldl' (→) empty (replicate n $ c → a)

constructThinAffine :: Int -> (Int, Int)
constructThinAffine n = graphSize r
  where
    a = singleton (Generator (2,1))
    r = foldl' (→) empty (replicate n $ a)

constructWide :: Int -> (Int, Int)
constructWide n = graphSize r
  where
    a = singleton (Generator (2,1))
    b = singleton (Generator (1,1))
    c = singleton (Generator (1,2))
    r = foldl' (<>) empty (replicate n $ c → a)

main = do
  defaultMain
    [ bgroup "construct"
      [ bench "constructThin" $ nf constructThin 10000
      , bench "constructWide" $ nf constructWide 10000
      , bench "constructThinAffine" $ nf constructWide 10000
      ]
    , bgroup "match"
      [ bgroup "rare"
        [ bgroup "compose"
          [ surroundBench "noPartials" size (→) (copy → add) bit
          , surroundBench
              "manyPartials" size (→) (add → copy) (add → bit → copy)
          ]
        , bgroup "tensor"
          [ surroundBench "noPartials" size (<>) (copy → add) bit
          , surroundBench
              "manyPartials" size (<>) (add → copy) (add → bit → copy)
          ]
        ]
      , bgroup "bigPattern"
        [ let pattern = foldl' (→) empty (replicate size bit)
          in  surroundBench "bit" size (→) (copy → add) pattern
        ]
      , bgroup "homogenous"
        [ homogenous "bit" (size*10) bit
        , homogenous "coherence" (size*10) coherence
        ]
      ]
    , bgroup "rewrite"
      [ bgroup "fullyReduce"
        [ let p = bit
              g = foldl' (→) empty (replicate size p)
          in  bench "bit→identity" $ nf (rewriteUntilDone p identity) g
        , let p = copy → add
              g = foldl' (→) empty (replicate size p)
          in  bench "copyadd→identity" $ nf (rewriteUntilDone p identity) g
        , let lhs = coherence
              rhs = add → copy
              g = cohere 5 coherence -- 64x64 square
          in  bench "cohere5" $ nf (rewriteUntilDone lhs rhs) g
        ]
      ]
    ]
  where
    size = 10000

-- Time to find the first match of a somewhat complicated pattern in a graph
-- where that pattern is extremely common.
homogenous :: String -> Int -> OpenHypergraph Generator -> Benchmark
homogenous name size pattern =
  let g = foldl' (→) empty (replicate size pattern)
  in  bench name $ nf (head . uncurry match) (pattern,g)

-- construct a graph using size*2 repetitions of 'filler' where 'pattern' only
-- occurs once (sandwiched between the fillers)
surround
  :: (OpenHypergraph a -> OpenHypergraph a -> OpenHypergraph a) -- ^ function to compose graphs
  -> Int -- ^ size of filler on each side
  -> OpenHypergraph a -- ^ filler graph
  -> OpenHypergraph a -- ^ pattern to occur once
  -> OpenHypergraph a
surround (·) size filler pattern = g · pattern · g
  where g = foldl' (·) empty (replicate size filler)

surroundBench
  :: (a ~ Generator)
  => String
  -> Int
  -> (OpenHypergraph a -> OpenHypergraph a -> OpenHypergraph a)
  -> OpenHypergraph a
  -> OpenHypergraph a
  -> Benchmark
surroundBench name size (·) filler pattern =
  let g = surround (<>) size filler pattern
  in  bench name $ nf (head . uncurry match) (pattern, g)

-- Try applying a rewrite rule until it doesn't match anymore
rewriteUntilDone
  :: Eq a
  => OpenHypergraph a -- ^ LHS
  -> OpenHypergraph a -- ^ RHS
  -> OpenHypergraph a -- ^ graph to rewrite
  -> OpenHypergraph a
rewriteUntilDone lhs rhs ctx = go ctx
  where
    go g = case (observeMany 1 $ match lhs g) of
      (m:_) -> go (rewrite m rhs g)
      _     -> g

-------------------------------
-- Various graphs for benching with

add = singleton (Generator (2,1))
bit = singleton (Generator (1,1))
copy = singleton (Generator (1,2))

coherence = (copy <> copy) → middle → (add <> add)
  where
    -- the "coherence" subgraph
    middle = fromJust $ permute [0,2,1,3]

-- cohere does "block-wise repetition" of a 2 → 2 graph, n times.
-- i.e., it puts 4 copies of the graph in a grid, and connects the lower
-- outputs of the upper-left graph to the upper inputs of the lower right
-- graph, and vice versa.
cohere :: Int -> OpenHypergraph Generator -> OpenHypergraph Generator
cohere n hg = go 1 hg n
  where
    go size g 0 = g
    go size g n =
      let p = identityN size <> swap size size <> identityN size
          g' = (g <> g) → p → (g <> g)
      in  go (size * 2) g' (n - 1)
