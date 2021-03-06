{-
    Module containing utility functions for operating on Data.Graph.

    Retrieved from:
    http://cdsmith.wordpress.com/2009/04/20/code-for-manipulating-graphs-in-haskell/

    Per personal communication with the author Chris Smith, this code is
    part of the public domain.
-}
module GraphUtil where

import Data.Graph
import Data.List
import Data.Tree
import Data.Array
import Data.Maybe
import Data.Function (on)
import qualified Data.Map as M
import Data.Map (Map)

{-
    Computes all possible (directed) graphs over n vertices.  Allows
    loops and antiparallel edges, but does not allow parallel edges.
-}
allGraphs n = map (buildG (1,n))
                  (subsequences [ (a,b) | a <- [1..n], b <- [1..n] ])

{-
    Finds all cycles in a graph.  A cycle is given as a finite list of
    the vertices in order of occurrence, where each vertex only appears
    once.

    The point where the cycle is broken to form a linear list is arbitrary, so
    each cycle is merely one representative of the equivalence class
    generated by the relation identifying (v:vs) with vs++[v].  In
    particular, this function chooses the representative with the lowest
    numbered starting vertex.
-}
cycles g = concatMap cycles' (vertices g)
    where cycles' v   = build [] v v
          build p s v =
            let p'         = p ++ [v]
                local      = [ p' | x <- (g!v), x == s ]
                good w     = w > s && not (w `elem` p')
                ws         = filter good (g ! v)
                extensions = concatMap (build p' s) ws
            in  local ++ extensions

{-
    Computes a table of the number of loops at each vertex.
-}
loopdegree g = array (bounds g) [ (v, count v (g!v)) | v <- vertices g]
    where count v [] = 0
          count v (x:xs) | x == v    = 1 + count v xs
                         | otherwise = count v xs

{-
    Tests two graphs to see if they are isomorphic.  This is a worst-case
    exponential algorithm, but should perform acceptably in practice on
    small graphs.  It uses backtracking, associating vertices one by one
    until it either finds a complete isomorphism, or reaches a vertex
    that can't be matched with a remaining vertex in the other graph.
-}
isIsomorphic g1 g2 = let v1   = vertices g1
                         v2   = vertices g2
                     in  (length v1 == length v2) && test [] v1 v2

    where {-
            Takes the first vertex v of g1, looks for vertices w of g2
            that work "so far", and tries to construct an isomorphism
            that maps v to w.
          -}
          test m []     [] = True
          test m (v:vs) ws =
            let cs = filter (similar m v) ws
            in  any (\w -> test ((v,w):m) vs (delete w ws)) cs

          {-
            Tests whether a given mapping v -> w for v in g1, w in g2
            works "so far".  In order for the mapping to work, the
            vertices must agree on their in-degree, out-degree, and
            number of loops; and already mapped adjacent vertices via
            in- and out-edges must correspond.
          -}
          similar m v w    =    (in1   ! v) == (in2   ! w)
                             && (out1  ! v) == (out2  ! w)
                             && (loop1 ! v) == (loop2 ! w)
                             && match ((v,w):m) (g1 !v) (g2 !w)
                             && match ((v,w):m) (g1'!v) (g2'!w)

          {-
            Tests whether a list of vertices agrees in those edges that
            are already mapped to each other via the association list m.
          -}
          match m vs ws =
            let kvs = mapMaybe (\v -> find ((== v) . fst) m) vs
                kws = mapMaybe (\w -> find ((== w) . snd) m) ws
            in  sort kvs == sort kws

          {-
            Some global information about the graphs that can be
            calculated only once to save time.  This includes the
            degrees and number of loops at each vertex, and the
            transpose of the graphs (used to find in-edges).
          -}
          g1'   = transposeG g1
          g2'   = transposeG g2
          in1   = outdegree  g1'
          out1  = outdegree  g1
          in2   = outdegree  g2'
          out2  = outdegree  g2
          loop1 = loopdegree g1
          loop2 = loopdegree g2

{-
    Returns the degree sequence of a given graph.  The degree sequence is
    a sorted sequence of tuples representing the in-degree, out-degree,
    and loop-degree, respectively, of each vertex in the graph.  The
    degree sequence has the desirable properties that it is:

        (a) a graph property (that is, invariant under isomorphism)

        (b) relatively cheap to compute, and

        (c) classifies non-isomorphic graphs very effectively into small
            groups.
-}
degsequence g = sort (zip3 (elems (indegree g))
                           (elems (outdegree g))
                           (elems (loopdegree g)))

{-
    Given a list of graphs, removes the duplicates up to isomorphism.

    The implementation takes advantage of the fact that the degree
    sequence is the same for any pair of isomorphic graphs.  Therefore,
    the process maintains a map from degree sequences found so far to
    their respective graphs.  This removes the need to compare most sets
    of graphs in a normal list.  If the list contains only graphs with
    the same degree sequence, then this function will be very slow, as
    it will perform O(n^2) isomorphism tests, each of which are worst-case
    exponential.
-}
isonub :: [Graph] -> [Graph]
isonub gs = go M.empty gs
    where go hs []     = []
          go hs (g:gs) =
            let dseq = degsequence g
                poss = M.findWithDefault [] dseq hs
            in  if any (isIsomorphic g) poss
                    then     go hs                          gs
                    else g : go (M.insert dseq (g:poss) hs) gs

{-
    Finds the shortest paths from the given vertex to any other vertex
    of the graph.  The paths returned are lists of vertices, so if there
    are parallel edges in the graph, each result may actually correspond
    to multiple paths.

    The algorithm is essentially breadth first search, but done in bulk
    for each increase in depth, ensuring that all shortest paths are
    found for each vertex.
-}
shortestPaths g v = go (M.singleton v [[v]]) [v]
    where go ans []  = M.map (map reverse) ans
          go ans ws  = let new  = map (step ans) ws
                           next = foldl (M.unionWith (++)) ans new
                       in  go next (M.keys next \\ M.keys ans)
          step ans w = let new   = filter (not . (`M.member` ans)) (g!w)
                           soFar = ans M.! w
                           paths = map (\x -> map (x:) soFar) new
                       in  M.fromList (zip new paths)

{-
    Computes all possible directed subforests of a given graph.  A
    subforest is a subgraph with the property that there is at most one
    edge entering any vertex.

    The approach is to first choose the set of vertices, which may be
    any subset of the vertices of the given graph, and then choose the
    edge (if any) entering each vertex.  Parallel edges are ignored,
    since they would lead to isomorphic subforests.
-}
subforests :: Graph -> [Graph]
subforests g = filter ((== []) . cycles)
                $ concatMap subsAt
                $ subsequences (vertices g)
    where g'        = transposeG g
          subsAt vs = let vmap      = zip vs [1..]
                          tr v      = fromJust (lookup v vmap)
                          tre (v,w) = (tr v, tr w)
                          inAt v    = filter (`elem` vs) $ nub $ (g' ! v)
                      in  [ buildG (1, length vs) fixedEdges
                                | edges <- [ [] : [ [(w,v)] | w <- inAt v ] | v <- vs ],
                                  let fixedEdges = map tre (concat edges) ]
