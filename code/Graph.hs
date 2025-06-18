module Graph where 
import Data.Graph
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Array as Array
import Data.List (subsequences, nub)
import Parser.AbsGrammar
import Debug.Trace

-- DATA DEFINITIONS
type AtomType = Atom
type Rule = Declaration

-- UTILITIES 
powersetNonEmpty :: [a] -> [[a]]
powersetNonEmpty = filter (not . null) . subsequences
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x
snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x
thd3 :: (a,b,c) -> c
thd3 (_,_,x) = x

buildGraph :: [Rule] -> (Graph, Vertex -> (AtomType, AtomType, [AtomType]), AtomType -> Maybe Vertex)
buildGraph rules =  graphFromEdges [ (h, h, b) | (h, b) <- grouped ]
  where grouped = groupRulesByHead rules
        groupRulesByHead :: [Rule] -> [(AtomType, [AtomType])]
        groupRulesByHead rules = Map.toList $ Map.fromListWith (++) [ (h, map (\(PositiveLiteral at)->at) (filter (\x -> case x of 
          (PositiveLiteral lit)-> True
          _ -> False) b)) | (AspRule (NormalRule (SimpleHead h) b)) <- rules ]

-- STRONG CONNECTIVITY ON INDUCED SUBGRAPH
isStronglyConnected :: [Vertex] -> Graph -> Bool
isStronglyConnected verts fullGraph = ret
  where vertSet = Set.fromList verts
        edges :: [(Vertex, Vertex)]
        edges = [ (v, w) | v <- verts, w <- fullGraph Array.! v, w `Set.member` vertSet ]
        inducedGraph = buildG (minimum verts, maximum verts) edges
        sccs = stronglyConnCompR [ ((), v, inducedGraph Array.! v) | v <- verts ]
        ret =  case sccs of
             [CyclicSCC vs] -> length vs == length verts
             [AcyclicSCC (_,v,_)] -> (v `Set.member` vertSet) && (v `elem` (inducedGraph Array.! v))  -- self-loop case
             _              -> False

-- MAIN LOOP FINDING LOGIC
findAllLoops :: (Graph , (Vertex -> (AtomType, AtomType, [AtomType])), (AtomType -> Maybe Vertex)) -> [[AtomType]]
findAllLoops (graph, nodeFromVertex, vertexFromKey) = ret
  where sccs = stronglyConnCompR [ ((), v, graph Array.! v) | v <- vertices graph ]
        nonTrivialSCCs = [ map (\(_, v, _) -> v) vs | CyclicSCC vs <- sccs ]
        ret = concat
           [ 
              [ loop
                | subset <- powersetNonEmpty (map (fst3 . nodeFromVertex) scc)
                , Just verts <- [traverse vertexFromKey subset]
                , isStronglyConnected verts graph
                , loop <- [ map (fst3 . nodeFromVertex) verts ]
                -- , traceShow ("loop Found") True
                ]
           | (idx,scc) <- zip [1..] nonTrivialSCCs
           -- , traceShow ("scc Found ", idx, length nonTrivialSCCs, map (fst3 . nodeFromVertex) scc) True
           ]


{--- EXAMPLE
exampleProgram1 :: [Rule]
exampleProgram1 =
  [ Rule "a" [] []
  , Rule "b" [] ["a"]
  , Rule "c" ["a"] ["d"]
  , Rule "d" [] ["c", "e"]
  , Rule "e" ["e"] []  -- self-loop
  , Rule "e" ["b"] ["f"]
  ]

exampleProgram2 :: [Rule]
exampleProgram2 =
  [ Rule "a" [] ["b"]
  , Rule "b" [] ["a"]
  , Rule "c" ["a","b"] []
  , Rule "c" ["d"] []
  , Rule "d" ["a"] []
  , Rule "d" ["b","c"] []
  , Rule "e" [] ["a","b"]
  ]


exampleProgram3 :: [Rule]
exampleProgram3 =
  [ Rule "a" [] ["b"]
  , Rule "b" [] ["a"]
  , Rule "c" ["a"] []
  , Rule "c" ["b","d"] []
  , Rule "d" ["b","c"] []
  , Rule "d" ["e"] []
  , Rule "e" ["b"] ["a"]
  , Rule "e" ["c","d"] []
  ]


loops1 = findAllLoops . buildGraph $ exampleProgram1
loops2 = findAllLoops . buildGraph $ exampleProgram2
loops3 = findAllLoops . buildGraph $ exampleProgram3
-}
