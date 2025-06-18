{-# LANGUAGE OverloadedStrings #-}

module DisjunctiveEncoder where
import Parser.AbsGrammar
import Logic
import Data.List (intercalate)
import qualified Data.Set as Set
import qualified Data.Text as T
import Parser.PrintGrammar (printTree)
import Data.List (nub,isPrefixOf)
import Control.Monad.State
import EncoderUtilities
import Debug.Trace
import Graph
import qualified Data.HashSet        as HS
import qualified Data.Map.Strict     as M

encodeDisjunctive:: Program  -> Program
encodeDisjunctive p@(Task decls)  = (Task (dummy ++ hypothesis ++ declPos ++ declNeg))
 where
  positiveExamples = [ (PositiveExample p) | (PositiveExample p) <- decls ]
  negativeExamples = [ (NegativeExample p) | (NegativeExample p) <- decls ]
  dummy = if (positiveExamples ++ negativeExamples) == [] then error "The task does not contain any example" else []
  (Task declPos) = if positiveExamples /= [] then encodeDisjunctivePositive p else (Task [])
  (Task declNeg) = if negativeExamples /= [] then encodeDisjunctiveNegative p else (Task [])
  hypoSpace = getHypoSpace decls
  hypoChoice = [ (buildDisjunction [positiveVar . printTree . atomH $ [ intTerm  index], negativeVar . printTree . atomH $ [ intTerm  index] ] ) | (index, _) <- getHypoIndexWeightList hypoSpace]
  hypothesis = if negativeExamples == [] then ([Comment "hypothesis space"] ++ hypoChoice) else []

encodeDisjunctivePositive :: Program -> Program
encodeDisjunctivePositive (Task decls) = Task (
        [Comment "BRAVE ENTAILMENT ###########################"] ++
        [Comment "background rules"] ++ declarationsBackground ++
        [Comment "hypothesis rules"] ++ declarationsHypothesis ++
        [Comment "examples"] ++ declarationsExamples )
  where declarationsBackground = [ encodeBgRule (AspRule (NormalRule h b)) | (AspRule (NormalRule h b)) <- decls ]
        hypoSpace = getHypoSpace decls 
        declarationsHypothesis = [ encodeHypoRule index (AspRule rule) | (Hypothesis index _ rule ) <- hypoSpace ]
        examples = [ (PositiveExample e) | (PositiveExample e) <- decls]
        declarationsExamples = foldr (++) [] [ encodeExample index ex | (index, ex) <- zip [1..] examples ]
    
encodeLiteral :: Parser.AbsGrammar.Literal -> Parser.AbsGrammar.Literal
encodeLiteral (PositiveLiteral a) = (PositiveLiteral (atomInAs [FunctionalTerm a, termVariable "A"]) )
encodeLiteral (NegativeLiteral a) = (NegativeLiteral (atomInAs [FunctionalTerm a, termVariable "A"]) )
encodeLiteral _ = error "Unsupported atom in body of a rule"

encodeHead :: Atom -> Atom
encodeHead a = atomInAs [FunctionalTerm a, termVariable "A"]

encodeHypoRule :: Integer -> Declaration -> Declaration
encodeHypoRule index (AspRule (NormalRule (SimpleHead h) body)) = 
  (AspRule (NormalRule (SimpleHead (encodeHead h)) ((map encodeLiteral body)++[PositiveLiteral . atomAs  $ [termVariable "A"], PositiveLiteral . atomH $ [ intTerm  index]])))
encodeHypoRule _ _ = error "Unsupported background rule"

encodeBgRule :: Declaration -> Declaration
encodeBgRule (AspRule (NormalRule (SimpleHead h) body)) = 
  (AspRule (NormalRule (SimpleHead (encodeHead h)) ((map encodeLiteral body)++[PositiveLiteral . atomAs $ [termVariable  "A"]])))
encodeBgRule _ = error "Unsupported background rule"

encodeDisjunctiveNegative :: Program -> Program
encodeDisjunctiveNegative (Task decls) = prog'
  where 
    hypospace = getHypoSpace decls
    as = [(AspRule decl) | (AspRule decl) <- decls]
      ++ [(AspRule (NormalRule h (b ++ [(PositiveLiteral . atomH $ [ intTerm  index])]))) | (Hypothesis index _ (NormalRule h b)) <- hypospace ]
    (asForm, loops) = asFormula (Task as)
    negForm = negFormula (Task decls)
    finalForm = negationNormalForm (Implies asForm negForm)
    vars = collectVars finalForm
    (existentials, universals) = partition vars
    (Task prog) = eitterGottlob existentials universals finalForm
    prog' = Task (
      prog ++
      [Comment ("As formula: "++ (show asForm))] ++
      [Comment ("Loops found: "++ (show loops))] ++
      [Comment ("Neg formula: "++ (show negForm))] ++
      [Comment ("nnf: "++ (show finalForm))])
    partition :: [String] -> ([String], [String])
    partition [] = ([], [])
    partition (x:xs)
        | not (null x) && hypoHead `isPrefixOf` x = (x:ys, zs)
        | otherwise                               = (ys, x:zs)
        where
         (ys, zs) = partition xs

encodeExample :: Integer -> Declaration -> [Declaration]
encodeExample index (PositiveExample ex@(LasExample incls excls)) = 
  [(AspRule (Fact (atomAs [intTerm index])))] 
  ++ (encodeExampleCovering index ex)
  ++ [(AspRule (Denial [ NegativeLiteral (atomCov [intTerm index]) ] )) ]
encodeExample _ _ = error "Unsupported example type"

encodeExampleCovering :: Integer -> Example -> [Declaration]
encodeExampleCovering index (LasExample incls excls) = 
  [(AspRule (NormalRule (SimpleHead . atomCov $ [intTerm index]) 
                       ([ (PositiveLiteral . atomInAs $ [FunctionalTerm incl, intTerm index]) | incl <- incls ] ++ 
                       [ (NegativeLiteral . atomInAs $ [FunctionalTerm excl, intTerm index]) | excl <- excls ] 
                       )))]
encodeExampleCovering _ _ = error "Unsupported example type"

{-iffFormula :: Program -> Formula
iffFormula (Task decls) = makeConjunction ruleFormulas 
 where
  ruleFormulas = [ ruleFormula h (getBodies h) | h <- getAtoms ]
  getAtoms :: [Atom]
  getAtoms = filter (not . isHypoAtom) (nub ( foldr (++) [] [ nub ((h:[p | (PositiveLiteral p ) <- lits])++[n | (NegativeLiteral n ) <- lits]) | (AspRule (NormalRule (SimpleHead h) lits)) <- decls ] ))
  getBodies :: Atom -> [[Parser.AbsGrammar.Literal]]
  getBodies h = [ b | (AspRule (NormalRule (SimpleHead h1) b)) <- decls, h == h1 ]
  ruleFormula h bss = case rhss bss of
    FTrue -> (lhs h)
    FFalse -> (Not . lhs $ h)
    _ -> (Iff (lhs h) (rhss bss))

  lhs h = (Atomic . Atom . printTree $ h)
  rhss bss = makeDisjunction [ makeConjunction  (
    [ (Atomic . Atom . printTree $ a) | (PositiveLiteral a) <- bs]
    ++ [ (Atomic . NegAtom . printTree $ a) | (NegativeLiteral a) <- bs]) |  bs <- bss, bs /= [] ]
-}
iffFormula :: Program -> Formula
iffFormula (Task decls) = makeConjunction ruleFormulas 
 where
  ruleFormulas = [ ruleFormula h (getBodies h) | h <- getHeads ]
                 ++ [ (Not . lhs $ h) | h <- getFalseAtoms ]
  getHeads :: [Atom]
  getHeads = nub [ h | (AspRule (NormalRule (SimpleHead h) _)) <- decls ]
  getFalseAtoms :: [Atom]
  getFalseAtoms = filter (not . isHypoAtom) $ Set.toList (
    (collect AllAtoms Ground decls) 
    `Set.difference` 
    (Set.fromList getHeads) )
  getBodies :: Atom -> [[Parser.AbsGrammar.Literal]]
  getBodies h = [ b | (AspRule (NormalRule (SimpleHead h1) b)) <- decls, h == h1 ]
  ruleFormula h bs = if (rhss bs /= FTrue) 
    then (Iff (lhs h) (rhss bs))
    else (lhs h)
  lhs h = (Atomic . Atom . printTree $ h)
  rhss [] = FTrue
  rhss ([]:bs) = rhss bs
  rhss bs = makeDisjunction (map rhs bs)
  rhs b = makeConjunction  bodyList 
    where bodyList = [ (Atomic . Atom . printTree $ a) | (PositiveLiteral a) <- b]
                  ++ [ (Atomic . NegAtom . printTree $ a) | (NegativeLiteral a) <- b]


loopFormula :: Program -> (Formula, Int)
loopFormula (Task decls) = (ret, length loops)
  where graph = buildGraph decls
        loops = findAllLoops graph
        ret = case loops of
          [] -> FTrue
          _ -> makeConjunction [lf (filter (not . isHypoAtom) l) | l <- loops]
{-        getBodiesSlow :: [Atom] -> [[Parser.AbsGrammar.Literal]]
        getBodiesSlow hs = [ b | h <- hs, (AspRule (NormalRule (SimpleHead h1) b)) <- decls, h == h1, (atomIntersect hs b) == [] ]
        atomIntersect :: [Atom] -> [Parser.AbsGrammar.Literal] -> [Atom]
        atomIntersect heads lits = ret
          where posAtoms = [at | (PositiveLiteral at) <- lits]
                ret = Set.toList ((Set.fromList heads) `Set.intersection` (Set.fromList posAtoms))
-}
        compatible :: HS.HashSet Atom -> [Parser.AbsGrammar.Literal] -> Bool
        compatible hS = all ok
          where
            ok (PositiveLiteral a) = not (HS.member a hS)
            ok _                  = True 
        getBodies :: [Atom] -> [[Parser.AbsGrammar.Literal]]
        getBodies hs = concatMap bodiesOf hs
          where
            table :: M.Map Atom [[Parser.AbsGrammar.Literal]]
            table = M.fromListWith (++) [ (h1,[b]) | AspRule (NormalRule (SimpleHead h1) b) <- decls ]
            bodiesOf h = case M.lookup h table of
                           Nothing -> []
                           Just bs -> [ b | b <- bs, compatible (HS.fromList hs) b ]

        lf l = (Implies (lhs l) (rhs (getBodies l)))
        lhs hs = makeDisjunction [ (Atomic . Atom . printTree $ a) | a <- hs]
        rhs litss = makeDisjunction [
                  makeConjunction ( 
                     [ (Atomic . Atom . printTree $ a) | (PositiveLiteral a) <- lits]
                  ++ [ (Atomic . NegAtom . printTree $ a) | (NegativeLiteral a) <- lits] ) | lits <- litss]

asFormula :: Program -> (Formula, Int)
asFormula p = (ret,n)
  where iff = iffFormula p
        (loop, n) = (loopFormula p)
        ret = case loop of
          FTrue -> iff
          _ ->  (And iff loop)

negFormula :: Program -> Formula
negFormula (Task decls) = makeConjunction disjunctions 
  where
    disjunctions = [ encodeExample incls excls | (NegativeExample (LasExample incls excls)) <- decls ]
    encodeExample :: [Atom] -> [Atom] -> Formula
    encodeExample incls excls = makeDisjunction atoms
      where atoms = (map (\x -> (Atomic . NegAtom . printTree $ x)) incls) ++ (map (\x -> (Atomic . Atom . printTree $ x)) excls)
            
buildDisjunction :: [Var] -> Declaration
buildDisjunction vs = (AspRule (NormalRule (DisjunctiveHead [(SimpleAtom (BasicSymbol v)) | v <- vs]) []))

buildNormalRule :: Var -> [Var] -> [Var] -> Declaration
buildNormalRule h pos neg = (AspRule (NormalRule (SimpleHead (SimpleAtom (BasicSymbol h))) ([(PositiveLiteral (SimpleAtom (BasicSymbol b))) | b <- pos]++[(NegativeLiteral (SimpleAtom (BasicSymbol b))) | b <- neg]) ))

positiveVar :: Var -> Var
positiveVar = id

negativeVar :: Var -> Var
negativeVar v = (predicatePrefix++"neg_"++v)

clauseVar :: Int -> Var
clauseVar index = (predicatePrefix++"subformula_"++(show index))

sat :: Var
sat = predicatePrefix++"saturation"

type EncodingState = State Int
eitterGottlob existentials universals f = (Task (
      [Comment "CAUTIOUS ENTAILMENT"] ++
      [Comment "Each atom is either true or false"] ++ disHeads ++
      [Comment "Universal atoms are saturated"] ++ satUniversals ++
      [Comment "Saturation predicate"] ++ saturation ++ 
      [Comment "Formula encoding"] ++ formulaEncoding) )
  where
    disHeads = [ (buildDisjunction [positiveVar e, negativeVar e]) | e <- (existentials ++ universals) ]
    satUniversals = [ (buildNormalRule (positiveVar e) [sat] []) | e <- universals]
              ++ [ (buildNormalRule (negativeVar e) [sat] []) | e <- universals]
              -- ++ [ (buildNormalRule lasco_true [] []) ]
    saturation = [ (AspRule (NormalRule (SimpleHead (SimpleAtom (BasicSymbol sat))) [(NegativeLiteral (SimpleAtom (BasicSymbol sat)))] )) ]
    formulaEncoding = buildClausesRules f
    lasco_true = "lasco_lasco_true"
    lasco_false = "lasco_lasc_false"
    buildClausesRulesMon :: Formula -> EncodingState ([Declaration], Var)
    buildClausesRulesMon (Atomic (Atom var)) = return ([], positiveVar var)
    buildClausesRulesMon (Atomic (NegAtom var)) = return ([], negativeVar var)
    -- buildClausesRulesMon (FFalse) = return ([], lasco_false)
    -- buildClausesRulesMon (FTrue) = return ([], lasco_true)
    buildClausesRulesMon (Or f1 f2) = do
      newFormula <- newFormula
      (decls1, v1) <- buildClausesRulesMon f1
      (decls2, v2) <- buildClausesRulesMon f2
      let newDecls = [ buildNormalRule newFormula [v1] [], buildNormalRule newFormula [v2] []]
      return (newDecls++decls1++decls2, newFormula)
    buildClausesRulesMon (And f1 f2) = do
      newFormula <- newFormula
      (decls1, v1) <- buildClausesRulesMon f1
      (decls2, v2) <- buildClausesRulesMon f2
      let newDecls = [ buildNormalRule newFormula [v1,v2] []]
      return (newDecls++decls1++decls2, newFormula)

    newFormula :: EncodingState Var
    newFormula = do
      n <- get
      put (n + 1)
      let ret = if n == 0 then sat else clauseVar n
      return ret

    buildClausesRules :: Formula -> [Declaration]
    buildClausesRules f = decls
      where ((decls, v), val) = runState (buildClausesRulesMon f) 0
