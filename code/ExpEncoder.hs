{-# LANGUAGE OverloadedStrings #-}

module ExpEncoder where
import Parser.AbsGrammar
import Data.List (intercalate)
import qualified Data.Set as Set
import qualified Data.Text as T
import Parser.PrintGrammar (printTree)
import EncoderUtilities

encodeExp :: Program -> Program
encodeExp (Task decls) = Task 
  ([Comment "background rules"] ++ declarationsBackground ++
    [Comment "hypothesis space"] ++ hypoChoice ++
    [Comment "hypothesis rules"] ++ declarationsHypothesis ++
    [Comment "enumerating all models"] ++ declarationsModels ++
    [Comment "reduction rules"] ++ declarationsAsCheck ++
    [Comment "examples"] ++ declarationsExamples ++
    [Comment "entailment rules"] ++ declarationsEntailment)
  where 
    declarationsBackground = [ encodeBgRule (AspRule (NormalRule h b)) | (AspRule (NormalRule h b)) <- decls ]
    hypoSpace = getHypoSpace decls 
    declarationsHypothesis = [ encodeHypoRule index (AspRule rule) | (Hypothesis index _ rule ) <- hypoSpace ]
    hypoIndexWeightList = getHypoIndexWeightList hypoSpace
    hypoChoice = [ (AspRule (NormalRule (ChoiceHead 
          (ExplicitBound 0) 
          [ (SimpleChoiceElem . atomH $ [intTerm index] ) | (index, _) <- hypoIndexWeightList ] 
          (ExplicitBound . toInteger $ (length hypoIndexWeightList))) [])) 
      ]
    examples = [ (PositiveExample e) | (PositiveExample e) <- decls] ++ [ (NegativeExample e) | (NegativeExample e) <- decls]
    declarationsExamples = foldr (++) [] [ encodeExample index ex | (index, ex) <- zip [1..] examples ]
    atoms = Set.toList . collectGroundAtoms $ decls
    declarationsModels = foldr (++) [] [ encodeModel index incl excl | (index, (incl, excl)) <- zip [1..] (powerset atoms) ]

-- answer set checking rules
declarationsAsCheck :: [Declaration]
declarationsAsCheck = [
  -- :- reduct(ninAs(X,A)),inMod(X,A),as(A)
  AspRule (Denial [PositiveLiteral . atomReduct $ [FunctionalTerm . atomNinAs $ [termVariable "X", termVariable "A"]],
          PositiveLiteral . atomInMod $ [termVariable "X", termVariable "A"],
          PositiveLiteral . atomAs $ [termVariable "A"]]),
  --  reduct(ninAs(X,A)) :- ninMod(X,A),as(A).
  AspRule (NormalRule (SimpleHead . atomReduct $ [FunctionalTerm . atomNinAs $ [termVariable "X", termVariable "A"]]) 
    [PositiveLiteral (atomNinMod [termVariable "X",termVariable "A"]),
    PositiveLiteral (atomAs [termVariable "A"])]),
  -- notAS(A) :- inAs(X,A), ninMod(X,A), as(A).
  AspRule (NormalRule (SimpleHead (atomNotAs [termVariable "A"])) 
    [PositiveLiteral (atomInAs [termVariable "X",termVariable "A"]),
    PositiveLiteral (atomNinMod [termVariable "X",termVariable "A"]),
    PositiveLiteral (atomAs [termVariable "A"])]),
  -- notAS(A) :- not inAs(X,A), inMod(X,A), as(A).
  AspRule (NormalRule (SimpleHead (atomNotAs [termVariable "A"])) 
    [NegativeLiteral (atomInAs [termVariable "X",termVariable "A"]),
    PositiveLiteral (atomInMod [termVariable "X",termVariable "A"]),
    PositiveLiteral (atomAs [termVariable "A"])]),
  -- isAS(A) :- not notAS(A), as(A).
  AspRule (NormalRule (SimpleHead (atomIsAs [termVariable "A"])) 
    [NegativeLiteral (atomNotAs [termVariable "A"]),
    PositiveLiteral (atomAs [termVariable "A"])])
  ]

-- entailment rules
declarationsEntailment :: [Declaration]
declarationsEntailment = [
  -- bad :- not cov(X), positive(X).
  AspRule (NormalRule (SimpleHead atomBad) 
    [NegativeLiteral (atomCov [termVariable "X"]),
    PositiveLiteral (atomPositive [termVariable "X"])]),
  -- bad :- cov(X), negative(X).
  AspRule (NormalRule (SimpleHead atomBad) 
    [PositiveLiteral (atomCov [termVariable "X"]),
    PositiveLiteral (atomNegative [termVariable "X"])]),
  -- :- bad.
  AspRule (Denial [PositiveLiteral atomBad])
  ]

encodeLiteral :: Literal -> Literal
encodeLiteral (PositiveLiteral a) = (PositiveLiteral (atomInAs [FunctionalTerm a, termVariable "A"]) )
encodeLiteral (NegativeLiteral a) = (PositiveLiteral (atomReduct [FunctionalTerm ( atomNinAs [FunctionalTerm a, termVariable "A"])] ))
encodeLiteral _ = error "Unsupported atom in body of a rule"

encodeHead :: Atom -> Atom
encodeHead a = atomInAs [FunctionalTerm a, termVariable "A"]

encodeBgRule :: Declaration -> Declaration
encodeBgRule (AspRule (NormalRule (SimpleHead h) body)) = 
  (AspRule (NormalRule (SimpleHead (encodeHead h)) ((map encodeLiteral body)++[PositiveLiteral . atomAs $ [termVariable  "A"]])))
encodeBgRule _ = error "Unsupported background rule"

encodeHypoRule :: Integer -> Declaration -> Declaration
encodeHypoRule index (AspRule (NormalRule (SimpleHead h) body)) = 
  (AspRule (NormalRule (SimpleHead (encodeHead h)) ((map encodeLiteral body)++[PositiveLiteral . atomAs  $ [termVariable "A"], PositiveLiteral . atomH $ [ intTerm  index]])))
encodeHypoRule _ _ = error "Unsupported background rule"

-- encoding examples
encodeExample :: Integer -> Declaration -> [Declaration]
encodeExample index (PositiveExample ex@(LasExample incls excls)) = 
  [(AspRule (Fact (atomPositive [intTerm index])))] ++ (encodeExampleCovering index ex)
encodeExample index (NegativeExample ex@(LasExample incls excls)) = 
  [(AspRule (Fact (atomNegative [intTerm index])))] ++ (encodeExampleCovering index ex)
encodeExample _ _ = error "Unsupported example type"

encodeExampleCovering :: Integer -> Example -> [Declaration]
encodeExampleCovering index (LasExample incls excls) = 
  [(AspRule (NormalRule (SimpleHead . atomCov $ [intTerm index]) 
                       ([ (PositiveLiteral . atomInMod $ [FunctionalTerm incl, termVariable "A"]) | incl <- incls ] ++ 
                       [ (NegativeLiteral . atomInMod $ [FunctionalTerm excl, termVariable "A"]) | excl <- excls ] ++ 
                       [ (PositiveLiteral . atomIsAs $ [termVariable "A"]) ])))]
encodeExampleCovering _ _ = error "Unsupported example type"

encodeModel ::  Integer -> [Atom] -> [Atom] -> [Declaration]
encodeModel index incl excl = [(AspRule (Fact . atomInMod $ [FunctionalTerm a, intTerm index])) | a <- incl] ++
                        [(AspRule (Fact . atomNinMod $ [FunctionalTerm a, intTerm index])) | a <- excl] ++
                        [(AspRule (Fact . atomAs $ [intTerm index]))]

powerset :: [a] -> [([a], [a])]
powerset []     = [([], [])]
powerset (x:xs) = 
    [ (x:incl, excl) | (incl, excl) <- ps ] ++
    [ (incl, x:excl) | (incl, excl) <- ps ]
  where
    ps = powerset xs

powerset' :: [a] -> [[a]]
powerset' []     = [[]]
powerset' (x:xs) = let ps = powerset' xs
                  in ps ++ map (x:) ps
