{-# LANGUAGE FlexibleContexts #-}

module Normalizer (normalize) where

import Parser.AbsGrammar
import Control.Monad.State
import EncoderUtilities
import Parser.PrintGrammar
import Data.List (nub)

type NormalizeM = State Int
genNewHead :: String -> NormalizeM BasicSymbol
genNewHead s = do
    i <- get
    put (i + 1)
    return $ BasicSymbol (predicatePrefix ++ s ++ show i)

genNewIndex :: NormalizeM Int
genNewIndex = do
    i <- get
    put (i + 1)
    return $ i

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat (mapM f xs)

(>=>*) :: (a -> NormalizeM [a]) -> (a -> NormalizeM [a]) -> (a -> NormalizeM [a])
f >=>* g = \x -> do
  fx <- f x
  concatMapM g fx

isSimpleChoicheHead :: ChoiceElem -> Bool
isSimpleChoicheHead (SimpleChoiceElem a) = isGroundAtom a
isSimpleChoicheHead _ = False

convertChoiceNormalRules :: AspDeclaration -> NormalizeM [AspDeclaration]
convertChoiceNormalRules (NormalRule (ChoiceHead lb choices ub) body) | (foldr (&&) True (map isSimpleChoicheHead choices)) == False = 
  do idx <- genNewIndex
     let r1 = (NormalRule (SimpleHead (CompositeAtom (BasicSymbol choiceRuleHead) ([intTerm . toInteger $ idx]++ processVars vars))) (ltrue:body))
     let r2 = (NormalRule (SimpleHead (CompositeAtom (BasicSymbol choiceRuleStructureHead) ([intTerm . toInteger $ idx, intTerm realLb, intTerm realUb] ++ (map choiceToTerm simpleChoices)) )) (ltrue:body))
     return ([r1,r2]++(rs idx)++dummies)
 where 
  (simpleChoices, guardedChoices) = ([c | c@(SimpleChoiceElem _) <- choices], [ c | c@(GuardedChoiceElem _ _) <- choices])
  n = toInteger (length choices)
  realLb = case lb of 
    (ExplicitBound i) -> i
    (ImplicitBound) -> 0
  realUb = case ub of 
    (ExplicitBound i) -> i
    (ImplicitBound) -> n
  vars = nub (foldr (++) [] (map getVarsLiteral body))
  rs :: Int -> [AspDeclaration]
  rs idx= map (createGuardedRule idx) guardedChoices
  processVars :: [String] -> [Term]
  processVars [] = []
  processVars (x:xs) = (FunctionalTerm (SimpleAtom (BasicSymbol (predicatePrefix++x)))):(simpleTermVariable x):(processVars xs)
  choiceToTerm :: ChoiceElem -> Term
  choiceToTerm (SimpleChoiceElem at) = (FunctionalTerm at)
  choiceToTerm _ = error "Unsupported guarded choice element"
  createGuardedRule :: Int -> ChoiceElem -> AspDeclaration
  createGuardedRule idx (GuardedChoiceElem at lits) = (NormalRule (SimpleHead newHead) (ltrue:(body++lits)))
    where newHead = case at of
                      (SimpleAtom (BasicSymbol s)) -> (CompositeAtom (BasicSymbol (choiceRuleGuardedHead++s)) [intTerm . toInteger $ idx])
                      (CompositeAtom (BasicSymbol s) ts) -> (CompositeAtom (BasicSymbol (choiceRuleGuardedHead++s)) ((intTerm . toInteger $ idx):ts))
  createGuardedRule _ _ = error "Unsupported guarded choice element" 
  dummies = [createDummy c | c <- choices]
  createDummy (SimpleChoiceElem at) = (NormalRule (SimpleHead at) (ldummy:body))
  createDummy (GuardedChoiceElem at lits) = (NormalRule (SimpleHead at) (lits++(ldummy:body)))

convertChoiceNormalRules (NormalRule (ChoiceHead lb choices ub) body) = return [ret]
 where
  n = toInteger (length choices)
  realLb = case lb of 
    (ExplicitBound i) -> i
    (ImplicitBound) -> 0
  realUb = case ub of 
    (ExplicitBound i) -> i
    (ImplicitBound) -> n
  ret = case (realLb, realUb) of
    (0,val) | val == n -> (GroundChoiceRule (map (\(SimpleChoiceElem a) -> a ) choices) body)
    (_,_) -> (GroundCardConstraint realLb (map (\(SimpleChoiceElem a) -> (PositiveLiteral a) ) choices) realUb body)
convertChoiceNormalRules other = return [other]

removeGroundCardConstraint :: AspDeclaration -> NormalizeM [AspDeclaration]
removeGroundCardConstraint (GroundCardConstraint lb lits ub body) = do
    b <- genNewHead "card_b_"
    c <- genNewHead "card_c_"
    let rule0 = (NormalRule (SimpleHead . SimpleAtom $ b) body)
        rule1 = (GroundChoiceRule [a | (PositiveLiteral a) <- lits] [PositiveLiteral . SimpleAtom $ b])
        rule2 = (GroundCardRuleLBUB (SimpleAtom c) lb lits ub)
        rule3 = (Denial [PositiveLiteral . SimpleAtom $ b, NegativeLiteral . SimpleAtom $ c])
    return [rule0, rule1, rule2, rule3]
removeGroundCardConstraint other = return [other]

removeGroundCardRuleLBUB :: AspDeclaration -> NormalizeM [AspDeclaration]
removeGroundCardRuleLBUB (GroundCardRuleLBUB a lb lits ub) = do
    b <- genNewHead "lbub_b_"
    c <- genNewHead "lbub_c_"
    let rule0 = (NormalRule (SimpleHead a) [PositiveLiteral . SimpleAtom $ b, NegativeLiteral . SimpleAtom $ c])
        rule1 = (GroundCardRuleLB (SimpleAtom b) lb lits)
        rule2 = (GroundCardRuleLB (SimpleAtom c) (ub+1) lits)
    return [rule0, rule1, rule2]
removeGroundCardRuleLBUB other = return [other]

removeGroundCardRuleLB :: AspDeclaration -> NormalizeM [AspDeclaration]
removeGroundCardRuleLB (GroundCardRuleLB a lb lits) = do
    newPred <- genNewHead "ctr_"
    let rule0 = (NormalRule (SimpleHead a) [PositiveLiteral (ctrPredicate newPred 1 lb)])
        lastRule = (Fact (ctrPredicate newPred (toInteger ((length lits)+1)) 0))
        rules = [ [(NormalRule (SimpleHead (ctrPredicate newPred i (k+1))) [PositiveLiteral (ctrPredicate newPred (i+1) k),l]),
                   (NormalRule (SimpleHead (ctrPredicate newPred i k)) [PositiveLiteral (ctrPredicate newPred (i+1) k)])
                   ] 
                   | (l,i) <- zip lits [1..], k <- [0..lb] ]
    return ([rule0]++(foldr (++) [] rules)++[lastRule])
  where ctrPredicate :: BasicSymbol -> Integer -> Integer -> Atom
        ctrPredicate p i j = CompositeAtom p [intTerm i, intTerm j]
removeGroundCardRuleLB other = return [other]

removeGroundChoiceRule :: AspDeclaration -> NormalizeM [AspDeclaration]
removeGroundChoiceRule (GroundChoiceRule atoms literals) = do
    newPred <- genNewHead "choice_body_"
    let rule0 = NormalRule (SimpleHead . SimpleAtom $ newPred) literals
        rules = [ (NormalRule (SimpleHead . getOppositeAtom $ a) [NegativeLiteral a] )| a <- atoms ]
             ++ [ (NormalRule (SimpleHead a) [NegativeLiteral . getOppositeAtom $ a, PositiveLiteral . SimpleAtom $ newPred]) | a <- atoms ]
    return (rule0:rules)
  where getOppositeAtom :: Atom -> Atom
        getOppositeAtom (SimpleAtom (BasicSymbol str)) = (SimpleAtom (BasicSymbol (getInverseString str)))
        getOppositeAtom (CompositeAtom (BasicSymbol str) terms) = (CompositeAtom (BasicSymbol (getInverseString str)) terms) 
        getInverseString :: String -> String
        getInverseString str = predicatePrefix ++ "n_" ++ str
removeGroundChoiceRule other = return [other]

removeDenial :: AspDeclaration -> NormalizeM [AspDeclaration]
removeDenial (Denial body) = do
    newPred <- genNewHead "denial_"
    let newAtom = SimpleAtom newPred
        newHead = SimpleHead newAtom 
        newLiteral = NegativeLiteral newAtom
        newRule = NormalRule newHead (newLiteral:body)
    return [newRule]
removeDenial other = return [other]

removeRange :: AspDeclaration -> NormalizeM [AspDeclaration]
removeRange (Range bs l u) =  return [(Fact (CompositeAtom bs [intTerm i])) | i <- [l..u] ]
removeRange other = return [other]

removeFact :: AspDeclaration -> NormalizeM [AspDeclaration]
removeFact (Fact a) =  return [NormalRule (SimpleHead a) []]
removeFact other = return [other]

evalAritmetics :: AspDeclaration -> NormalizeM [AspDeclaration]
evalAritmetics (NormalRule (SimpleHead atom) lits) = return [(NormalRule (SimpleHead (evalAritmeticsAtom atom)) (map evalAritmeticsLiteral lits))]
evalAritmetics _ = error "error during normalization"

evalComparisons :: AspDeclaration -> NormalizeM [AspDeclaration]
evalComparisons (NormalRule atom lits) = retStatement
  where values = map valueComparisonLiteral lits
        totalValue = foldr (&&) True values
        retStatement = case totalValue of
            -- we mantain literals that are not comparison or that are not ground (cannot be fully evaluated until this point)
            True -> return [(NormalRule atom (filter (\x -> ((isComarisonLiteral x) == False) || (isGroundLiteral x == False)) lits))]
            False -> return []
evalComparisons _ = error "error during normalization"

normalizeAsp :: AspDeclaration -> NormalizeM [AspDeclaration]
normalizeAsp = 
         convertChoiceNormalRules
    >=>* removeGroundCardConstraint
    >=>* removeGroundCardRuleLBUB
    >=>* removeGroundCardRuleLB
    >=>* removeGroundChoiceRule 
    >=>* removeDenial 
    >=>* removeRange 
    >=>* removeFact 
    >=>* evalAritmetics
    >=>* evalComparisons

normalizeDecl :: Declaration -> NormalizeM [Declaration]
normalizeDecl (AspRule asp) = do
    asp' <- normalizeAsp asp
    return [(AspRule a) | a <- asp']
normalizeDecl (PositiveExample (LasCtxExample i j asps)) = do
    asps' <- mapM normalizeAsp asps
    return [PositiveExample (LasCtxExample i j (foldr (++) [] asps'))]
normalizeDecl (NegativeExample (LasCtxExample i j asps)) = do
    asps' <- mapM normalizeAsp asps
    return [NegativeExample (LasCtxExample i j (foldr (++) [] asps'))]
normalizeDecl (Hypothesis num i asp) = do
    asp' <- normalizeAsp asp
    return [(Hypothesis num i a) | a <- asp']
normalizeDecl a = return [a]

normalizeM :: Program -> NormalizeM Program
normalizeM (Task decls) = do
    decls' <- mapM normalizeDecl decls
    return (Task (foldr (++) [] decls'))

normalize :: Int -> Program -> (Program, Int)
normalize i p = (p'',i'')
  where
    (p', i') = normalizeExamples i p 
    (p'',i'') = normalizeBgHypo i' p'

normalizeBgHypo :: Int -> Program -> (Program, Int)
normalizeBgHypo i t = (runState (normalizeM $ t) i)

normalizeExamples :: Int -> Program -> (Program, Int)
normalizeExamples i (Task decls) = (Task (bg++hypos++examples), start)
  where 
    bg = (getBackground decls)
       ++ ( if ctxExamples /= [] then  [ AspRule (GroundCardConstraint 1 [ PositiveLiteral (ctxAtom i) | (i,_) <- indexedCtxExamples ] 1 []) ] else [] )
       ++ (foldr (++) [] [ map (append i) ds | (i, Task ds) <- zip indexes normCtx ])
    hypos = getHypoSpace decls
    examples = lasExamples ++ (map projectAndContextualize indexedCtxExamples)
    allExamples = getExamples decls
    (lasExamples, ctxExamples) = ( [ e | e@(PositiveExample (LasExample _ _)) <- allExamples] ++ [ e | e@(NegativeExample (LasExample _ _)) <- allExamples] ++ [ projectCtxExample e | e@(PositiveExample (LasCtxExample _ _ [])) <- allExamples] ++ [ projectCtxExample e | e@(NegativeExample (LasCtxExample _ _ [])) <- allExamples],
                                   [ e | e@(PositiveExample (LasCtxExample _ _ _)) <- allExamples, getExampleCtx e /= [] ] ++ [ e | e@(NegativeExample (LasCtxExample _ _ _)) <- allExamples, getExampleCtx e /= []])
    
    indexedCtxExamples::[(Int, Declaration)]
    indexedCtxExamples = zip [1..] ctxExamples
    projectAndContextualize :: (Int, Declaration) -> Declaration
    projectAndContextualize (i,ctxEx) = (setExamplePositive ((getExamplePositive ctxEx)++[(ctxAtom i)]) ) . projectCtxExample $ ctxEx
    ctxAtom :: Int -> Atom
    ctxAtom i = SimpleAtom ( BasicSymbol (predicatePrefix ++ "ctx_"++ show i) )
    indexes :: [Int]
    ctxs :: [Program]
    (indexes, ctxs) = unzip (map (\(i, ex) -> (i, Task (getExampleCtx ex))) indexedCtxExamples)
    normalizePrograms :: [Program] -> NormalizeM [Program]
    normalizePrograms = mapM normalizeM
    runNormalization :: [Program] -> Int -> ([Program], Int)
    runNormalization ps initial = runState (normalizePrograms ps) initial
    start :: Int
    normCtx :: [Program]
    (normCtx,start) = runNormalization ctxs (i + (length indexes) + 1)
    append :: Int -> Declaration -> Declaration
    append i (AspRule (NormalRule h ls)) = (AspRule (NormalRule h (ls ++ [PositiveLiteral . ctxAtom $ i])))

--------------------------------
------ TESTS -------------------
--------------------------------
-- GENERAL TEST
-- printTree (Task (map (AspRule) (fst $ (runState (normalizeAsp  (gc)) 0))))


-- TEST GroundCardConstraint --------------------------------
-- gc = (GroundCardConstraint 2 [PositiveLiteral . SimpleAtom . BasicSymbol $ "a1", NegativeLiteral . SimpleAtom . BasicSymbol $ "a2"] 4 [PositiveLiteral . SimpleAtom . BasicSymbol $ "a3", NegativeLiteral . SimpleAtom . BasicSymbol $ "a4"])
-- printTree (Task [AspRule gc])
---- 2 {  a1;  not a2 }4 :- a3, not a4 .
-- printTree (Task (map (AspRule) (fst $ (runState (removeGroundCardConstraint  (gc)) 0))))
---- lasco_card_b_0 :- a3, not a4 .
---- {  a1 }:- lasco_card_b_0 .
---- lasco_card_c_1 :- 2 {  a1, not a2 }4 .
---- :- lasco_card_b_0, not lasco_card_c_1 .

-- TEST GroundCardRuleLBUB --------------------------------
--cardlbub = (GroundCardRuleLBUB (SimpleAtom . BasicSymbol $ "a0") 2 [PositiveLiteral . SimpleAtom . BasicSymbol $ "a1", PositiveLiteral . SimpleAtom . BasicSymbol $ "a2", NegativeLiteral . SimpleAtom . BasicSymbol $ "a4", NegativeLiteral . SimpleAtom . BasicSymbol $ "a5"] 3)
-- printTree (Task [AspRule cardlbub])
---- a0 :- 2 {  a1, a2, not a4, not a5 }3 .
-- printTree (Task (map (AspRule) (fst $ (runState (removeGroundCardRuleLBUB  (cardlbub)) 0))))
---- a0 :- lasco_lbub_b_0, not lasco_lbub_c_1 .
---- lasco_lbub_b_0 :- 2 {  a1, a2, not a4, not a5 }.
---- lasco_lbub_c_1 :- 4 {  a1, a2, not a4, not a5 }.

-- TEST GroundCardRuleLB --------------------------------
-- cardlb = (GroundCardRuleLB (SimpleAtom . BasicSymbol $ "c") 1 [PositiveLiteral . SimpleAtom . BasicSymbol $ "a", NegativeLiteral . SimpleAtom . BasicSymbol $ "b"])
-- printTree (Task [AspRule cardlb])
---- c :- 1 {  a, not b }.
-- printTree (Task (map (AspRule) (fst $ (runState (removeGroundCardRuleLB  (cardlb)) 0))))
---- c :- lasco_ctr_0 (1, 1) .
---- lasco_ctr_0 (1, 1) :- lasco_ctr_0 (2, 0), a .
---- lasco_ctr_0 (1, 0) :- lasco_ctr_0 (2, 0) .
---- lasco_ctr_0 (1, 2) :- lasco_ctr_0 (2, 1), a .
---- lasco_ctr_0 (1, 1) :- lasco_ctr_0 (2, 1) .
---- lasco_ctr_0 (2, 1) :- lasco_ctr_0 (3, 0), not b .
---- lasco_ctr_0 (2, 0) :- lasco_ctr_0 (3, 0) .
---- lasco_ctr_0 (2, 2) :- lasco_ctr_0 (3, 1), not b .
---- lasco_ctr_0 (2, 1) :- lasco_ctr_0 (3, 1) .
---- lasco_ctr_0 (3, 0) .

-- TEST GroundChoiceRule --------------------------------
-- ch = (GroundChoiceRule [SimpleAtom . BasicSymbol $ "a1", SimpleAtom . BasicSymbol $ "a2"] [PositiveLiteral . SimpleAtom . BasicSymbol $ "a3", NegativeLiteral . SimpleAtom . BasicSymbol $ "a4"] )
-- printTree (Task [AspRule ch])
---- {  a1;  a2 }:- a3, not a4 .
-- printTree (Task (map (AspRule) (fst $ (runState (removeGroundChoiceRule  (ch)) 0))))
---- lasco_choice_body_0 :- a3, not a4 .
---- lasco_n_a1 :- not a1 .
---- lasco_n_a2 :- not a2 .
---- a1 :- not lasco_n_a1, lasco_choice_body_0 .
---- a2 :- not lasco_n_a2, lasco_choice_body_0 .

-- TEST Denial --------------------------------
-- den = (Denial [PositiveLiteral . SimpleAtom . BasicSymbol $ "a1", PositiveLiteral . SimpleAtom . BasicSymbol $ "a2", NegativeLiteral . SimpleAtom . BasicSymbol $ "a4", NegativeLiteral . SimpleAtom . BasicSymbol $ "a5"])
-- printTree (Task [AspRule den])
---- :- a1, a2, not a4, not a5 .
-- printTree (Task (map (AspRule) (fst $ (runState (removeDenial  (den)) 0))))
---- lasco_denial_0 :- not lasco_denial_0, a1, a2, not a4, not a5 .

-- TEST Range --------------------------------
-- range = (Range (BasicSymbol "p") 4 5)
-- printTree (Task [AspRule range])
---- p (4 .. 5) .
-- printTree (Task (map (AspRule) (fst $ (runState (removeRange  (range)) 0))))
---- p (4) . p (5) .

-- TEST Fact --------------------------------
-- fact = (Fact (SimpleAtom . BasicSymbol $ "p"))
-- printTree (Task [AspRule fact])
---- p
-- printTree (Task (map (AspRule) (fst $ (runState (removeFact  (fact)) 0))))
---- p