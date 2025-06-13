module EncoderUtilities where

import Parser.AbsGrammar
import Parser.PrintGrammar(printTree)
import Data.List (nub)
import qualified Data.Set as Set

predicatePrefix="lasco_"
variablePrefix="LASCO_"
hypoHead=predicatePrefix++"hypo"
choiceRuleHead = predicatePrefix ++ "choice_rule"
choiceRuleStructureHead = predicatePrefix ++ "choice_rule_structure_"
choiceRuleGuardedHead= predicatePrefix ++ "choice_rule_guarded_head_"
choiceVar = predicatePrefix ++ "choice_var_"
simpleTermVariable str = ArithmeticTerm (Variable (VariableSymbol (str)))
termVariable str = simpleTermVariable (variablePrefix++str)
intTerm index = ArithmeticTerm (IntExpr index)
atomReduct ts = (CompositeAtom (BasicSymbol (predicatePrefix++"reduct")) ts)
atomInAs ts = (CompositeAtom (BasicSymbol (predicatePrefix++"inAs")) ts)
atomNinAs ts = (CompositeAtom (BasicSymbol (predicatePrefix++"ninAs")) ts)
atomIsAs ts = (CompositeAtom (BasicSymbol (predicatePrefix++"isAs")) ts)
atomInMod ts = (CompositeAtom (BasicSymbol (predicatePrefix++"inInterpretation")) ts)
atomNinMod ts = (CompositeAtom (BasicSymbol (predicatePrefix++"ninInterpretation")) ts)
atomNotAs ts = (CompositeAtom (BasicSymbol (predicatePrefix++"notAs")) ts)
atomAs ts = (CompositeAtom (BasicSymbol (predicatePrefix++"interpretation")) ts)
atomH ts = (CompositeAtom (BasicSymbol hypoHead) ts)
atomBad = (SimpleAtom (BasicSymbol (predicatePrefix++"bad")))
atomCov ts = (CompositeAtom (BasicSymbol (predicatePrefix++"cov")) ts)
atomPositive ts = (CompositeAtom (BasicSymbol (predicatePrefix++"positive")) ts)
atomNegative ts = (CompositeAtom (BasicSymbol (predicatePrefix++"negative")) ts)

getHypoSpace :: [Declaration] -> [Declaration]
getHypoSpace decls = [ (Hypothesis num w h) | (Hypothesis num w h) <- decls ]

getHypoSpaceP :: Program -> [Declaration]
getHypoSpaceP (Task decls) = getHypoSpace decls

isHypoAtom :: Atom -> Bool
isHypoAtom (CompositeAtom (BasicSymbol hypoHead) [ArithmeticTerm (IntExpr _)]) = True 
isHypoAtom _ = False

getBackground :: [Declaration] -> [Declaration]
getBackground decls = [ (AspRule aspDecl) | (AspRule aspDecl) <- decls ]

getBackgroundP :: Program -> [Declaration]
getBackgroundP (Task decls) = getBackground decls

getExamples :: [Declaration] -> [Declaration]
getExamples decls = [ (PositiveExample ex) | (PositiveExample ex) <- decls ]
                 ++ [ (NegativeExample ex) | (NegativeExample ex) <- decls ]

getExamplePositive :: Declaration -> [Atom]
getExamplePositive (PositiveExample (LasExample p n)) = p
getExamplePositive (NegativeExample (LasExample p n)) = p
getExamplePositive p = getExamplePositive . projectCtxExample $ p

getExampleNegative :: Declaration -> [Atom]
getExampleNegative (PositiveExample (LasExample p n)) = n
getExampleNegative (NegativeExample (LasExample p n)) = n
getExampleNegative p = getExampleNegative . projectCtxExample $ p

getExampleCtx :: Declaration -> [Declaration]
getExampleCtx (PositiveExample (LasCtxExample p n ctx)) = map (AspRule) ctx
getExampleCtx (NegativeExample (LasCtxExample p n ctx)) = map (AspRule) ctx

projectCtxExample :: Declaration -> Declaration
projectCtxExample (PositiveExample (LasCtxExample p n ctx)) = (PositiveExample (LasExample p n))
projectCtxExample (NegativeExample (LasCtxExample p n ctx)) = (NegativeExample (LasExample p n))
projectCtxExample a = a

setExamplePositive :: [Atom] -> Declaration -> Declaration
setExamplePositive p (PositiveExample (LasExample _ n)) = (PositiveExample (LasExample p n))
setExamplePositive p (NegativeExample (LasExample _ n)) = (NegativeExample (LasExample p n))

getExamplesP :: Program -> [Declaration]
getExamplesP (Task decls) = getExamples decls

getHypoIndexWeightList :: [Declaration] -> [(Integer, Integer)]
getHypoIndexWeightList hypoSpace = nub [ (i,w) | (Hypothesis i w _) <- hypoSpace ]

removeHypoSpace :: [Declaration] -> [Declaration]
removeHypoSpace decls = [ decl | decl <- decls, isHypo decl == False ]

isHypo :: Declaration -> Bool
isHypo (Hypothesis _ _ _) = True
isHypo _ = False

transformHypoSpace :: [Declaration] -> ([Declaration] -> [Declaration]) -> [Declaration]
transformHypoSpace decls f = (removeHypoSpace decls) ++ (f (getHypoSpace decls))
  
transformHypoSpaceP :: Program -> ([Declaration] -> [Declaration]) -> Program
transformHypoSpaceP (Task decls) f = (Task (transformHypoSpace decls f))

isGroundProgam :: Program -> Bool
isGroundProgam (Task decls) = if ((\x -> x == []) . Set.toList . collectVariables $ decls)
                              then True
                              else False 

data SymbolType = Ground | NonGround | AllSymbols
  deriving (Eq)

data AtomType = PositiveAtoms | NegativeAtoms | AllAtoms
  deriving (Eq)
  
collectGroundAtoms = collect AllAtoms Ground 
collectVariables = collect AllAtoms NonGround 

-- Extract atoms from declarations
collect ::AtomType -> SymbolType -> [Declaration] -> Set.Set Atom
collect atomType symbolType decls = Set.fromList [ a | d <- decls, a <- extractAtoms d ]
  where
    filterSymbols:: Atom -> Bool
    filterSymbols = case symbolType of
      (Ground) -> isGroundAtom
      (NonGround) -> (not . isGroundAtom )
      (AllSymbols) -> (\x -> True)
    extractAtoms :: Declaration -> [Atom]
    extractAtoms (AspRule (NormalRule h body)) = filter (filterSymbols) (headAtoms h ++ bodyAtoms body)
    extractAtoms (PositiveExample (LasExample pos neg)) = 
      if (symbolType == Ground && ((foldr (&&) True (map filterSymbols (pos++neg))) == False))
        then error "Examples require ground atoms"
        else pos ++ neg
    extractAtoms (NegativeExample (LasExample pos neg)) = 
      if (foldr (&&) True (map filterSymbols (pos++neg))) == True
        then pos ++ neg
        else error "Examples require ground atoms"
    extractAtoms (Hypothesis _ _ aspDecl) = extractAtoms (AspRule aspDecl)
    extractAtoms _ = []
    headAtoms (SimpleHead a) = filter (filterSymbols) [a]
    headAtoms _ = []
    bodyAtoms = concatMap litAtoms
    litAtoms (PositiveLiteral a) = if atomType /= NegativeAtoms then filter (filterSymbols) [a] else []
    litAtoms (NegativeLiteral a) = if atomType /= PositiveAtoms then filter (filterSymbols) [a] else []
    litAtoms _ =[] 
    --assumption. if a variable appears in a comparison literal,
    -- then it must also appear in a positive literal


 
-----------------------------------------
---- isGround functions
-----------------------------------------

isGroundLiteral :: Literal -> Bool
isGroundLiteral lt = (getVarsLiteral lt) == []

isGroundAtom :: Atom -> Bool
isGroundAtom at = (getVarsAtom at) == []

isGroundTerm :: Term -> Bool
isGroundTerm t = (getVarsTerm t) == []

isGroundExpr :: ArithmeticExpr -> Bool
isGroundExpr expr = (getVarsExpr expr) == []

getVarsLiteral :: Literal -> [String]
getVarsLiteral (PositiveLiteral at) = getVarsAtom at
getVarsLiteral (NegativeLiteral at) = getVarsAtom at 
getVarsLiteral (ComparisonLiteral t1 _ t2) = nub (( getVarsTerm t1 ) ++ ( getVarsTerm t2 ))

-----------------------------------------
---- getVars functions
-----------------------------------------

getVarsAtom :: Atom -> [String]
getVarsAtom (SimpleAtom symbol) = []
getVarsAtom (CompositeAtom symbol ts) = nub (foldr (++) [] (map getVarsTerm ts))

getVarsTerm :: Term -> [String]
getVarsTerm (Constant symbol) = []
getVarsTerm (ArithmeticTerm expr) = getVarsExpr expr
getVarsTerm (StringTerm str) = []
getVarsTerm (TupleTerm t1 ts) = nub (foldr (++) [] (map getVarsTerm (t1:ts)))
getVarsTerm (FunctionalTerm atom) = getVarsAtom atom

getVarsExpr :: ArithmeticExpr -> [String]
getVarsExpr (AddExpr e1 e2) = nub ((getVarsExpr e1) ++ (getVarsExpr e2))
getVarsExpr (SubExpr e1 e2) = nub ((getVarsExpr e1) ++ (getVarsExpr e2))
getVarsExpr (MulExpr e1 e2) = nub ((getVarsExpr e1) ++ (getVarsExpr e2))
getVarsExpr (DivExpr e1 e2) = nub ((getVarsExpr e1) ++ (getVarsExpr e2))
getVarsExpr (NegExpr e1) = getVarsExpr e1
getVarsExpr (AbsExpr e1) = getVarsExpr e1
getVarsExpr (Variable (VariableSymbol str)) = [str]
getVarsExpr (IntExpr int) = []

bindVarsLiteral :: Literal -> (ArithmeticExpr, Either Integer String) -> Literal
bindVarsLiteral (PositiveLiteral at) bind = (PositiveLiteral (bindVarsAtom at bind))
bindVarsLiteral (NegativeLiteral at) bind = (NegativeLiteral (bindVarsAtom at bind)) 
bindVarsLiteral (ComparisonLiteral t1 cmp t2) bind = (ComparisonLiteral (bindVarsTerm t1 bind) cmp (bindVarsTerm t2 bind))

bindVarsAtom :: Atom -> (ArithmeticExpr, Either Integer String) -> Atom
bindVarsAtom (SimpleAtom symbol) bind = (SimpleAtom symbol)
bindVarsAtom (CompositeAtom symbol ts) bind = (CompositeAtom symbol (map (\x -> bindVarsTerm x bind) ts))

bindVarsTerm :: Term -> (ArithmeticExpr, Either Integer String) -> Term
bindVarsTerm (Constant symbol) bind = (Constant symbol)
bindVarsTerm (ArithmeticTerm expr) (e1, Left i) | expr == e1 = (ArithmeticTerm (IntExpr i))
bindVarsTerm (ArithmeticTerm expr) (e1, Right str) | expr == e1 = (Constant (BasicSymbol str))
bindVarsTerm (ArithmeticTerm expr) bind = (ArithmeticTerm (bindVarsExpr expr bind))
bindVarsTerm (StringTerm str) bind = (StringTerm str)
bindVarsTerm (TupleTerm t1 ts) bind = (TupleTerm (bindVarsTerm t1 bind) (map (\x -> bindVarsTerm x bind) (ts)) )
bindVarsTerm (FunctionalTerm atom)  bind= (FunctionalTerm (bindVarsAtom atom bind))

bindVarsExpr :: ArithmeticExpr -> (ArithmeticExpr, Either Integer String) -> ArithmeticExpr
bindVarsExpr (AddExpr e1 e2) bind = (AddExpr (bindVarsExpr' e1 bind) (bindVarsExpr' e2 bind))
bindVarsExpr (SubExpr e1 e2) bind = (SubExpr (bindVarsExpr' e1 bind) (bindVarsExpr' e2 bind))
bindVarsExpr (MulExpr e1 e2) bind = (MulExpr (bindVarsExpr' e1 bind) (bindVarsExpr' e2 bind))
bindVarsExpr (DivExpr e1 e2) bind = (DivExpr (bindVarsExpr' e1 bind) (bindVarsExpr' e2 bind))
bindVarsExpr (NegExpr e1) bind = (NegExpr (bindVarsExpr' e1 bind))
bindVarsExpr (AbsExpr e1) bind = (AbsExpr (bindVarsExpr' e1 bind))
bindVarsExpr (IntExpr int) bind = (IntExpr int) 
bindVarsExpr _ _ = error "error during binding"

bindVarsExpr' e (e1, Left i) | e == e1 = (IntExpr i)
bindVarsExpr' e bind = bindVarsExpr e bind

-----------------------------------------
---- evalAritmetics functions
-----------------------------------------

evalAritmeticsLiteral :: Literal -> Literal 
evalAritmeticsLiteral (PositiveLiteral at) = (PositiveLiteral (evalAritmeticsAtom at))
evalAritmeticsLiteral (NegativeLiteral at) = (NegativeLiteral (evalAritmeticsAtom at)) 
evalAritmeticsLiteral (ComparisonLiteral t1 cmp t2) = (ComparisonLiteral (evalAritmeticsTerm t1) cmp (evalAritmeticsTerm t2))

evalAritmeticsAtom :: Atom -> Atom 
evalAritmeticsAtom (SimpleAtom symbol) = (SimpleAtom symbol)
evalAritmeticsAtom (CompositeAtom symbol ts) = (CompositeAtom symbol (map evalAritmeticsTerm ts))

evalAritmeticsTerm :: Term -> Term 
evalAritmeticsTerm (Constant symbol) = (Constant symbol)
evalAritmeticsTerm (ArithmeticTerm expr) = (ArithmeticTerm (evalAritmeticsExpr expr))
evalAritmeticsTerm (StringTerm str) = (StringTerm str)
evalAritmeticsTerm (TupleTerm t1 ts) = (TupleTerm (evalAritmeticsTerm t1) (map evalAritmeticsTerm ts) )
evalAritmeticsTerm (FunctionalTerm atom) = (FunctionalTerm (evalAritmeticsAtom atom))

evalAritmeticsExpr :: ArithmeticExpr -> ArithmeticExpr 
evalAritmeticsExpr (AddExpr e1 e2) = ret
  where e1' = (evalAritmeticsExpr e1)
        e2' = (evalAritmeticsExpr e2)
        ret = case (e1',e2') of
          (IntExpr a, IntExpr b) -> (IntExpr (a+b))
          other -> (AddExpr e1' e2')
evalAritmeticsExpr (SubExpr e1 e2) = ret
  where e1' = (evalAritmeticsExpr e1)
        e2' = (evalAritmeticsExpr e2)
        ret = case (e1',e2') of
          (IntExpr a, IntExpr b) -> (IntExpr (a-b))
          other -> (SubExpr e1' e2')
evalAritmeticsExpr (MulExpr e1 e2) = ret
  where e1' = (evalAritmeticsExpr e1)
        e2' = (evalAritmeticsExpr e2)
        ret = case (e1',e2') of
          (IntExpr a, IntExpr b) -> (IntExpr (a*b))
          other -> (MulExpr e1' e2')
evalAritmeticsExpr (DivExpr e1 e2) = ret
  where e1' = (evalAritmeticsExpr e1)
        e2' = (evalAritmeticsExpr e2)
        ret = case (e1',e2') of
          (IntExpr a, IntExpr b) -> (IntExpr (div a b))
          other -> (DivExpr e1' e2')
evalAritmeticsExpr (NegExpr e1) = ret
  where e1' = (evalAritmeticsExpr e1)
        ret = case e1' of
          (IntExpr a) -> (IntExpr (-a))
          other -> e1'
evalAritmeticsExpr (AbsExpr e1) = ret
  where e1' = (evalAritmeticsExpr e1)
        ret = case e1' of
          (IntExpr a) -> (IntExpr (abs a))
          other -> e1'
evalAritmeticsExpr (IntExpr int) = (IntExpr int)
evalAritmeticsExpr (Variable (VariableSymbol str)) = (Variable (VariableSymbol str))


-----------------------------------------
---- valueComparisonLiteral
-----------------------------------------

-- in any case we cannot fully evaluate the expression, we answer 'yes'
valueComparisonLiteral :: Literal -> Bool
valueComparisonLiteral (ComparisonLiteral (ArithmeticTerm (IntExpr e1)) CompOpLeq (ArithmeticTerm (IntExpr e2))) = e1 <= e2
valueComparisonLiteral (ComparisonLiteral (ArithmeticTerm (IntExpr e1)) CompOpGeq (ArithmeticTerm (IntExpr e2))) = e1 >= e2
valueComparisonLiteral (ComparisonLiteral (ArithmeticTerm (IntExpr e1)) CompOpLe  (ArithmeticTerm (IntExpr e2))) = e1 <  e2
valueComparisonLiteral (ComparisonLiteral (ArithmeticTerm (IntExpr e1)) CompOpGe  (ArithmeticTerm (IntExpr e2))) = e1 >  e2
valueComparisonLiteral (ComparisonLiteral e1 CompOpEq  e2) | isGroundTerm e1 && isGroundTerm e2 = e1 == e2
valueComparisonLiteral (ComparisonLiteral e1 CompOpEqq e2) | isGroundTerm e1 && isGroundTerm e2 = e1 == e2
valueComparisonLiteral (ComparisonLiteral e1 CompOpNeq e2) | isGroundTerm e1 && isGroundTerm e2 = e1 /= e2
valueComparisonLiteral _ = True

isComarisonLiteral :: Literal -> Bool
isComarisonLiteral (ComparisonLiteral _ _ _) = True
isComarisonLiteral _ = False