module Logic where

import Data.List (intercalate)
import Control.Monad.State
import Data.List (nub)

type Var = String
data Literal = Atom Var
             | NegAtom Var
  deriving (Eq)
data Clause = Clause [Literal]
data Cnf = Cnf [Clause]
data Formula = Atomic Literal 
             | Not Formula
             | Or Formula Formula 
             | And Formula Formula
             | Implies Formula Formula
             | Iff Formula Formula
  deriving (Eq) 

data PrintMode = Latex | Human
  deriving(Eq, Show)
data Operator = And_op | Or_op | Not_op | Implies_op | Iff_op
  deriving(Eq)

true = Or (Atomic . Atom $ "p") (Atomic . NegAtom $ "p")
false = And (Atomic . Atom $ "p") (Atomic . NegAtom $ "p")
makeConjunction l = foldl (And) (head l) (tail l)
makeDisjunction l = foldl (Or) (head l) (tail l)

makeDisjunction' [] = false
makeDisjunction' f = makeDisjunction f

makeConjunction' [] = false
makeConjunction' f = makeConjunction f

collectVars (Atomic (Atom v)) = [v]
collectVars (Atomic (NegAtom v)) = [v]
collectVars (Not f1) = collectVars f1
collectVars (Or f1 f2) = nub ((collectVars f1) ++ (collectVars f2)) 
collectVars (And f1 f2) = nub ((collectVars f1) ++ (collectVars f2))
collectVars (Implies f1 f2) = nub ((collectVars f1) ++ (collectVars f2))
collectVars (Iff f1 f2) = nub ((collectVars f1) ++ (collectVars f2)) 


collectCnfVars (Cnf clauses) = collectVars . makeConjunction $ (map (\(Clause lits) ->  makeDisjunction (map Atomic lits)) clauses)

printMode = Human
printSpace = 1
instance Show Operator where
  show Not_op = (replicate printSpace ' ') ++ if printMode == Latex then "\\lnot" else "¬" ++ (replicate printSpace ' ') 
  show And_op = (replicate printSpace ' ') ++ if printMode == Latex then "\\land" else "∧" ++ (replicate printSpace ' ') 
  show Or_op = (replicate printSpace ' ') ++ if printMode == Latex then "\\lor" else "∨" ++ (replicate printSpace ' ') 
  show Iff_op = (replicate printSpace ' ') ++ if printMode == Latex then "\\iff" else "↔" ++ (replicate printSpace ' ') 
  show Implies_op = (replicate printSpace ' ') ++ if printMode == Latex then "\\implies" else "→" ++ (replicate printSpace ' ') 

instance Show Literal where
  show (Atom c) = " " ++ c 
  show (NegAtom c) = (show Not_op) ++" " ++  c 
instance Show Clause where
  show (Clause fs) = "(" ++ intercalate (show Or_op) (map show fs) ++ ")"
instance Show Cnf where
  show (Cnf cs) = intercalate (show And_op) (map show cs)
instance Show Formula where
  show (Atomic f) = (show f)
  show (Not f) = show Not_op ++ ( show f )
  show (Or f1 f2) = "(" ++ ( show f1 ++ show Or_op ++ show f2 ) ++ ")"
  show (And f1 f2) = "(" ++ ( show f1 ++ show And_op ++ show f2 ) ++ ")"
  show (Implies f1 f2) = "(" ++ ( show f1 ++ show Implies_op ++ show f2 ) ++ ")"
  show (Iff f1 f2) = "(" ++ ( show f1 ++ show Iff_op ++ show f2 ) ++ ")"

type TseitinState = State Int

newVar :: TseitinState Var
newVar = do
    n <- get
    put (n + 1)
    let ret = if printMode == Human then ("x" ++ show n) else ("x_{"++show n++"}")
    return ret

tseitinMon :: Formula -> TseitinState (Cnf, Var)

tseitinMon (Atomic (Atom var)) = return (Cnf [], var)

tseitinMon (Atomic (NegAtom var)) = do
    newVar <- newVar
    let clause1 = Clause [NegAtom newVar, NegAtom var]
        clause2 = Clause [Atom var, Atom newVar]
    return (Cnf (clause1 : clause2 : []), newVar)

tseitinMon (Not f) = do
    newVar <- newVar
    (Cnf cnf, var) <- tseitinMon f
    let clause1 = Clause [NegAtom newVar, NegAtom var]
        clause2 = Clause [Atom var, Atom newVar]
    return (Cnf (clause1 : clause2 : cnf), newVar)

tseitinMon (Or f1 f2) = do
    newVar <- newVar
    (Cnf cnf1, var1) <- tseitinMon f1
    (Cnf cnf2, var2) <- tseitinMon f2
    let clause1 = Clause [Atom newVar, NegAtom var1]
        clause2 = Clause [Atom newVar, NegAtom var2]
        clause3 = Clause [Atom var1, Atom var2, NegAtom newVar]
    return (Cnf (clause1 : clause2 : clause3 : cnf1 ++ cnf2), newVar)

tseitinMon (And f1 f2) = do
    newVar <- newVar
    (Cnf cnf1, var1) <- tseitinMon f1
    (Cnf cnf2, var2) <- tseitinMon f2
    let clause1 = Clause [NegAtom newVar, Atom var1]
        clause2 = Clause [NegAtom newVar, Atom var2]
        clause3 = Clause [Atom newVar, NegAtom var1, NegAtom var2]
    return (Cnf (clause1 : clause2 : clause3 : cnf1 ++ cnf2), newVar)

tseitinMon (Implies f1 f2) = do
    newVar <- newVar
    (Cnf cnf1, var1) <- tseitinMon f1
    (Cnf cnf2, var2) <- tseitinMon f2
    let clause1 = Clause [Atom newVar, Atom var1]
        clause2 = Clause [Atom newVar, NegAtom var2]
        clause3 = Clause [NegAtom newVar, NegAtom var1, Atom var2]
    return (Cnf (clause1 : clause2 : clause3 : cnf1 ++ cnf2), newVar)

tseitinMon (Iff f1 f2) = do
    newVar <- newVar
    (Cnf cnf1, var1) <- tseitinMon f1
    (Cnf cnf2, var2) <- tseitinMon f2
    let clause1 = Clause [NegAtom newVar, Atom var1, NegAtom var2]
        clause2 = Clause [NegAtom newVar, NegAtom var1, Atom var2]
        clause3 = Clause [Atom newVar, NegAtom var1, NegAtom var2]
        clause4 = Clause [Atom newVar, NegAtom var1, NegAtom var2]
    return (Cnf (clause1 : clause2 : clause3 : clause4 : cnf1 ++ cnf2), newVar)

tseitin :: Formula -> Cnf
tseitin f = f'
  where ((Cnf cnf, auxVar), val) = runState (tseitinMon f) 1
        f' = Cnf ((Clause [Atom auxVar]):cnf)


f1 = (Implies (Or (Atomic (Atom "p")) (Atomic (Atom "q"))) (And (Atomic (Atom "p")) (Atomic (NegAtom "r"))))

p = (Atomic (Atom "p"))
np = (Atomic (NegAtom "p"))
q = (Atomic (Atom "q"))
nq = (Atomic (NegAtom "q"))
h1 = (Atomic (Atom "h(1)"))
nh1 = (Atomic (NegAtom "h(1)"))
h2 = (Atomic (Atom "h(2)"))
nh2 = (Atomic (NegAtom "h(2)"))

phi_as = (And (Iff p nq) (Iff (q) (Or (And np h2) (h1))))
phi_neg = (Or nq p)
phi = (Implies phi_as phi_neg)

negationNormalForm :: Formula -> Formula
negationNormalForm = nnf . eliminateArrows

eliminateArrows :: Formula -> Formula
eliminateArrows (Atomic l)         = Atomic l
eliminateArrows (Not f)            = Not (eliminateArrows f)
eliminateArrows (And f1 f2)        = And (eliminateArrows f1) (eliminateArrows f2)
eliminateArrows (Or f1 f2)         = Or (eliminateArrows f1) (eliminateArrows f2)
eliminateArrows (Implies f1 f2)    =
    Or (Not (eliminateArrows f1)) (eliminateArrows f2)
eliminateArrows (Iff f1 f2)        =
    let a = eliminateArrows f1
        b = eliminateArrows f2
    in And (Or (Not a) b) (Or (Not b) a)  -- (A → B) ∧ (B → A)

nnf :: Formula -> Formula
nnf (Atomic l)        = Atomic l
nnf (Not f) = case f of
  Atomic (Atom v)     -> Atomic (NegAtom v)
  Atomic (NegAtom v)  -> Atomic (Atom v)
  Not f'              -> nnf f'                            -- ¬¬A ≡ A
  And f1 f2           -> Or (nnf (Not f1)) (nnf (Not f2))  -- ¬(A ∧ B) ≡ ¬A ∨ ¬B
  Or f1 f2            -> And (nnf (Not f1)) (nnf (Not f2)) -- ¬(A ∨ B) ≡ ¬A ∧ ¬B
nnf (And f1 f2)       = And (nnf f1) (nnf f2)
nnf (Or f1 f2)        = Or (nnf f1) (nnf f2)