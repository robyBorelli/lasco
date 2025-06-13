module Encoder where

import Parser.AbsGrammar
import Parser.PrintGrammar(printTree)
import EncoderUtilities
import ExpEncoder (encodeExp)
import DisjunctiveEncoder (encodeDisjunctive)
import GHC.Generics (Generic)
import Solver

data EncoderType = Exponential | Disjunctive 
  deriving (Eq, Generic)

instance Show EncoderType where
  show Exponential = "'exponential'"
  show Disjunctive = "'disjunctive'"


class Enc a where
  encode :: a -> Program -> Program
instance Enc EncoderType where
  encode =  encoder 
    where 
      encoder enc = if enc == Exponential then encodeExp else encodeDisjunctive

printSolution:: (Maybe Solver) -> [Declaration] -> Program -> Program
printSolution (Just Clingo) hypos (Task decls) = (Task (decls ++  [Comment "show the hypothesis", 
                                                                  ShowNothing, 
                                                                  ShowTerm (termVariable "A")  [PositiveLiteral (CompositeAtom (BasicSymbol hypoHead) [termVariable "X"]),
                                                                                                 PositiveLiteral (CompositeAtom (BasicSymbol hypoHead) [termVariable "X",termVariable "A"]) ]
                                                                  ]
                                                                  ++[ AspRule ( NormalRule (SimpleHead (CompositeAtom (BasicSymbol hypoHead) [intTerm index, Constant (BasicSymbol ("\""++(printTree aspDecl)++"\""))] )) []) | (Hypothesis index weigth aspDecl) <- hypos] ) )
printSolution (Just _) hypos (Task decls) = (Task (decls ++  [Comment "show the hypothesis", (ShowAtom (BasicSymbol hypoHead) 2)]
                                                                  ++[ AspRule ( NormalRule (SimpleHead (CompositeAtom (BasicSymbol hypoHead) [intTerm index, Constant (BasicSymbol ("\""++(printTree aspDecl)++"\""))] )) [PositiveLiteral (CompositeAtom (BasicSymbol hypoHead) [intTerm index])]) | (Hypothesis index weigth aspDecl) <- hypos] ) )
printSolution _ _ (Task decls) = (Task (decls ++  [Comment "show the hypothesis", (ShowAtom (BasicSymbol hypoHead) 1)]) )

removeComments :: Bool -> Program -> Program
removeComments True (Task decls) = Task (filter (not . isComment) decls)
removeComments _ p = p

isComment :: Declaration -> Bool
isComment (Comment c) = True
isComment _ = False