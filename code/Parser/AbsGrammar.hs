{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser.AbsGrammar where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String
import Control.DeepSeq (deepseq, NFData)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)   -- da package `hashable`

data Program = Task [Declaration]
  deriving (C.Eq, C.Ord, C.Show, C.Read, Generic)

data Declaration
    = AspRule AspDeclaration
    | PositiveExample Example
    | NegativeExample Example
    | Hypothesis Integer Integer AspDeclaration
    | Comment String
    | ShowAtom BasicSymbol Integer
    | ShowTerm Term [Literal]
    | ShowNothing
    | Minimize [(Integer,Integer, Atom)]
    | WeakContraint [Atom] Integer Integer Integer
  deriving (C.Eq, C.Ord, C.Show, C.Read, Generic)

data AspDeclaration
    = NormalRule Head [Literal]
    | Fact Atom
    | Denial [Literal]
    | Range BasicSymbol Integer Integer
    | GroundChoiceRule     [Atom] [Literal]
    | GroundCardRuleLB     Atom Integer [Literal]
    | GroundCardRuleLBUB   Atom Integer [Literal] Integer
    | GroundCardConstraint Integer [Literal] Integer [Literal]
  deriving (C.Eq, C.Ord, C.Show, C.Read, Generic)

data Head = SimpleHead Atom | ChoiceHead Bound [ChoiceElem] Bound
          | DisjunctiveHead [Atom]
  deriving (C.Eq, C.Ord, C.Show, C.Read, Generic)

data Bound = ExplicitBound Integer | ImplicitBound
  deriving (C.Eq, C.Ord, C.Show, C.Read, Generic)

data ChoiceElem
    = GuardedChoiceElem Atom [Literal] | SimpleChoiceElem Atom
    | FakeChoiceElem Literal
  deriving (C.Eq, C.Ord, C.Show, C.Read, Generic)

data Atom
    = SimpleAtom BasicSymbol | CompositeAtom BasicSymbol [Term]
  deriving (C.Eq, C.Ord, C.Show, C.Read, Generic)
  deriving anyclass (Hashable)

data Term
    = Constant BasicSymbol
    | ArithmeticTerm ArithmeticExpr
    | StringTerm String
    | TupleTerm Term [Term]
    | FunctionalTerm Atom
  deriving (C.Eq, C.Ord, C.Show, C.Read, Generic)
  deriving anyclass (Hashable)

data Literal
    = PositiveLiteral Atom
    | NegativeLiteral Atom
    | ComparisonLiteral Term CompOp Term
  deriving (C.Eq, C.Ord, C.Show, C.Read, Generic)
  deriving anyclass (Hashable)

data CompOp
    = CompOpLe
    | CompOpLeq
    | CompOpGe
    | CompOpGeq
    | CompOpEq
    | CompOpEqq
    | CompOpNeq
  deriving (C.Eq, C.Ord, C.Show, C.Read, Generic)
  deriving anyclass (Hashable)

data ArithmeticExpr
    = AddExpr ArithmeticExpr ArithmeticExpr
    | SubExpr ArithmeticExpr ArithmeticExpr
    | MulExpr ArithmeticExpr ArithmeticExpr
    | DivExpr ArithmeticExpr ArithmeticExpr
    | NegExpr ArithmeticExpr
    | AbsExpr ArithmeticExpr
    | Variable VariableSymbol
    | IntExpr Integer 
  deriving (C.Eq, C.Ord, C.Show, C.Read, Generic)
  deriving anyclass (Hashable)

data Example
    = LasExample [Atom] [Atom]
    | LasCtxExample [Atom] [Atom] [AspDeclaration]
  deriving (C.Eq, C.Ord, C.Show, C.Read, Generic)

newtype VariableSymbol = VariableSymbol String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Generic)
  deriving newtype  (Data.String.IsString)
  deriving anyclass (Hashable) 

newtype BasicSymbol = BasicSymbol String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Generic)
  deriving newtype  (Data.String.IsString)
  deriving anyclass (Hashable)

instance NFData Program
instance NFData Declaration
instance NFData AspDeclaration
instance NFData Head
instance NFData Bound
instance NFData ChoiceElem
instance NFData Atom
instance NFData Term
instance NFData Literal
instance NFData CompOp
instance NFData ArithmeticExpr
instance NFData Example
instance NFData VariableSymbol
instance NFData BasicSymbol
