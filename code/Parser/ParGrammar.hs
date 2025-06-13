{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Parser.ParGrammar
  ( happyError
  , myLexer
  , myShallowLexer
  , pProgram
  , parse
  ) where

import Prelude

import qualified Parser.AbsGrammar as AbsGrammar
import Parser.LexUtilities
import Parser.LexGrammar
import Parser.ShallowLexGrammar (shallowTokens)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (Integer)
	| HappyAbsSyn5 (String)
	| HappyAbsSyn6 (AbsGrammar.VariableSymbol)
	| HappyAbsSyn7 (AbsGrammar.BasicSymbol)
	| HappyAbsSyn8 (AbsGrammar.Program)
	| HappyAbsSyn9 (AbsGrammar.Declaration)
	| HappyAbsSyn10 (AbsGrammar.AspDeclaration)
	| HappyAbsSyn11 (AbsGrammar.Head)
	| HappyAbsSyn12 (AbsGrammar.Bound)
	| HappyAbsSyn13 (AbsGrammar.ChoiceElem)
	| HappyAbsSyn14 (AbsGrammar.Atom)
	| HappyAbsSyn15 (AbsGrammar.Term)
	| HappyAbsSyn16 (AbsGrammar.Literal)
	| HappyAbsSyn17 (AbsGrammar.CompOp)
	| HappyAbsSyn18 (AbsGrammar.ArithmeticExpr)
	| HappyAbsSyn23 (AbsGrammar.Example)
	| HappyAbsSyn24 ([AbsGrammar.Literal])
	| HappyAbsSyn25 ([AbsGrammar.Atom])
	| HappyAbsSyn26 ([AbsGrammar.ChoiceElem])
	| HappyAbsSyn27 ([AbsGrammar.AspDeclaration])
	| HappyAbsSyn28 ([AbsGrammar.Term])
	| HappyAbsSyn29 ([AbsGrammar.Declaration])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,374) ([0,49152,1024,1152,0,0,0,8,0,0,0,0,0,0,1024,0,0,1,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,1024,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,1,0,0,4096,0,0,0,8448,37888,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,512,16128,0,0,4096,0,0,0,640,0,0,0,260,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,2,121,0,256,36864,2,0,0,16384,0,0,1,640,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,4,0,528,31040,0,49152,1024,1152,0,4096,2,121,0,0,32772,4,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,256,0,0,0,2,0,0,0,0,0,0,0,0,0,0,16,0,0,0,2048,0,0,0,32,0,0,0,8192,0,0,0,16384,0,0,0,16,0,4096,2,41,0,0,0,0,0,0,0,0,0,16,0,0,40960,2,0,0,8448,36864,2,0,528,10496,0,0,33,656,0,4096,2,41,0,8448,37888,7,0,528,30976,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,528,30976,0,0,0,0,0,0,0,0,0,1024,1,0,0,4160,0,0,0,0,0,0,0,0,0,0,0,0,0,0,528,30976,0,0,42,0,0,0,0,0,0,4096,0,0,0,0,512,0,0,0,128,0,4096,16386,121,0,0,0,4,0,0,0,0,0,33,1936,0,0,0,8,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,1024,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,4,0,0,512,0,0,18,0,0,0,0,0,0,0,2048,0,0,16384,18432,0,0,64,0,0,0,0,2,0,512,0,0,0,16384,18432,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","Integer","String","VariableSymbol","BasicSymbol","Program","Declaration","AspDeclaration","Head","Bound","ChoiceElem","Atom","Term","Literal","CompOp","ArithmeticExpr","ArithmeticExpr1","ArithmeticExpr2","ArithmeticExpr3","ArithmeticExpr4","Example","ListLiteral","ListAtom","ListChoiceElem","ListAspDeclaration","ListTerm","ListDeclaration","'!='","'#neg'","'#pos'","'('","')'","'*'","'+'","','","'-'","'.'","'..'","'/'","':'","':-'","';'","'<'","'<='","'='","'=='","'>'","'>='","'not'","'{'","'|'","'}'","'~'","L_integ","L_quoted","L_VariableSymbol","L_BasicSymbol","%eof"]
        bit_start = st Prelude.* 60
        bit_end = (st Prelude.+ 1) Prelude.* 60
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..59]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (31) = happyShift action_12
action_0 (32) = happyShift action_13
action_0 (43) = happyShift action_14
action_0 (56) = happyShift action_2
action_0 (59) = happyShift action_15
action_0 (4) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 (11) = happyGoto action_8
action_0 (12) = happyGoto action_9
action_0 (14) = happyGoto action_10
action_0 (29) = happyGoto action_11
action_0 _ = happyReduce_18

action_1 (56) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (55) = happyShift action_42
action_3 _ = happyReduce_17

action_4 (33) = happyShift action_41
action_4 _ = happyReduce_21

action_5 (60) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (39) = happyShift action_40
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_6

action_8 (43) = happyShift action_39
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (52) = happyShift action_38
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (43) = happyReduce_15
action_10 _ = happyReduce_11

action_11 _ = happyReduce_5

action_12 (33) = happyShift action_36
action_12 (23) = happyGoto action_37
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (33) = happyShift action_36
action_13 (23) = happyGoto action_35
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (33) = happyShift action_29
action_14 (38) = happyShift action_30
action_14 (51) = happyShift action_31
action_14 (53) = happyShift action_32
action_14 (56) = happyShift action_2
action_14 (57) = happyShift action_33
action_14 (58) = happyShift action_34
action_14 (59) = happyShift action_15
action_14 (4) = happyGoto action_16
action_14 (5) = happyGoto action_17
action_14 (6) = happyGoto action_18
action_14 (7) = happyGoto action_19
action_14 (14) = happyGoto action_20
action_14 (15) = happyGoto action_21
action_14 (16) = happyGoto action_22
action_14 (18) = happyGoto action_23
action_14 (19) = happyGoto action_24
action_14 (20) = happyGoto action_25
action_14 (21) = happyGoto action_26
action_14 (22) = happyGoto action_27
action_14 (24) = happyGoto action_28
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_4

action_16 _ = happyReduce_48

action_17 _ = happyReduce_25

action_18 _ = happyReduce_47

action_19 (33) = happyShift action_75
action_19 (37) = happyReduce_21
action_19 (39) = happyReduce_21
action_19 (44) = happyReduce_21
action_19 (54) = happyReduce_21
action_19 _ = happyReduce_23

action_20 _ = happyReduce_27

action_21 (30) = happyShift action_68
action_21 (45) = happyShift action_69
action_21 (46) = happyShift action_70
action_21 (47) = happyShift action_71
action_21 (48) = happyShift action_72
action_21 (49) = happyShift action_73
action_21 (50) = happyShift action_74
action_21 (17) = happyGoto action_67
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (37) = happyShift action_66
action_22 _ = happyReduce_52

action_23 (36) = happyShift action_64
action_23 (38) = happyShift action_65
action_23 _ = happyReduce_24

action_24 (35) = happyShift action_62
action_24 (41) = happyShift action_63
action_24 _ = happyReduce_39

action_25 _ = happyReduce_42

action_26 _ = happyReduce_44

action_27 _ = happyReduce_46

action_28 _ = happyReduce_12

action_29 (33) = happyShift action_29
action_29 (38) = happyShift action_30
action_29 (53) = happyShift action_32
action_29 (56) = happyShift action_2
action_29 (57) = happyShift action_33
action_29 (58) = happyShift action_34
action_29 (59) = happyShift action_15
action_29 (4) = happyGoto action_16
action_29 (5) = happyGoto action_17
action_29 (6) = happyGoto action_18
action_29 (7) = happyGoto action_46
action_29 (15) = happyGoto action_60
action_29 (18) = happyGoto action_61
action_29 (19) = happyGoto action_24
action_29 (20) = happyGoto action_25
action_29 (21) = happyGoto action_26
action_29 (22) = happyGoto action_27
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (33) = happyShift action_57
action_30 (53) = happyShift action_32
action_30 (56) = happyShift action_2
action_30 (58) = happyShift action_34
action_30 (4) = happyGoto action_16
action_30 (6) = happyGoto action_18
action_30 (21) = happyGoto action_59
action_30 (22) = happyGoto action_27
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (59) = happyShift action_15
action_31 (7) = happyGoto action_51
action_31 (14) = happyGoto action_58
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (33) = happyShift action_57
action_32 (56) = happyShift action_2
action_32 (58) = happyShift action_34
action_32 (4) = happyGoto action_16
action_32 (6) = happyGoto action_18
action_32 (22) = happyGoto action_56
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_2

action_34 _ = happyReduce_3

action_35 _ = happyReduce_7

action_36 (52) = happyShift action_55
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_8

action_38 (59) = happyShift action_15
action_38 (7) = happyGoto action_51
action_38 (13) = happyGoto action_52
action_38 (14) = happyGoto action_53
action_38 (26) = happyGoto action_54
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (33) = happyShift action_29
action_39 (38) = happyShift action_30
action_39 (51) = happyShift action_31
action_39 (53) = happyShift action_32
action_39 (56) = happyShift action_2
action_39 (57) = happyShift action_33
action_39 (58) = happyShift action_34
action_39 (59) = happyShift action_15
action_39 (4) = happyGoto action_16
action_39 (5) = happyGoto action_17
action_39 (6) = happyGoto action_18
action_39 (7) = happyGoto action_19
action_39 (14) = happyGoto action_20
action_39 (15) = happyGoto action_21
action_39 (16) = happyGoto action_22
action_39 (18) = happyGoto action_23
action_39 (19) = happyGoto action_24
action_39 (20) = happyGoto action_25
action_39 (21) = happyGoto action_26
action_39 (22) = happyGoto action_27
action_39 (24) = happyGoto action_50
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (31) = happyShift action_12
action_40 (32) = happyShift action_13
action_40 (43) = happyShift action_14
action_40 (56) = happyShift action_2
action_40 (59) = happyShift action_15
action_40 (60) = happyReduce_63
action_40 (4) = happyGoto action_3
action_40 (7) = happyGoto action_4
action_40 (9) = happyGoto action_6
action_40 (10) = happyGoto action_7
action_40 (11) = happyGoto action_8
action_40 (12) = happyGoto action_9
action_40 (14) = happyGoto action_10
action_40 (29) = happyGoto action_49
action_40 _ = happyReduce_18

action_41 (33) = happyShift action_29
action_41 (38) = happyShift action_30
action_41 (53) = happyShift action_32
action_41 (56) = happyShift action_2
action_41 (57) = happyShift action_33
action_41 (58) = happyShift action_34
action_41 (59) = happyShift action_15
action_41 (4) = happyGoto action_45
action_41 (5) = happyGoto action_17
action_41 (6) = happyGoto action_18
action_41 (7) = happyGoto action_46
action_41 (15) = happyGoto action_47
action_41 (18) = happyGoto action_23
action_41 (19) = happyGoto action_24
action_41 (20) = happyGoto action_25
action_41 (21) = happyGoto action_26
action_41 (22) = happyGoto action_27
action_41 (28) = happyGoto action_48
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (43) = happyShift action_14
action_42 (56) = happyShift action_2
action_42 (59) = happyShift action_15
action_42 (4) = happyGoto action_43
action_42 (7) = happyGoto action_4
action_42 (10) = happyGoto action_44
action_42 (11) = happyGoto action_8
action_42 (12) = happyGoto action_9
action_42 (14) = happyGoto action_10
action_42 _ = happyReduce_18

action_43 _ = happyReduce_17

action_44 _ = happyReduce_9

action_45 (40) = happyShift action_93
action_45 _ = happyReduce_48

action_46 _ = happyReduce_23

action_47 (37) = happyShift action_92
action_47 _ = happyReduce_61

action_48 (34) = happyShift action_91
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_64

action_50 _ = happyReduce_10

action_51 (33) = happyShift action_75
action_51 _ = happyReduce_21

action_52 (44) = happyShift action_90
action_52 _ = happyReduce_57

action_53 (42) = happyShift action_89
action_53 _ = happyReduce_20

action_54 (54) = happyShift action_88
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (59) = happyShift action_15
action_55 (7) = happyGoto action_51
action_55 (14) = happyGoto action_86
action_55 (25) = happyGoto action_87
action_55 _ = happyReduce_54

action_56 (53) = happyShift action_85
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (33) = happyShift action_57
action_57 (38) = happyShift action_30
action_57 (53) = happyShift action_32
action_57 (56) = happyShift action_2
action_57 (58) = happyShift action_34
action_57 (4) = happyGoto action_16
action_57 (6) = happyGoto action_18
action_57 (18) = happyGoto action_84
action_57 (19) = happyGoto action_24
action_57 (20) = happyGoto action_25
action_57 (21) = happyGoto action_26
action_57 (22) = happyGoto action_27
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_28

action_59 _ = happyReduce_43

action_60 (37) = happyShift action_83
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (34) = happyShift action_82
action_61 (36) = happyShift action_64
action_61 (38) = happyShift action_65
action_61 _ = happyReduce_24

action_62 (33) = happyShift action_57
action_62 (38) = happyShift action_30
action_62 (53) = happyShift action_32
action_62 (56) = happyShift action_2
action_62 (58) = happyShift action_34
action_62 (4) = happyGoto action_16
action_62 (6) = happyGoto action_18
action_62 (20) = happyGoto action_81
action_62 (21) = happyGoto action_26
action_62 (22) = happyGoto action_27
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (33) = happyShift action_57
action_63 (38) = happyShift action_30
action_63 (53) = happyShift action_32
action_63 (56) = happyShift action_2
action_63 (58) = happyShift action_34
action_63 (4) = happyGoto action_16
action_63 (6) = happyGoto action_18
action_63 (20) = happyGoto action_80
action_63 (21) = happyGoto action_26
action_63 (22) = happyGoto action_27
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (33) = happyShift action_57
action_64 (38) = happyShift action_30
action_64 (53) = happyShift action_32
action_64 (56) = happyShift action_2
action_64 (58) = happyShift action_34
action_64 (4) = happyGoto action_16
action_64 (6) = happyGoto action_18
action_64 (19) = happyGoto action_79
action_64 (20) = happyGoto action_25
action_64 (21) = happyGoto action_26
action_64 (22) = happyGoto action_27
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (33) = happyShift action_57
action_65 (38) = happyShift action_30
action_65 (53) = happyShift action_32
action_65 (56) = happyShift action_2
action_65 (58) = happyShift action_34
action_65 (4) = happyGoto action_16
action_65 (6) = happyGoto action_18
action_65 (19) = happyGoto action_78
action_65 (20) = happyGoto action_25
action_65 (21) = happyGoto action_26
action_65 (22) = happyGoto action_27
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (33) = happyShift action_29
action_66 (38) = happyShift action_30
action_66 (51) = happyShift action_31
action_66 (53) = happyShift action_32
action_66 (56) = happyShift action_2
action_66 (57) = happyShift action_33
action_66 (58) = happyShift action_34
action_66 (59) = happyShift action_15
action_66 (4) = happyGoto action_16
action_66 (5) = happyGoto action_17
action_66 (6) = happyGoto action_18
action_66 (7) = happyGoto action_19
action_66 (14) = happyGoto action_20
action_66 (15) = happyGoto action_21
action_66 (16) = happyGoto action_22
action_66 (18) = happyGoto action_23
action_66 (19) = happyGoto action_24
action_66 (20) = happyGoto action_25
action_66 (21) = happyGoto action_26
action_66 (22) = happyGoto action_27
action_66 (24) = happyGoto action_77
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (33) = happyShift action_29
action_67 (38) = happyShift action_30
action_67 (53) = happyShift action_32
action_67 (56) = happyShift action_2
action_67 (57) = happyShift action_33
action_67 (58) = happyShift action_34
action_67 (59) = happyShift action_15
action_67 (4) = happyGoto action_16
action_67 (5) = happyGoto action_17
action_67 (6) = happyGoto action_18
action_67 (7) = happyGoto action_46
action_67 (15) = happyGoto action_76
action_67 (18) = happyGoto action_23
action_67 (19) = happyGoto action_24
action_67 (20) = happyGoto action_25
action_67 (21) = happyGoto action_26
action_67 (22) = happyGoto action_27
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_36

action_69 _ = happyReduce_30

action_70 _ = happyReduce_31

action_71 _ = happyReduce_34

action_72 _ = happyReduce_35

action_73 _ = happyReduce_32

action_74 _ = happyReduce_33

action_75 (33) = happyShift action_29
action_75 (38) = happyShift action_30
action_75 (53) = happyShift action_32
action_75 (56) = happyShift action_2
action_75 (57) = happyShift action_33
action_75 (58) = happyShift action_34
action_75 (59) = happyShift action_15
action_75 (4) = happyGoto action_16
action_75 (5) = happyGoto action_17
action_75 (6) = happyGoto action_18
action_75 (7) = happyGoto action_46
action_75 (15) = happyGoto action_47
action_75 (18) = happyGoto action_23
action_75 (19) = happyGoto action_24
action_75 (20) = happyGoto action_25
action_75 (21) = happyGoto action_26
action_75 (22) = happyGoto action_27
action_75 (28) = happyGoto action_48
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_29

action_77 _ = happyReduce_53

action_78 (35) = happyShift action_62
action_78 (41) = happyShift action_63
action_78 _ = happyReduce_38

action_79 (35) = happyShift action_62
action_79 (41) = happyShift action_63
action_79 _ = happyReduce_37

action_80 _ = happyReduce_41

action_81 _ = happyReduce_40

action_82 _ = happyReduce_49

action_83 (33) = happyShift action_29
action_83 (38) = happyShift action_30
action_83 (53) = happyShift action_32
action_83 (56) = happyShift action_2
action_83 (57) = happyShift action_33
action_83 (58) = happyShift action_34
action_83 (59) = happyShift action_15
action_83 (4) = happyGoto action_16
action_83 (5) = happyGoto action_17
action_83 (6) = happyGoto action_18
action_83 (7) = happyGoto action_46
action_83 (15) = happyGoto action_47
action_83 (18) = happyGoto action_23
action_83 (19) = happyGoto action_24
action_83 (20) = happyGoto action_25
action_83 (21) = happyGoto action_26
action_83 (22) = happyGoto action_27
action_83 (28) = happyGoto action_101
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (34) = happyShift action_82
action_84 (36) = happyShift action_64
action_84 (38) = happyShift action_65
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_45

action_86 (37) = happyShift action_100
action_86 _ = happyReduce_55

action_87 (54) = happyShift action_99
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (56) = happyShift action_2
action_88 (4) = happyGoto action_43
action_88 (12) = happyGoto action_98
action_88 _ = happyReduce_18

action_89 (33) = happyShift action_29
action_89 (38) = happyShift action_30
action_89 (51) = happyShift action_31
action_89 (53) = happyShift action_32
action_89 (56) = happyShift action_2
action_89 (57) = happyShift action_33
action_89 (58) = happyShift action_34
action_89 (59) = happyShift action_15
action_89 (4) = happyGoto action_16
action_89 (5) = happyGoto action_17
action_89 (6) = happyGoto action_18
action_89 (7) = happyGoto action_19
action_89 (14) = happyGoto action_20
action_89 (15) = happyGoto action_21
action_89 (16) = happyGoto action_22
action_89 (18) = happyGoto action_23
action_89 (19) = happyGoto action_24
action_89 (20) = happyGoto action_25
action_89 (21) = happyGoto action_26
action_89 (22) = happyGoto action_27
action_89 (24) = happyGoto action_97
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (59) = happyShift action_15
action_90 (7) = happyGoto action_51
action_90 (13) = happyGoto action_52
action_90 (14) = happyGoto action_53
action_90 (26) = happyGoto action_96
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_22

action_92 (33) = happyShift action_29
action_92 (38) = happyShift action_30
action_92 (53) = happyShift action_32
action_92 (56) = happyShift action_2
action_92 (57) = happyShift action_33
action_92 (58) = happyShift action_34
action_92 (59) = happyShift action_15
action_92 (4) = happyGoto action_16
action_92 (5) = happyGoto action_17
action_92 (6) = happyGoto action_18
action_92 (7) = happyGoto action_46
action_92 (15) = happyGoto action_47
action_92 (18) = happyGoto action_23
action_92 (19) = happyGoto action_24
action_92 (20) = happyGoto action_25
action_92 (21) = happyGoto action_26
action_92 (22) = happyGoto action_27
action_92 (28) = happyGoto action_95
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (56) = happyShift action_2
action_93 (4) = happyGoto action_94
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (34) = happyShift action_105
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_62

action_96 _ = happyReduce_58

action_97 _ = happyReduce_19

action_98 (43) = happyReduce_16
action_98 _ = happyReduce_14

action_99 (37) = happyShift action_104
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (59) = happyShift action_15
action_100 (7) = happyGoto action_51
action_100 (14) = happyGoto action_86
action_100 (25) = happyGoto action_103
action_100 _ = happyReduce_54

action_101 (34) = happyShift action_102
action_101 _ = happyFail (happyExpListPerState 101)

action_102 _ = happyReduce_26

action_103 _ = happyReduce_56

action_104 (52) = happyShift action_106
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_13

action_106 (59) = happyShift action_15
action_106 (7) = happyGoto action_51
action_106 (14) = happyGoto action_86
action_106 (25) = happyGoto action_107
action_106 _ = happyReduce_54

action_107 (54) = happyShift action_108
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (34) = happyShift action_109
action_108 (37) = happyShift action_110
action_108 _ = happyFail (happyExpListPerState 108)

action_109 _ = happyReduce_50

action_110 (52) = happyShift action_111
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (43) = happyShift action_14
action_111 (54) = happyReduce_59
action_111 (56) = happyShift action_2
action_111 (59) = happyShift action_15
action_111 (4) = happyGoto action_43
action_111 (7) = happyGoto action_4
action_111 (10) = happyGoto action_112
action_111 (11) = happyGoto action_8
action_111 (12) = happyGoto action_9
action_111 (14) = happyGoto action_10
action_111 (27) = happyGoto action_113
action_111 _ = happyReduce_18

action_112 (39) = happyShift action_115
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (54) = happyShift action_114
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (34) = happyShift action_117
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (43) = happyShift action_14
action_115 (54) = happyReduce_59
action_115 (56) = happyShift action_2
action_115 (59) = happyShift action_15
action_115 (4) = happyGoto action_43
action_115 (7) = happyGoto action_4
action_115 (10) = happyGoto action_112
action_115 (11) = happyGoto action_8
action_115 (12) = happyGoto action_9
action_115 (14) = happyGoto action_10
action_115 (27) = happyGoto action_116
action_115 _ = happyReduce_18

action_116 _ = happyReduce_60

action_117 _ = happyReduce_51

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn4
		 ((read happy_var_1) :: Integer
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (T_VariableSymbol happy_var_1)))
	 =  HappyAbsSyn6
		 (AbsGrammar.VariableSymbol happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal (PT _ (T_BasicSymbol happy_var_1)))
	 =  HappyAbsSyn7
		 (AbsGrammar.BasicSymbol happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn8
		 (AbsGrammar.Task happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (AbsGrammar.AspRule happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  9 happyReduction_7
happyReduction_7 (HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (AbsGrammar.PositiveExample happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  9 happyReduction_8
happyReduction_8 (HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (AbsGrammar.NegativeExample happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  9 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn9
		 (AbsGrammar.Hypothesis 0 happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  10 happyReduction_10
happyReduction_10 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (AbsGrammar.NormalRule happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn10
		 (AbsGrammar.Fact happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  10 happyReduction_12
happyReduction_12 (HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (AbsGrammar.Denial happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 6 10 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (AbsGrammar.Range happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 5 10 happyReduction_14
happyReduction_14 ((HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (AbsGrammar.NormalRule (AbsGrammar.ChoiceHead happy_var_1 happy_var_3 happy_var_5) []
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  11 happyReduction_15
happyReduction_15 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn11
		 (AbsGrammar.SimpleHead happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happyReduce 5 11 happyReduction_16
happyReduction_16 ((HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (AbsGrammar.ChoiceHead happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_1  12 happyReduction_17
happyReduction_17 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn12
		 (AbsGrammar.ExplicitBound happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  12 happyReduction_18
happyReduction_18  =  HappyAbsSyn12
		 (AbsGrammar.ImplicitBound
	)

happyReduce_19 = happySpecReduce_3  13 happyReduction_19
happyReduction_19 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (--error "guarded elements not implemented"
    AbsGrammar.GuardedChoiceElem happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (AbsGrammar.SimpleChoiceElem happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  14 happyReduction_21
happyReduction_21 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn14
		 (AbsGrammar.SimpleAtom happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happyReduce 4 14 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (AbsGrammar.CompositeAtom happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_1  15 happyReduction_23
happyReduction_23 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsGrammar.Constant happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  15 happyReduction_24
happyReduction_24 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsGrammar.ArithmeticTerm happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  15 happyReduction_25
happyReduction_25 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsGrammar.StringTerm happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happyReduce 5 15 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AbsGrammar.TupleTerm happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn16
		 (AbsGrammar.PositiveLiteral happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  16 happyReduction_28
happyReduction_28 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (AbsGrammar.NegativeLiteral happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  16 happyReduction_29
happyReduction_29 (HappyAbsSyn15  happy_var_3)
	(HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (-- error "comparisons not implemented"
    AbsGrammar.ComparisonLiteral happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn17
		 (AbsGrammar.CompOpLe
	)

happyReduce_31 = happySpecReduce_1  17 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn17
		 (AbsGrammar.CompOpLeq
	)

happyReduce_32 = happySpecReduce_1  17 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn17
		 (AbsGrammar.CompOpGe
	)

happyReduce_33 = happySpecReduce_1  17 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn17
		 (AbsGrammar.CompOpGeq
	)

happyReduce_34 = happySpecReduce_1  17 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn17
		 (AbsGrammar.CompOpEq
	)

happyReduce_35 = happySpecReduce_1  17 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn17
		 (AbsGrammar.CompOpEqq
	)

happyReduce_36 = happySpecReduce_1  17 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn17
		 (AbsGrammar.CompOpNeq
	)

happyReduce_37 = happySpecReduce_3  18 happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (-- error "arithmetic expressions not implemented"
  AbsGrammar.AddExpr happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  18 happyReduction_38
happyReduction_38 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (-- error "arithmetic expressions not implemented"
  AbsGrammar.SubExpr happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  18 happyReduction_39
happyReduction_39 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  19 happyReduction_40
happyReduction_40 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (-- error "arithmetic expressions not implemented"
  AbsGrammar.MulExpr happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  19 happyReduction_41
happyReduction_41 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (-- error "arithmetic expressions not implemented"
  AbsGrammar.DivExpr happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  19 happyReduction_42
happyReduction_42 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_2  20 happyReduction_43
happyReduction_43 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (-- error "arithmetic expressions not implemented"
   AbsGrammar.NegExpr happy_var_2
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  20 happyReduction_44
happyReduction_44 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  21 happyReduction_45
happyReduction_45 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (-- error "arithmetic expressions not implemented"
  AbsGrammar.AbsExpr happy_var_2
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  21 happyReduction_46
happyReduction_46 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  22 happyReduction_47
happyReduction_47 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn18
		 (-- error "variables are not supported"
  AbsGrammar.Variable happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  22 happyReduction_48
happyReduction_48 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn18
		 (AbsGrammar.IntExpr happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  22 happyReduction_49
happyReduction_49 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happyReduce 9 23 happyReduction_50
happyReduction_50 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (AbsGrammar.LasExample happy_var_3 happy_var_7
	) `HappyStk` happyRest

happyReduce_51 = happyReduce 13 23 happyReduction_51
happyReduction_51 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_11) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (-- error "context examples are not supported"
    AbsGrammar.LasCtxExample happy_var_3 happy_var_7 happy_var_11
	) `HappyStk` happyRest

happyReduce_52 = happySpecReduce_1  24 happyReduction_52
happyReduction_52 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn24
		 ((:[]) happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  24 happyReduction_53
happyReduction_53 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn24
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_0  25 happyReduction_54
happyReduction_54  =  HappyAbsSyn25
		 ([]
	)

happyReduce_55 = happySpecReduce_1  25 happyReduction_55
happyReduction_55 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn25
		 ((:[]) happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  25 happyReduction_56
happyReduction_56 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn25
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  26 happyReduction_57
happyReduction_57 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn26
		 ((:[]) happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  26 happyReduction_58
happyReduction_58 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn26
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_0  27 happyReduction_59
happyReduction_59  =  HappyAbsSyn27
		 ([]
	)

happyReduce_60 = happySpecReduce_3  27 happyReduction_60
happyReduction_60 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn27
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  28 happyReduction_61
happyReduction_61 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn28
		 ((:[]) happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  28 happyReduction_62
happyReduction_62 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn28
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  29 happyReduction_63
happyReduction_63 _
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn29
		 ((:[]) happy_var_1
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  29 happyReduction_64
happyReduction_64 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn29
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 60 60 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 30;
	PT _ (TS _ 2) -> cont 31;
	PT _ (TS _ 3) -> cont 32;
	PT _ (TS _ 4) -> cont 33;
	PT _ (TS _ 5) -> cont 34;
	PT _ (TS _ 6) -> cont 35;
	PT _ (TS _ 7) -> cont 36;
	PT _ (TS _ 8) -> cont 37;
	PT _ (TS _ 9) -> cont 38;
	PT _ (TS _ 10) -> cont 39;
	PT _ (TS _ 11) -> cont 40;
	PT _ (TS _ 12) -> cont 41;
	PT _ (TS _ 13) -> cont 42;
	PT _ (TS _ 14) -> cont 43;
	PT _ (TS _ 15) -> cont 44;
	PT _ (TS _ 16) -> cont 45;
	PT _ (TS _ 17) -> cont 46;
	PT _ (TS _ 18) -> cont 47;
	PT _ (TS _ 19) -> cont 48;
	PT _ (TS _ 20) -> cont 49;
	PT _ (TS _ 21) -> cont 50;
	PT _ (TS _ 22) -> cont 51;
	PT _ (TS _ 23) -> cont 52;
	PT _ (TS _ 24) -> cont 53;
	PT _ (TS _ 25) -> cont 54;
	PT _ (TS _ 26) -> cont 55;
	PT _ (TI happy_dollar_dollar) -> cont 56;
	PT _ (TL happy_dollar_dollar) -> cont 57;
	PT _ (T_VariableSymbol happy_dollar_dollar) -> cont 58;
	PT _ (T_BasicSymbol happy_dollar_dollar) -> cont 59;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 60 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

myShallowLexer :: String -> [Token]
myShallowLexer = shallowTokens

parse :: LexerModality -> String -> Either String AbsGrammar.Program
parse Tight = pProgram . myLexer
parse Shallow = pProgram . myShallowLexer
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
