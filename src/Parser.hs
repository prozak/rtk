{-# OPTIONS_GHC -w #-}
module Parser where

import qualified Lexer as L (Token(..), alexScanTokens)
import Data.Generics
import Data.Data
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20
	= HappyTerminal (L.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20

action_0 (21) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (21) = happyShift action_2
action_1 _ = happyFail

action_2 (39) = happyShift action_4
action_2 _ = happyFail

action_3 (42) = happyAccept
action_3 _ = happyFail

action_4 (26) = happyShift action_5
action_4 _ = happyFail

action_5 (22) = happyShift action_7
action_5 (5) = happyGoto action_6
action_5 _ = happyReduce_3

action_6 (6) = happyGoto action_9
action_6 (7) = happyGoto action_10
action_6 (8) = happyGoto action_11
action_6 _ = happyReduce_8

action_7 (41) = happyShift action_8
action_7 _ = happyFail

action_8 _ = happyReduce_2

action_9 (42) = happyReduce_1
action_9 (7) = happyGoto action_18
action_9 (8) = happyGoto action_11
action_9 _ = happyReduce_8

action_10 _ = happyReduce_4

action_11 (32) = happyShift action_14
action_11 (36) = happyShift action_15
action_11 (37) = happyShift action_16
action_11 (38) = happyShift action_17
action_11 (9) = happyGoto action_12
action_11 (12) = happyGoto action_13
action_11 _ = happyFail

action_12 _ = happyReduce_7

action_13 _ = happyReduce_6

action_14 (38) = happyShift action_23
action_14 _ = happyFail

action_15 (31) = happyShift action_22
action_15 _ = happyFail

action_16 _ = happyReduce_10

action_17 (23) = happyShift action_19
action_17 (25) = happyShift action_20
action_17 (32) = happyShift action_21
action_17 _ = happyFail

action_18 _ = happyReduce_5

action_19 (13) = happyGoto action_30
action_19 (14) = happyGoto action_31
action_19 (15) = happyGoto action_32
action_19 (16) = happyGoto action_33
action_19 _ = happyReduce_24

action_20 (38) = happyShift action_29
action_20 _ = happyFail

action_21 (38) = happyShift action_28
action_21 _ = happyFail

action_22 (38) = happyShift action_27
action_22 (10) = happyGoto action_25
action_22 (11) = happyGoto action_26
action_22 _ = happyReduce_12

action_23 (25) = happyShift action_24
action_23 _ = happyFail

action_24 (38) = happyShift action_50
action_24 _ = happyFail

action_25 (30) = happyShift action_49
action_25 _ = happyFail

action_26 (35) = happyShift action_48
action_26 _ = happyReduce_11

action_27 _ = happyReduce_14

action_28 (25) = happyShift action_47
action_28 _ = happyFail

action_29 (23) = happyShift action_46
action_29 _ = happyFail

action_30 (26) = happyShift action_45
action_30 _ = happyFail

action_31 (24) = happyShift action_44
action_31 _ = happyReduce_19

action_32 _ = happyReduce_21

action_33 (31) = happyShift action_37
action_33 (32) = happyShift action_38
action_33 (33) = happyShift action_39
action_33 (35) = happyShift action_40
action_33 (38) = happyShift action_41
action_33 (39) = happyShift action_42
action_33 (40) = happyShift action_43
action_33 (17) = happyGoto action_34
action_33 (18) = happyGoto action_35
action_33 (19) = happyGoto action_36
action_33 _ = happyReduce_22

action_34 _ = happyReduce_23

action_35 _ = happyReduce_27

action_36 (27) = happyShift action_59
action_36 (28) = happyShift action_60
action_36 (29) = happyShift action_61
action_36 _ = happyReduce_31

action_37 (13) = happyGoto action_58
action_37 (14) = happyGoto action_31
action_37 (15) = happyGoto action_32
action_37 (16) = happyGoto action_33
action_37 _ = happyReduce_24

action_38 _ = happyReduce_35

action_39 (31) = happyShift action_37
action_39 (32) = happyShift action_38
action_39 (38) = happyShift action_41
action_39 (39) = happyShift action_42
action_39 (40) = happyShift action_43
action_39 (18) = happyGoto action_57
action_39 (19) = happyGoto action_36
action_39 _ = happyFail

action_40 (31) = happyShift action_37
action_40 (32) = happyShift action_38
action_40 (38) = happyShift action_41
action_40 (39) = happyShift action_42
action_40 (40) = happyShift action_43
action_40 (18) = happyGoto action_56
action_40 (19) = happyGoto action_36
action_40 _ = happyFail

action_41 _ = happyReduce_33

action_42 _ = happyReduce_34

action_43 _ = happyReduce_36

action_44 (15) = happyGoto action_55
action_44 (16) = happyGoto action_33
action_44 _ = happyReduce_24

action_45 _ = happyReduce_15

action_46 (13) = happyGoto action_54
action_46 (14) = happyGoto action_31
action_46 (15) = happyGoto action_32
action_46 (16) = happyGoto action_33
action_46 _ = happyReduce_24

action_47 (38) = happyShift action_53
action_47 _ = happyFail

action_48 (38) = happyShift action_52
action_48 _ = happyFail

action_49 _ = happyReduce_9

action_50 (23) = happyShift action_51
action_50 _ = happyFail

action_51 (13) = happyGoto action_68
action_51 (14) = happyGoto action_31
action_51 (15) = happyGoto action_32
action_51 (16) = happyGoto action_33
action_51 _ = happyReduce_24

action_52 _ = happyReduce_13

action_53 (23) = happyShift action_67
action_53 _ = happyFail

action_54 (26) = happyShift action_66
action_54 _ = happyFail

action_55 _ = happyReduce_20

action_56 _ = happyReduce_25

action_57 _ = happyReduce_26

action_58 (30) = happyShift action_65
action_58 _ = happyFail

action_59 (34) = happyShift action_63
action_59 (20) = happyGoto action_64
action_59 _ = happyReduce_37

action_60 (34) = happyShift action_63
action_60 (20) = happyGoto action_62
action_60 _ = happyReduce_37

action_61 _ = happyReduce_30

action_62 _ = happyReduce_29

action_63 (31) = happyShift action_37
action_63 (32) = happyShift action_38
action_63 (38) = happyShift action_41
action_63 (39) = happyShift action_42
action_63 (40) = happyShift action_43
action_63 (19) = happyGoto action_71
action_63 _ = happyFail

action_64 _ = happyReduce_28

action_65 _ = happyReduce_32

action_66 _ = happyReduce_16

action_67 (13) = happyGoto action_70
action_67 (14) = happyGoto action_31
action_67 (15) = happyGoto action_32
action_67 (16) = happyGoto action_33
action_67 _ = happyReduce_24

action_68 (26) = happyShift action_69
action_68 _ = happyFail

action_69 _ = happyReduce_18

action_70 (26) = happyShift action_72
action_70 _ = happyFail

action_71 _ = happyReduce_38

action_72 _ = happyReduce_17

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.StrLit happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (InitialGrammar happy_var_2 happy_var_4 (reverse happy_var_5)
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyTerminal (L.BigStr happy_var_2))
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 (""
	)

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_2 : happy_var_1
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (addRuleOptions (reverse happy_var_1) happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  8 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_2 : happy_var_1
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_0  8 happyReduction_8
happyReduction_8  =  HappyAbsSyn8
		 ([]
	)

happyReduce_9 = happyReduce 4 9 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (OShortcuts (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn9
		 (OSymmacro
	)

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  10 happyReduction_12
happyReduction_12  =  HappyAbsSyn10
		 ([]
	)

happyReduce_13 = happySpecReduce_3  11 happyReduction_13
happyReduction_13 (HappyTerminal (L.Id happy_var_3))
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_3 : happy_var_1
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  11 happyReduction_14
happyReduction_14 (HappyTerminal (L.Id happy_var_1))
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 12 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Id happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (IRule Nothing Nothing happy_var_1 happy_var_3 []
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 12 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Id happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Id happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (IRule (Just happy_var_1) Nothing happy_var_3 happy_var_5 []
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 8 12 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Id happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Id happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Id happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (IRule (Just happy_var_1) (Just happy_var_3) happy_var_5 happy_var_7 []
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 7 12 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Id happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Id happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (IRule Nothing (Just happy_var_2) happy_var_4 happy_var_6 []
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (IAlt (reverse happy_var_1)
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  14 happyReduction_20
happyReduction_20 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_3 : happy_var_1
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  14 happyReduction_21
happyReduction_21 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  15 happyReduction_22
happyReduction_22 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (ISeq (reverse happy_var_1)
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  16 happyReduction_23
happyReduction_23 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_2 : happy_var_1
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_0  16 happyReduction_24
happyReduction_24  =  HappyAbsSyn16
		 ([]
	)

happyReduce_25 = happySpecReduce_2  17 happyReduction_25
happyReduction_25 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (ILifted happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  17 happyReduction_26
happyReduction_26 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (IIgnore happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  17 happyReduction_27
happyReduction_27 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  18 happyReduction_28
happyReduction_28 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (IStar happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  18 happyReduction_29
happyReduction_29 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (IPlus happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  18 happyReduction_30
happyReduction_30 _
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (IOpt happy_var_1
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  18 happyReduction_31
happyReduction_31 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  19 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  19 happyReduction_33
happyReduction_33 (HappyTerminal (L.Id happy_var_1))
	 =  HappyAbsSyn19
		 (IId happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  19 happyReduction_34
happyReduction_34 (HappyTerminal (L.StrLit happy_var_1))
	 =  HappyAbsSyn19
		 (IStrLit happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  19 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn19
		 (IDot
	)

happyReduce_36 = happySpecReduce_1  19 happyReduction_36
happyReduction_36 (HappyTerminal (L.RegExpLit happy_var_1))
	 =  HappyAbsSyn19
		 (IRegExpLit happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  20 happyReduction_37
happyReduction_37  =  HappyAbsSyn20
		 (Nothing
	)

happyReduce_38 = happySpecReduce_2  20 happyReduction_38
happyReduction_38 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (Just happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 42 42 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	L.Grammar -> cont 21;
	L.Imports -> cont 22;
	L.Eq -> cont 23;
	L.OrClause -> cont 24;
	L.Colon -> cont 25;
	L.RlEnd -> cont 26;
	L.Star -> cont 27;
	L.Plus -> cont 28;
	L.Question -> cont 29;
	L.RParen -> cont 30;
	L.LParen -> cont 31;
	L.Dot -> cont 32;
	L.Excl -> cont 33;
	L.Tilde -> cont 34;
	L.Comma -> cont 35;
	L.Shortcuts -> cont 36;
	L.Symmacro -> cont 37;
	L.Id happy_dollar_dollar -> cont 38;
	L.StrLit happy_dollar_dollar -> cont 39;
	L.RegExpLit happy_dollar_dollar -> cont 40;
	L.BigStr happy_dollar_dollar -> cont 41;
	_ -> happyError' (tk:tks)
	}

happyError_ 42 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(L.Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [L.Token] -> a
parseError rest = error $ "Parse error" ++ (show rest)

data InitialGrammar = InitialGrammar { getIGrammarName :: String, getImports :: String, getIRules :: [IRule] }
                 deriving (Eq, Show, Typeable, Data)

data IRule = IRule { getIDataTypeName :: (Maybe String), 
                     getIDataFunc :: (Maybe String), 
                     getIRuleName :: String, 
                     getIClause :: IClause,
                     getIRuleOptions :: [IOption]}
                  deriving (Eq, Show, Typeable, Data)

data IOption = OShortcuts [ID] | OSymmacro
                  deriving (Eq, Show, Typeable, Data)

addRuleOptions :: [IOption] -> IRule -> IRule
addRuleOptions opts rule = rule{ getIRuleOptions = opts ++ (getIRuleOptions rule)}                        

type ID = String

data IClause = IId { getIdStr :: ID }
             | IStrLit String
             | IDot
             | IRegExpLit String
             | IStar IClause (Maybe IClause)
             | IPlus IClause (Maybe IClause)
             | IAlt [IClause]
             | ISeq [IClause]
             | IOpt IClause
             | ILifted IClause
             | IIgnore IClause
              deriving (Eq, Show, Typeable, Data)

type LClause = IClause

isLexicalRule :: String -> Bool
isLexicalRule [] = False
isLexicalRule (c:_) = isLower c
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
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
