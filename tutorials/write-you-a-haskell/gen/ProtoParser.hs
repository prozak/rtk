{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
module ProtoParser where
import qualified Data.Generics as Gen
import qualified ProtoLexer as L (Token(..), PosToken(..), AlexPosn(..), alexScanTokens)
import qualified Control.Monad as Happy_Prelude
import qualified Data.Bool as Happy_Prelude
import qualified Data.Function as Happy_Prelude
import qualified Data.Int as Happy_Prelude
import qualified Data.List as Happy_Prelude
import qualified Data.Maybe as Happy_Prelude
import qualified Data.String as Happy_Prelude
import qualified Data.Tuple as Happy_Prelude
import qualified GHC.Err as Happy_Prelude
import qualified GHC.Num as Happy_Prelude
import qualified Text.Show as Happy_Prelude
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.2

data HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40
        = HappyTerminal (L.PosToken)
        | HappyErrorToken Happy_Prelude.Int
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
        | HappyAbsSyn21 t21
        | HappyAbsSyn22 t22
        | HappyAbsSyn23 t23
        | HappyAbsSyn24 t24
        | HappyAbsSyn25 t25
        | HappyAbsSyn26 t26
        | HappyAbsSyn27 t27
        | HappyAbsSyn28 t28
        | HappyAbsSyn29 t29
        | HappyAbsSyn30 t30
        | HappyAbsSyn31 t31
        | HappyAbsSyn32 t32
        | HappyAbsSyn33 t33
        | HappyAbsSyn34 t34
        | HappyAbsSyn35 t35
        | HappyAbsSyn36 t36
        | HappyAbsSyn37 t37
        | HappyAbsSyn38 t38
        | HappyAbsSyn39 t39
        | HappyAbsSyn40 t40

{-# NOINLINE happyTokenStrings #-}
happyTokenStrings = ["rtk__eof","tok_Alt_dummy_25","tok_AltList_dummy_24","tok_ConDef_dummy_23","tok_ConDefList_dummy_22","tok_ConId_dummy_21","tok_Decl_dummy_20","tok_DeclList_dummy_19","tok_Expr_dummy_18","tok_Field_dummy_17","tok_FieldList_dummy_16","tok_Id_dummy_15","tok_PArg_dummy_14","tok_PArgs_dummy_13","tok_Param_dummy_12","tok_Params_dummy_11","tok_Pat_dummy_10","tok_Proto_dummy_26","tok_Ty_dummy_9","tok_TyVar_dummy_8","tok_TyVarList_dummy_7","tok__symbol__18","tok__pipe__5","tok__symbol__17","tok_true_20","tok_then_12","tok_rec_4","tok_of_16","tok_let_3","tok_in_10","tok_if_11","tok_fix_14","tok_false_21","tok_else_13","tok_data_0","tok_case_15","tok__symbol__19","tok__symbol__9","tok__eql__eql__22","tok__eql__1","tok__semi__2","tok__minus__symbol__6","tok__minus__24","tok__plus__23","tok__star__25","tok__rparen__8","tok__lparen__7","conid","id","num","qq_Id","qq_ConId","qq_Param","qq_Params","qq_Expr","qq_PArg","qq_PArgs","qq_Pat","qq_Alt","qq_AltList","qq_Ty","qq_Field","qq_FieldList","qq_ConDef","qq_ConDefList","qq_TyVar","qq_TyVarList","qq_Decl","qq_DeclList","qq_Proto","%eof"]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xfe\xff\xff\xff\xfe\xff\xff\xff\x25\x00\x00\x00\x43\x00\x00\x00\xa0\x00\x00\x00\x8c\x00\x00\x00\x78\x00\x00\x00\x41\x01\x00\x00\xc0\x00\x00\x00\x70\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x15\x01\x00\x00\x00\x00\x00\x00\xeb\xff\xff\xff\xd0\x00\x00\x00\xb4\x00\x00\x00\x39\x00\x00\x00\x6d\x02\x00\x00\xe4\x00\x00\x00\xfe\xff\xff\xff\x5b\x01\x00\x00\x43\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\xff\xff\x1c\x00\x00\x00\x00\x00\x00\x00\x6e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x01\x00\x00\x26\x00\x00\x00\x5b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x36\x00\x00\x00\xb4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x68\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7d\x00\x00\x00\x9f\x00\x00\x00\x16\x00\x00\x00\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x01\x00\x00\x6b\x00\x00\x00\xea\xff\xff\xff\xb8\x00\x00\x00\xaa\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x6d\x02\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\xd9\x00\x00\x00\xc1\x00\x00\x00\x05\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\xe5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x35\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xee\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\xe1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x8c\x00\x00\x00\x00\x00\x00\x00\x4a\x01\x00\x00\x00\x00\x00\x00\x41\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x68\x01\x00\x00\xeb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\x00\x00\x00\x56\x02\x00\x00\x07\x01\x00\x00\x00\x00\x00\x00\x0b\x01\x00\x00\x03\x01\x00\x00\xeb\xff\xff\xff\x00\x00\x00\x00\x09\x01\x00\x00\x09\x01\x00\x00\x09\x01\x00\x00\x09\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\x00\x00\x00\x00\x00\x00\x00\xb4\x00\x00\x00\x00\x00\x00\x00\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x01\x00\x00\x08\x01\x00\x00\x08\x01\x00\x00\xea\xff\xff\xff\x0e\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x26\x01\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x6e\x01\x00\x00\x74\x01\x00\x00\x00\x01\x00\x00\xf1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x01\x00\x00\xfa\xff\xff\xff\x00\x01\x00\x00\x7a\x01\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x8c\x00\x00\x00\x2b\x01\x00\x00\x33\x01\x00\x00\x00\x01\x00\x00\x3c\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x01\x00\x00\x00\x34\x00\x00\x00\x00\x01\x00\x00\x47\x01\x00\x00\x00\x00\x00\x00\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4e\x01\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xd7\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\xaa\x01\x00\x00\xf2\x00\x00\x00\xdb\x00\x00\x00\x8e\x00\x00\x00\xa0\x02\x00\x00\x6b\x01\x00\x00\xb4\x01\x00\x00\x72\x01\x00\x00\xbe\x01\x00\x00\x95\x00\x00\x00\x70\x01\x00\x00\x81\x01\x00\x00\x13\x00\x00\x00\x49\x01\x00\x00\x96\x00\x00\x00\x76\x02\x00\x00\x54\x00\x00\x00\xce\x00\x00\x00\xf3\x00\x00\x00\x2e\x00\x00\x00\x62\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x00\x00\x38\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x52\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x36\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x83\x01\x00\x00\xc8\x01\x00\x00\xd2\x01\x00\x00\x00\x00\x00\x00\xdc\x01\x00\x00\x79\x02\x00\x00\xe6\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaa\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x01\x00\x00\x7f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x93\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\x01\x00\x00\x00\x00\x00\x00\xae\x00\x00\x00\x00\x00\x00\x00\x7a\x00\x00\x00\x00\x00\x00\x00\xf7\x00\x00\x00\x00\x00\x00\x00\x8a\x01\x00\x00\x80\x02\x00\x00\x94\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x36\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9a\x01\x00\x00\x00\x00\x00\x00\x86\x01\x00\x00\x68\x02\x00\x00\x72\x02\x00\x00\x6f\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfa\x01\x00\x00\x04\x02\x00\x00\x00\x00\x00\x00\x0e\x02\x00\x00\x00\x00\x00\x00\x83\x02\x00\x00\x36\x01\x00\x00\x18\x02\x00\x00\x5f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa4\x02\x00\x00\x00\x00\x00\x00\x22\x02\x00\x00\x36\x01\x00\x00\x2c\x02\x00\x00\x00\x00\x00\x00\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x36\x02\x00\x00\x00\x00\x00\x00\x40\x02\x00\x00\x4a\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x54\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xd0\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb3\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x8d\xff\xff\xff\xe9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x90\xff\xff\xff\x00\x00\x00\x00\xb0\xff\xff\xff\xb1\xff\xff\xff\x91\xff\xff\xff\x98\xff\xff\xff\x97\xff\xff\xff\x94\xff\xff\xff\x92\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xda\xff\xff\xff\xdb\xff\xff\xff\x99\xff\xff\xff\x00\x00\x00\x00\x9d\xff\xff\xff\x9e\xff\xff\xff\x9a\xff\xff\xff\x00\x00\x00\x00\xa0\xff\xff\xff\x9f\xff\xff\xff\xa1\xff\xff\xff\x00\x00\x00\x00\xa2\xff\xff\xff\xa3\xff\xff\xff\xa8\xff\xff\xff\xa6\xff\xff\xff\xa5\xff\xff\xff\x00\x00\x00\x00\xa9\xff\xff\xff\xa7\xff\xff\xff\x00\x00\x00\x00\x9d\xff\xff\xff\xac\xff\xff\xff\xab\xff\xff\xff\x00\x00\x00\x00\xae\xff\xff\xff\xaf\xff\xff\xff\xad\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\xff\xff\xff\xb7\xff\xff\xff\xc9\xff\xff\xff\xc6\xff\xff\xff\xc4\xff\xff\xff\xc1\xff\xff\xff\xbf\xff\xff\xff\xb8\xff\xff\xff\x00\x00\x00\x00\xca\xff\xff\xff\xcc\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcd\xff\xff\xff\xce\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd9\xff\xff\xff\x00\x00\x00\x00\xde\xff\xff\xff\xdd\xff\xff\xff\x00\x00\x00\x00\xb3\xff\xff\xff\xe1\xff\xff\xff\xdf\xff\xff\xff\x00\x00\x00\x00\xe4\xff\xff\xff\xe3\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe7\xff\xff\xff\xe5\xff\xff\xff\x00\x00\x00\x00\xd1\xff\xff\xff\xcf\xff\xff\xff\xd2\xff\xff\xff\xfe\xff\xff\xff\xfc\xff\xff\xff\x00\x00\x00\x00\xfb\xff\xff\xff\x00\x00\x00\x00\xfa\xff\xff\xff\xe0\xff\xff\xff\xf9\xff\xff\xff\x00\x00\x00\x00\xf8\xff\xff\xff\x8d\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd3\xff\xff\xff\xf7\xff\xff\xff\xf6\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xba\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc7\xff\xff\xff\xf4\xff\xff\xff\xb4\xff\xff\xff\xb2\xff\xff\xff\xf3\xff\xff\xff\xb5\xff\xff\xff\xf2\xff\xff\xff\xf1\xff\xff\xff\xaa\xff\xff\xff\xf0\xff\xff\xff\xef\xff\xff\xff\xa4\xff\xff\xff\xee\xff\xff\xff\x00\x00\x00\x00\xed\xff\xff\xff\x9b\xff\xff\xff\xfd\xff\xff\xff\x00\x00\x00\x00\xec\xff\xff\xff\x95\xff\xff\xff\x00\x00\x00\x00\xeb\xff\xff\xff\x8e\xff\xff\xff\x8c\xff\xff\xff\xea\xff\xff\xff\x8f\xff\xff\xff\x93\xff\xff\xff\x96\xff\xff\xff\x9c\xff\xff\xff\xc5\xff\xff\xff\xc3\xff\xff\xff\xc2\xff\xff\xff\xc0\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdc\xff\xff\xff\xe2\xff\xff\xff\xe6\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbe\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd5\xff\xff\xff\x00\x00\x00\x00\xd8\xff\xff\xff\xd4\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd7\xff\xff\xff\xb9\xff\xff\xff\xbb\xff\xff\xff\xbc\xff\xff\xff\xbd\xff\xff\xff\xd6\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x09\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x17\x00\x00\x00\x1f\x00\x00\x00\x29\x00\x00\x00\x08\x00\x00\x00\x04\x00\x00\x00\x32\x00\x00\x00\x48\x00\x00\x00\x34\x00\x00\x00\x1c\x00\x00\x00\x1c\x00\x00\x00\x0c\x00\x00\x00\x32\x00\x00\x00\x2a\x00\x00\x00\x34\x00\x00\x00\x1a\x00\x00\x00\x02\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x1e\x00\x00\x00\x2a\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x1c\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x16\x00\x00\x00\x27\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x32\x00\x00\x00\x32\x00\x00\x00\x34\x00\x00\x00\x34\x00\x00\x00\x14\x00\x00\x00\x06\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x2a\x00\x00\x00\x15\x00\x00\x00\x38\x00\x00\x00\x47\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x13\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x08\x00\x00\x00\x18\x00\x00\x00\x32\x00\x00\x00\x21\x00\x00\x00\x34\x00\x00\x00\x45\x00\x00\x00\x46\x00\x00\x00\x1f\x00\x00\x00\x3e\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x12\x00\x00\x00\x10\x00\x00\x00\x15\x00\x00\x00\x18\x00\x00\x00\x0f\x00\x00\x00\x08\x00\x00\x00\x1a\x00\x00\x00\x2a\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x1e\x00\x00\x00\x1e\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x1a\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x15\x00\x00\x00\x27\x00\x00\x00\x32\x00\x00\x00\x2a\x00\x00\x00\x34\x00\x00\x00\x22\x00\x00\x00\x36\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x26\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x38\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x08\x00\x00\x00\x15\x00\x00\x00\x0b\x00\x00\x00\x39\x00\x00\x00\x3a\x00\x00\x00\x3b\x00\x00\x00\x45\x00\x00\x00\x46\x00\x00\x00\x1a\x00\x00\x00\x0e\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x1e\x00\x00\x00\x15\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x05\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x08\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x2e\x00\x00\x00\x32\x00\x00\x00\x0b\x00\x00\x00\x34\x00\x00\x00\x08\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x1a\x00\x00\x00\x12\x00\x00\x00\x38\x00\x00\x00\x31\x00\x00\x00\x15\x00\x00\x00\x15\x00\x00\x00\x0d\x00\x00\x00\x35\x00\x00\x00\x22\x00\x00\x00\x19\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x26\x00\x00\x00\x1e\x00\x00\x00\x0a\x00\x00\x00\x45\x00\x00\x00\x08\x00\x00\x00\x08\x00\x00\x00\x08\x00\x00\x00\x41\x00\x00\x00\x1a\x00\x00\x00\x0c\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x22\x00\x00\x00\x15\x00\x00\x00\x15\x00\x00\x00\x15\x00\x00\x00\x26\x00\x00\x00\x3b\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x1a\x00\x00\x00\x01\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x26\x00\x00\x00\x3b\x00\x00\x00\x3c\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x28\x00\x00\x00\x08\x00\x00\x00\x0b\x00\x00\x00\x08\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x1a\x00\x00\x00\x2a\x00\x00\x00\x07\x00\x00\x00\x39\x00\x00\x00\x3a\x00\x00\x00\x3b\x00\x00\x00\x15\x00\x00\x00\x31\x00\x00\x00\x22\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x35\x00\x00\x00\x26\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x03\x00\x00\x00\x08\x00\x00\x00\x08\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x1a\x00\x00\x00\x08\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x22\x00\x00\x00\x15\x00\x00\x00\x15\x00\x00\x00\x39\x00\x00\x00\x26\x00\x00\x00\x3b\x00\x00\x00\x2b\x00\x00\x00\x2f\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x15\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x1a\x00\x00\x00\x08\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x1e\x00\x00\x00\x3b\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x1a\x00\x00\x00\x1d\x00\x00\x00\x25\x00\x00\x00\x1b\x00\x00\x00\x27\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x22\x00\x00\x00\x29\x00\x00\x00\x2f\x00\x00\x00\x2f\x00\x00\x00\x1c\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x2e\x00\x00\x00\x29\x00\x00\x00\x38\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x19\x00\x00\x00\x08\x00\x00\x00\x38\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x08\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x15\x00\x00\x00\x08\x00\x00\x00\x15\x00\x00\x00\x23\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x08\x00\x00\x00\x1f\x00\x00\x00\x3e\x00\x00\x00\x3f\x00\x00\x00\x15\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x15\x00\x00\x00\x08\x00\x00\x00\x1f\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x1c\x00\x00\x00\x2b\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x1c\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x2a\x00\x00\x00\x31\x00\x00\x00\x08\x00\x00\x00\x15\x00\x00\x00\x32\x00\x00\x00\x35\x00\x00\x00\x34\x00\x00\x00\x2a\x00\x00\x00\x3e\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x0b\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x14\x00\x00\x00\x23\x00\x00\x00\x43\x00\x00\x00\x08\x00\x00\x00\x3e\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x08\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x29\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x15\x00\x00\x00\x29\x00\x00\x00\x15\x00\x00\x00\x3e\x00\x00\x00\x32\x00\x00\x00\x15\x00\x00\x00\x34\x00\x00\x00\x29\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x32\x00\x00\x00\x15\x00\x00\x00\x34\x00\x00\x00\x29\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x32\x00\x00\x00\x14\x00\x00\x00\x34\x00\x00\x00\x15\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x32\x00\x00\x00\x23\x00\x00\x00\x34\x00\x00\x00\x15\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\xff\xff\xff\xff\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x15\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x2b\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x32\x00\x00\x00\xff\xff\xff\xff\x34\x00\x00\x00\x15\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x15\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x15\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x32\x00\x00\x00\xff\xff\xff\xff\x34\x00\x00\x00\xff\xff\xff\xff\x36\x00\x00\x00\x37\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x82\x00\x00\x00\x8c\x00\x00\x00\x8d\x00\x00\x00\xd5\x00\x00\x00\xc8\x00\x00\x00\xbd\x00\x00\x00\x3b\x00\x00\x00\x76\x00\x00\x00\x1e\x00\x00\x00\xff\xff\xff\xff\x1f\x00\x00\x00\x89\x00\x00\x00\x7f\x00\x00\x00\x93\x00\x00\x00\x1e\x00\x00\x00\xce\x00\x00\x00\x1f\x00\x00\x00\x51\x00\x00\x00\x73\x00\x00\x00\x2b\x00\x00\x00\x42\x00\x00\x00\x5e\x00\x00\x00\x77\x00\x00\x00\x53\x00\x00\x00\x54\x00\x00\x00\x55\x00\x00\x00\x3f\x00\x00\x00\x5f\x00\x00\x00\x56\x00\x00\x00\xa7\x00\x00\x00\x57\x00\x00\x00\x20\x00\x00\x00\xa8\x00\x00\x00\x1e\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x1f\x00\x00\x00\xa1\x00\x00\x00\x7a\x00\x00\x00\x58\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x59\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\x77\x00\x00\x00\x1b\x00\x00\x00\x5a\x00\x00\x00\x19\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x9f\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\x20\x00\x00\x00\x7b\x00\x00\x00\x1e\x00\x00\x00\x1c\x00\x00\x00\x1f\x00\x00\x00\x60\x00\x00\x00\x72\x00\x00\x00\xd3\x00\x00\x00\x29\x00\x00\x00\x48\x00\x00\x00\x94\x00\x00\x00\x9d\x00\x00\x00\x99\x00\x00\x00\x21\x00\x00\x00\x7b\x00\x00\x00\x98\x00\x00\x00\x2a\x00\x00\x00\x51\x00\x00\x00\xd4\x00\x00\x00\x20\x00\x00\x00\xa8\x00\x00\x00\x5e\x00\x00\x00\xa1\x00\x00\x00\x53\x00\x00\x00\x54\x00\x00\x00\x55\x00\x00\x00\x2f\x00\x00\x00\x5f\x00\x00\x00\x56\x00\x00\x00\x2b\x00\x00\x00\x57\x00\x00\x00\x1e\x00\x00\x00\xd0\x00\x00\x00\x1f\x00\x00\x00\x30\x00\x00\x00\x39\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x31\x00\x00\x00\x58\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x59\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\x9b\x00\x00\x00\x02\x00\x00\x00\x5a\x00\x00\x00\x32\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x33\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\x20\x00\x00\x00\xa4\x00\x00\x00\x03\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x34\x00\x00\x00\x60\x00\x00\x00\x72\x00\x00\x00\x51\x00\x00\x00\x96\x00\x00\x00\x90\x00\x00\x00\x91\x00\x00\x00\x5e\x00\x00\x00\x21\x00\x00\x00\x53\x00\x00\x00\x54\x00\x00\x00\x55\x00\x00\x00\x67\x00\x00\x00\x5f\x00\x00\x00\x56\x00\x00\x00\x64\x00\x00\x00\x57\x00\x00\x00\x46\x00\x00\x00\x8e\x00\x00\x00\x1e\x00\x00\x00\x90\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x39\x00\x00\x00\x3a\x00\x00\x00\x58\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x59\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\x2f\x00\x00\x00\x45\x00\x00\x00\x5a\x00\x00\x00\x27\x00\x00\x00\x21\x00\x00\x00\x34\x00\x00\x00\x95\x00\x00\x00\x28\x00\x00\x00\x30\x00\x00\x00\x3a\x00\x00\x00\x68\x00\x00\x00\xba\x00\x00\x00\x31\x00\x00\x00\x46\x00\x00\x00\x8a\x00\x00\x00\x60\x00\x00\x00\x2a\x00\x00\x00\x48\x00\x00\x00\x2a\x00\x00\x00\x66\x00\x00\x00\x2f\x00\x00\x00\x8e\x00\x00\x00\x32\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x33\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\x30\x00\x00\x00\x2b\x00\x00\x00\x4f\x00\x00\x00\x2b\x00\x00\x00\x31\x00\x00\x00\x34\x00\x00\x00\x6d\x00\x00\x00\x6e\x00\x00\x00\x2c\x00\x00\x00\x6b\x00\x00\x00\x2c\x00\x00\x00\x9b\x00\x00\x00\x2f\x00\x00\x00\x29\x00\x00\x00\x32\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x33\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\x30\x00\x00\x00\x19\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x31\x00\x00\x00\x34\x00\x00\x00\x6d\x00\x00\x00\x68\x00\x00\x00\x69\x00\x00\x00\x6a\x00\x00\x00\x8b\x00\x00\x00\x81\x00\x00\x00\x03\x00\x00\x00\x2a\x00\x00\x00\x32\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x33\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\x2f\x00\x00\x00\x80\x00\x00\x00\x7c\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x34\x00\x00\x00\x2b\x00\x00\x00\x27\x00\x00\x00\x30\x00\x00\x00\x78\x00\x00\x00\x6e\x00\x00\x00\x28\x00\x00\x00\x31\x00\x00\x00\x2c\x00\x00\x00\x6b\x00\x00\x00\x74\x00\x00\x00\x2a\x00\x00\x00\x20\x00\x00\x00\x61\x00\x00\x00\xb9\x00\x00\x00\x2f\x00\x00\x00\x64\x00\x00\x00\x32\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x33\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\x30\x00\x00\x00\x2b\x00\x00\x00\x21\x00\x00\x00\x41\x00\x00\x00\x31\x00\x00\x00\x34\x00\x00\x00\x75\x00\x00\x00\xb5\x00\x00\x00\x2c\x00\x00\x00\x6b\x00\x00\x00\x1b\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x32\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x33\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\x51\x00\x00\x00\x3b\x00\x00\x00\xa4\x00\x00\x00\xa5\x00\x00\x00\x52\x00\x00\x00\x34\x00\x00\x00\x53\x00\x00\x00\x54\x00\x00\x00\x55\x00\x00\x00\x51\x00\x00\x00\xb3\x00\x00\x00\x56\x00\x00\x00\xb2\x00\x00\x00\x57\x00\x00\x00\x2b\x00\x00\x00\x3c\x00\x00\x00\x96\x00\x00\x00\x55\x00\x00\x00\xb1\x00\x00\x00\xab\x00\x00\x00\xaa\x00\x00\x00\x3f\x00\x00\x00\x58\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x59\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\x8e\x00\x00\x00\xc6\x00\x00\x00\x5a\x00\x00\x00\x58\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x59\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\xc3\x00\x00\x00\x20\x00\x00\x00\x5a\x00\x00\x00\x68\x00\x00\x00\x69\x00\x00\x00\xc9\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x2a\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\x34\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\xc9\x00\x00\x00\x35\x00\x00\x00\x99\x00\x00\x00\x3b\x00\x00\x00\xc8\x00\x00\x00\x29\x00\x00\x00\x48\x00\x00\x00\x2b\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x9f\x00\x00\x00\x21\x00\x00\x00\x3b\x00\x00\x00\xd3\x00\x00\x00\x2c\x00\x00\x00\x6b\x00\x00\x00\x2b\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x3e\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\xa8\x00\x00\x00\x3f\x00\x00\x00\xa3\x00\x00\x00\x2b\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x9d\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x3f\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\xd1\x00\x00\x00\x27\x00\x00\x00\x60\x00\x00\x00\x1b\x00\x00\x00\x1e\x00\x00\x00\x28\x00\x00\x00\x1f\x00\x00\x00\xd9\x00\x00\x00\x29\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x5a\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\xa4\x00\x00\x00\xa5\x00\x00\x00\x66\x00\x00\x00\x67\x00\x00\x00\x44\x00\x00\x00\x1a\x00\x00\x00\x20\x00\x00\x00\x7c\x00\x00\x00\x29\x00\x00\x00\x48\x00\x00\x00\x94\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x1e\x00\x00\x00\x48\x00\x00\x00\x1f\x00\x00\x00\x28\x00\x00\x00\xb8\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\xae\x00\x00\x00\x43\x00\x00\x00\xc1\x00\x00\x00\x87\x00\x00\x00\x29\x00\x00\x00\x1e\x00\x00\x00\x4f\x00\x00\x00\x1f\x00\x00\x00\xbf\x00\x00\x00\x39\x00\x00\x00\x3a\x00\x00\x00\x1e\x00\x00\x00\x7d\x00\x00\x00\x1f\x00\x00\x00\xcc\x00\x00\x00\x39\x00\x00\x00\x3a\x00\x00\x00\x1e\x00\x00\x00\x78\x00\x00\x00\x1f\x00\x00\x00\xb5\x00\x00\x00\x39\x00\x00\x00\x3a\x00\x00\x00\x1e\x00\x00\x00\xb8\x00\x00\x00\x1f\x00\x00\x00\xaf\x00\x00\x00\x39\x00\x00\x00\x3a\x00\x00\x00\x48\x00\x00\x00\x6f\x00\x00\x00\x70\x00\x00\x00\x00\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\x5c\x00\x00\x00\x48\x00\x00\x00\x5b\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\x5c\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\x4e\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\x86\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\x85\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\x84\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\x82\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\xbb\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\xc4\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\xc3\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\xc1\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\xbd\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\xcc\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\xca\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\xc6\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\xd6\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\xd5\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\xd1\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\xd7\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\xad\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x00\x00\x00\x49\x00\x00\x00\xab\x00\x00\x00\x4f\x00\x00\x00\x49\x00\x00\x00\x4a\x00\x00\x00\xac\x00\x00\x00\xb4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x34\x00\x00\x00\x39\x00\x00\x00\x3a\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x35\x00\x00\x00\x36\x00\x00\x00\x83\x00\x00\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x36\x00\x00\x00\xb6\x00\x00\x00\x35\x00\x00\x00\x36\x00\x00\x00\xbf\x00\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x39\x00\x00\x00\x3a\x00\x00\x00\x61\x00\x00\x00\x62\x00\x00\x00\x63\x00\x00\x00\x64\x00\x00\x00\x61\x00\x00\x00\x62\x00\x00\x00\xce\x00\x00\x00\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 115) [
        (1 , happyReduce_1),
        (2 , happyReduce_2),
        (3 , happyReduce_3),
        (4 , happyReduce_4),
        (5 , happyReduce_5),
        (6 , happyReduce_6),
        (7 , happyReduce_7),
        (8 , happyReduce_8),
        (9 , happyReduce_9),
        (10 , happyReduce_10),
        (11 , happyReduce_11),
        (12 , happyReduce_12),
        (13 , happyReduce_13),
        (14 , happyReduce_14),
        (15 , happyReduce_15),
        (16 , happyReduce_16),
        (17 , happyReduce_17),
        (18 , happyReduce_18),
        (19 , happyReduce_19),
        (20 , happyReduce_20),
        (21 , happyReduce_21),
        (22 , happyReduce_22),
        (23 , happyReduce_23),
        (24 , happyReduce_24),
        (25 , happyReduce_25),
        (26 , happyReduce_26),
        (27 , happyReduce_27),
        (28 , happyReduce_28),
        (29 , happyReduce_29),
        (30 , happyReduce_30),
        (31 , happyReduce_31),
        (32 , happyReduce_32),
        (33 , happyReduce_33),
        (34 , happyReduce_34),
        (35 , happyReduce_35),
        (36 , happyReduce_36),
        (37 , happyReduce_37),
        (38 , happyReduce_38),
        (39 , happyReduce_39),
        (40 , happyReduce_40),
        (41 , happyReduce_41),
        (42 , happyReduce_42),
        (43 , happyReduce_43),
        (44 , happyReduce_44),
        (45 , happyReduce_45),
        (46 , happyReduce_46),
        (47 , happyReduce_47),
        (48 , happyReduce_48),
        (49 , happyReduce_49),
        (50 , happyReduce_50),
        (51 , happyReduce_51),
        (52 , happyReduce_52),
        (53 , happyReduce_53),
        (54 , happyReduce_54),
        (55 , happyReduce_55),
        (56 , happyReduce_56),
        (57 , happyReduce_57),
        (58 , happyReduce_58),
        (59 , happyReduce_59),
        (60 , happyReduce_60),
        (61 , happyReduce_61),
        (62 , happyReduce_62),
        (63 , happyReduce_63),
        (64 , happyReduce_64),
        (65 , happyReduce_65),
        (66 , happyReduce_66),
        (67 , happyReduce_67),
        (68 , happyReduce_68),
        (69 , happyReduce_69),
        (70 , happyReduce_70),
        (71 , happyReduce_71),
        (72 , happyReduce_72),
        (73 , happyReduce_73),
        (74 , happyReduce_74),
        (75 , happyReduce_75),
        (76 , happyReduce_76),
        (77 , happyReduce_77),
        (78 , happyReduce_78),
        (79 , happyReduce_79),
        (80 , happyReduce_80),
        (81 , happyReduce_81),
        (82 , happyReduce_82),
        (83 , happyReduce_83),
        (84 , happyReduce_84),
        (85 , happyReduce_85),
        (86 , happyReduce_86),
        (87 , happyReduce_87),
        (88 , happyReduce_88),
        (89 , happyReduce_89),
        (90 , happyReduce_90),
        (91 , happyReduce_91),
        (92 , happyReduce_92),
        (93 , happyReduce_93),
        (94 , happyReduce_94),
        (95 , happyReduce_95),
        (96 , happyReduce_96),
        (97 , happyReduce_97),
        (98 , happyReduce_98),
        (99 , happyReduce_99),
        (100 , happyReduce_100),
        (101 , happyReduce_101),
        (102 , happyReduce_102),
        (103 , happyReduce_103),
        (104 , happyReduce_104),
        (105 , happyReduce_105),
        (106 , happyReduce_106),
        (107 , happyReduce_107),
        (108 , happyReduce_108),
        (109 , happyReduce_109),
        (110 , happyReduce_110),
        (111 , happyReduce_111),
        (112 , happyReduce_112),
        (113 , happyReduce_113),
        (114 , happyReduce_114),
        (115 , happyReduce_115)
        ]

happyRuleArr :: HappyAddr
happyRuleArr = HappyA# "\x00\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x04\x00\x00\x00\x01\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x05\x00\x00\x00\x01\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x01\x00\x00\x00\x06\x00\x00\x00\x01\x00\x00\x00\x07\x00\x00\x00\x01\x00\x00\x00\x07\x00\x00\x00\x03\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x09\x00\x00\x00\x01\x00\x00\x00\x09\x00\x00\x00\x06\x00\x00\x00\x09\x00\x00\x00\x06\x00\x00\x00\x09\x00\x00\x00\x07\x00\x00\x00\x09\x00\x00\x00\x05\x00\x00\x00\x09\x00\x00\x00\x06\x00\x00\x00\x09\x00\x00\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00\x02\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x0c\x00\x00\x00\x03\x00\x00\x00\x0d\x00\x00\x00\x02\x00\x00\x00\x0d\x00\x00\x00\x01\x00\x00\x00\x0e\x00\x00\x00\x03\x00\x00\x00\x0e\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x03\x00\x00\x00\x0f\x00\x00\x00\x03\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x03\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x04\x00\x00\x00\x11\x00\x00\x00\x07\x00\x00\x00\x11\x00\x00\x00\x06\x00\x00\x00\x11\x00\x00\x00\x06\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x11\x00\x00\x00\x06\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x12\x00\x00\x00\x01\x00\x00\x00\x12\x00\x00\x00\x01\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x15\x00\x00\x00\x01\x00\x00\x00\x15\x00\x00\x00\x01\x00\x00\x00\x16\x00\x00\x00\x01\x00\x00\x00\x16\x00\x00\x00\x01\x00\x00\x00\x17\x00\x00\x00\x01\x00\x00\x00\x17\x00\x00\x00\x01\x00\x00\x00\x18\x00\x00\x00\x01\x00\x00\x00\x18\x00\x00\x00\x02\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x1a\x00\x00\x00\x01\x00\x00\x00\x1a\x00\x00\x00\x01\x00\x00\x00\x1b\x00\x00\x00\x01\x00\x00\x00\x1b\x00\x00\x00\x02\x00\x00\x00\x1c\x00\x00\x00\x01\x00\x00\x00\x1c\x00\x00\x00\x01\x00\x00\x00\x1c\x00\x00\x00\x01\x00\x00\x00\x1c\x00\x00\x00\x01\x00\x00\x00\x1c\x00\x00\x00\x01\x00\x00\x00\x1c\x00\x00\x00\x01\x00\x00\x00\x1c\x00\x00\x00\x01\x00\x00\x00\x1c\x00\x00\x00\x03\x00\x00\x00\x1d\x00\x00\x00\x02\x00\x00\x00\x1d\x00\x00\x00\x01\x00\x00\x00\x1e\x00\x00\x00\x01\x00\x00\x00\x1e\x00\x00\x00\x01\x00\x00\x00\x1e\x00\x00\x00\x01\x00\x00\x00\x1e\x00\x00\x00\x03\x00\x00\x00\x1f\x00\x00\x00\x02\x00\x00\x00\x1f\x00\x00\x00\x01\x00\x00\x00\x20\x00\x00\x00\x03\x00\x00\x00\x20\x00\x00\x00\x01\x00\x00\x00\x21\x00\x00\x00\x01\x00\x00\x00\x21\x00\x00\x00\x01\x00\x00\x00\x22\x00\x00\x00\x01\x00\x00\x00\x22\x00\x00\x00\x01\x00\x00\x00\x23\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x02\x00\x00\x00"#

happyCatchStates :: [Happy_Prelude.Int]
happyCatchStates = []

happy_n_terms = 73 :: Happy_Prelude.Int
happy_n_nonterms = 36 :: Happy_Prelude.Int

happy_n_starts = 1 :: Happy_Prelude.Int

happyReduce_1 = happySpecReduce_2  0# happyReduction_1
happyReduction_1 _
        (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn5
                 (happy_var_1
        )
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  1# happyReduction_2
happyReduction_2 _
        (HappyAbsSyn6  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__0 (rtkPosOf happy_var_1) happy_var_2
        )
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  1# happyReduction_3
happyReduction_3 _
        (HappyAbsSyn7  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__1 (rtkPosOf happy_var_1) happy_var_2
        )
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  1# happyReduction_4
happyReduction_4 _
        (HappyAbsSyn9  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__2 (rtkPosOf happy_var_1) (reverse happy_var_2)
        )
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  1# happyReduction_5
happyReduction_5 _
        (HappyAbsSyn10  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__3 (rtkPosOf happy_var_1) happy_var_2
        )
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  1# happyReduction_6
happyReduction_6 _
        (HappyAbsSyn12  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__4 (rtkPosOf happy_var_1) (reverse happy_var_2)
        )
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  1# happyReduction_7
happyReduction_7 _
        (HappyAbsSyn13  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__5 (rtkPosOf happy_var_1) happy_var_2
        )
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  1# happyReduction_8
happyReduction_8 _
        (HappyAbsSyn14  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__6 (rtkPosOf happy_var_1) happy_var_2
        )
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  1# happyReduction_9
happyReduction_9 _
        (HappyAbsSyn16  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__7 (rtkPosOf happy_var_1) (reverse happy_var_2)
        )
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  1# happyReduction_10
happyReduction_10 _
        (HappyAbsSyn22  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__8 (rtkPosOf happy_var_1) happy_var_2
        )
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  1# happyReduction_11
happyReduction_11 _
        (HappyAbsSyn23  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__9 (rtkPosOf happy_var_1) happy_var_2
        )
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  1# happyReduction_12
happyReduction_12 _
        (HappyAbsSyn25  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__10 (rtkPosOf happy_var_1) (reverse happy_var_2)
        )
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  1# happyReduction_13
happyReduction_13 _
        (HappyAbsSyn26  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__11 (rtkPosOf happy_var_1) happy_var_2
        )
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  1# happyReduction_14
happyReduction_14 _
        (HappyAbsSyn27  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__12 (rtkPosOf happy_var_1) happy_var_2
        )
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  1# happyReduction_15
happyReduction_15 _
        (HappyAbsSyn29  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__13 (rtkPosOf happy_var_1) (reverse happy_var_2)
        )
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  1# happyReduction_16
happyReduction_16 _
        (HappyAbsSyn30  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__14 (rtkPosOf happy_var_1) happy_var_2
        )
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  1# happyReduction_17
happyReduction_17 _
        (HappyAbsSyn32  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__15 (rtkPosOf happy_var_1) (reverse happy_var_2)
        )
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  1# happyReduction_18
happyReduction_18 _
        (HappyAbsSyn34  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__16 (rtkPosOf happy_var_1) happy_var_2
        )
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  1# happyReduction_19
happyReduction_19 _
        (HappyAbsSyn37  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__17 (rtkPosOf happy_var_1) happy_var_2
        )
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  1# happyReduction_20
happyReduction_20 _
        (HappyAbsSyn38  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__18 (rtkPosOf happy_var_1) happy_var_2
        )
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  1# happyReduction_21
happyReduction_21 _
        (HappyAbsSyn40  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__19 (rtkPosOf happy_var_1) (reverse happy_var_2)
        )
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  1# happyReduction_22
happyReduction_22 (HappyTerminal happy_var_1)
         =  HappyAbsSyn6
                 (Anti_Proto (tkVal_qq_Proto happy_var_1)
        )
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  1# happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn6
                 (Ctr__Proto__20 (rtkPosOf (reverse happy_var_1)) (reverse happy_var_1)
        )
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  2# happyReduction_24
happyReduction_24 (HappyTerminal happy_var_1)
         =  HappyAbsSyn7
                 (Anti_Alt (tkVal_qq_Alt happy_var_1)
        )
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  2# happyReduction_25
happyReduction_25 (HappyAbsSyn22  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn7
                 (Ctr__Alt__0 (rtkPosOf happy_var_1) happy_var_1 happy_var_3
        )
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  3# happyReduction_26
happyReduction_26 (HappyTerminal happy_var_1)
         =  HappyAbsSyn8
                 (Anti_Alt (tkVal_qq_AltList happy_var_1)
        )
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  3# happyReduction_27
happyReduction_27 (HappyAbsSyn7  happy_var_1)
         =  HappyAbsSyn8
                 (happy_var_1
        )
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  4# happyReduction_28
happyReduction_28 (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn9
                 ([happy_var_1]
        )
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  4# happyReduction_29
happyReduction_29 (HappyAbsSyn8  happy_var_3)
        _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_3 : happy_var_1
        )
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  5# happyReduction_30
happyReduction_30 (HappyTerminal happy_var_1)
         =  HappyAbsSyn10
                 (Anti_ConDef (tkVal_qq_ConDef happy_var_1)
        )
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  5# happyReduction_31
happyReduction_31 (HappyAbsSyn25  happy_var_2)
        (HappyAbsSyn13  happy_var_1)
         =  HappyAbsSyn10
                 (Ctr__ConDef__0 (rtkPosOf happy_var_1) happy_var_1 (reverse happy_var_2)
        )
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  6# happyReduction_32
happyReduction_32 (HappyTerminal happy_var_1)
         =  HappyAbsSyn11
                 (Anti_ConDef (tkVal_qq_ConDefList happy_var_1)
        )
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  6# happyReduction_33
happyReduction_33 (HappyAbsSyn10  happy_var_1)
         =  HappyAbsSyn11
                 (happy_var_1
        )
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  7# happyReduction_34
happyReduction_34 (HappyAbsSyn11  happy_var_1)
         =  HappyAbsSyn12
                 ([happy_var_1]
        )
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  7# happyReduction_35
happyReduction_35 (HappyAbsSyn11  happy_var_3)
        _
        (HappyAbsSyn12  happy_var_1)
         =  HappyAbsSyn12
                 (happy_var_3 : happy_var_1
        )
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  8# happyReduction_36
happyReduction_36 (HappyTerminal happy_var_1)
         =  HappyAbsSyn13
                 (Anti_ConId (tkVal_qq_ConId happy_var_1)
        )
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  8# happyReduction_37
happyReduction_37 (HappyTerminal happy_var_1)
         =  HappyAbsSyn13
                 (Ctr__ConId__0 (rtkPosOf happy_var_1) (tkVal_conid happy_var_1)
        )
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  9# happyReduction_38
happyReduction_38 (HappyTerminal happy_var_1)
         =  HappyAbsSyn14
                 (Anti_Decl (tkVal_qq_Decl happy_var_1)
        )
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happyReduce 6# 9# happyReduction_39
happyReduction_39 (_ `HappyStk`
        (HappyAbsSyn12  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn40  happy_var_3) `HappyStk`
        (HappyAbsSyn13  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (Ctr__Decl__0 (rtkPosOf happy_var_1) happy_var_2 (reverse happy_var_3) (reverse happy_var_5)
        ) `HappyStk` happyRest

happyReduce_40 = happyReduce 6# 9# happyReduction_40
happyReduction_40 (_ `HappyStk`
        (HappyAbsSyn22  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn26  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (Ctr__Decl__1 (rtkPosOf happy_var_1) happy_var_3 happy_var_5
        ) `HappyStk` happyRest

happyReduce_41 = happyReduce 7# 9# happyReduction_41
happyReduction_41 (_ `HappyStk`
        (HappyAbsSyn22  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn32  happy_var_4) `HappyStk`
        (HappyAbsSyn26  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (Ctr__Decl__2 (rtkPosOf happy_var_1) happy_var_3 (reverse happy_var_4) happy_var_6
        ) `HappyStk` happyRest

happyReduce_42 = happyReduce 5# 9# happyReduction_42
happyReduction_42 (_ `HappyStk`
        (HappyAbsSyn22  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn26  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (Ctr__Decl__3 (rtkPosOf happy_var_1) happy_var_2 happy_var_4
        ) `HappyStk` happyRest

happyReduce_43 = happyReduce 6# 9# happyReduction_43
happyReduction_43 (_ `HappyStk`
        (HappyAbsSyn22  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn32  happy_var_3) `HappyStk`
        (HappyAbsSyn26  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (Ctr__Decl__4 (rtkPosOf happy_var_1) happy_var_2 (reverse happy_var_3) happy_var_5
        ) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_2  9# happyReduction_44
happyReduction_44 _
        (HappyAbsSyn22  happy_var_1)
         =  HappyAbsSyn14
                 (Ctr__Decl__5 (rtkPosOf happy_var_1) happy_var_1
        )
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  10# happyReduction_45
happyReduction_45 (HappyTerminal happy_var_1)
         =  HappyAbsSyn15
                 (Anti_Decl (tkVal_qq_DeclList happy_var_1)
        )
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  10# happyReduction_46
happyReduction_46 (HappyAbsSyn14  happy_var_1)
         =  HappyAbsSyn15
                 (happy_var_1
        )
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_0  11# happyReduction_47
happyReduction_47  =  HappyAbsSyn16
                 ([]
        )

happyReduce_48 = happySpecReduce_2  11# happyReduction_48
happyReduction_48 (HappyAbsSyn15  happy_var_2)
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn16
                 (happy_var_2 : happy_var_1
        )
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  12# happyReduction_49
happyReduction_49 (HappyTerminal happy_var_1)
         =  HappyAbsSyn17
                 (Anti_Expr (tkVal_qq_Expr happy_var_1)
        )
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  12# happyReduction_50
happyReduction_50 (HappyTerminal happy_var_1)
         =  HappyAbsSyn17
                 (Ctr__Expr__0 (rtkPosOf happy_var_1) (tkVal_num happy_var_1)
        )
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  12# happyReduction_51
happyReduction_51 (HappyTerminal happy_var_1)
         =  HappyAbsSyn17
                 (Ctr__Expr__1 (rtkPosOf happy_var_1)
        )
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  12# happyReduction_52
happyReduction_52 (HappyTerminal happy_var_1)
         =  HappyAbsSyn17
                 (Ctr__Expr__2 (rtkPosOf happy_var_1)
        )
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  12# happyReduction_53
happyReduction_53 (HappyAbsSyn26  happy_var_1)
         =  HappyAbsSyn17
                 (Ctr__Expr__3 (rtkPosOf happy_var_1) happy_var_1
        )
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  12# happyReduction_54
happyReduction_54 (HappyAbsSyn13  happy_var_1)
         =  HappyAbsSyn17
                 (Ctr__Expr__4 (rtkPosOf happy_var_1) happy_var_1
        )
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  12# happyReduction_55
happyReduction_55 _
        (HappyAbsSyn22  happy_var_2)
        _
         =  HappyAbsSyn17
                 (happy_var_2
        )
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_2  13# happyReduction_56
happyReduction_56 (HappyAbsSyn17  happy_var_2)
        (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 (Ctr__Expr__6 (rtkPosOf happy_var_1) happy_var_1 happy_var_2
        )
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  13# happyReduction_57
happyReduction_57 (HappyAbsSyn17  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1
        )
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  14# happyReduction_58
happyReduction_58 (HappyAbsSyn18  happy_var_3)
        _
        (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn19
                 (Ctr__Expr__8 (rtkPosOf happy_var_1) happy_var_1 happy_var_3
        )
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  14# happyReduction_59
happyReduction_59 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn19
                 (happy_var_1
        )
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  15# happyReduction_60
happyReduction_60 (HappyAbsSyn19  happy_var_3)
        _
        (HappyAbsSyn20  happy_var_1)
         =  HappyAbsSyn20
                 (Ctr__Expr__10 (rtkPosOf happy_var_1) happy_var_1 happy_var_3
        )
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  15# happyReduction_61
happyReduction_61 (HappyAbsSyn19  happy_var_3)
        _
        (HappyAbsSyn20  happy_var_1)
         =  HappyAbsSyn20
                 (Ctr__Expr__11 (rtkPosOf happy_var_1) happy_var_1 happy_var_3
        )
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  15# happyReduction_62
happyReduction_62 (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn20
                 (happy_var_1
        )
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  16# happyReduction_63
happyReduction_63 (HappyAbsSyn20  happy_var_3)
        _
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn21
                 (Ctr__Expr__13 (rtkPosOf happy_var_1) happy_var_1 happy_var_3
        )
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  16# happyReduction_64
happyReduction_64 (HappyAbsSyn20  happy_var_1)
         =  HappyAbsSyn21
                 (happy_var_1
        )
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happyReduce 4# 17# happyReduction_65
happyReduction_65 ((HappyAbsSyn22  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn32  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn22
                 (Ctr__Expr__15 (rtkPosOf happy_var_1) (reverse happy_var_2) happy_var_4
        ) `HappyStk` happyRest

happyReduce_66 = happyReduce 7# 17# happyReduction_66
happyReduction_66 ((HappyAbsSyn22  happy_var_7) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn22  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn26  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn22
                 (Ctr__Expr__16 (rtkPosOf happy_var_1) happy_var_3 happy_var_5 happy_var_7
        ) `HappyStk` happyRest

happyReduce_67 = happyReduce 6# 17# happyReduction_67
happyReduction_67 ((HappyAbsSyn22  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn22  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn26  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn22
                 (Ctr__Expr__17 (rtkPosOf happy_var_1) happy_var_2 happy_var_4 happy_var_6
        ) `HappyStk` happyRest

happyReduce_68 = happyReduce 6# 17# happyReduction_68
happyReduction_68 ((HappyAbsSyn22  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn22  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn22  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn22
                 (Ctr__Expr__18 (rtkPosOf happy_var_1) happy_var_2 happy_var_4 happy_var_6
        ) `HappyStk` happyRest

happyReduce_69 = happySpecReduce_2  17# happyReduction_69
happyReduction_69 (HappyAbsSyn22  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn22
                 (Ctr__Expr__19 (rtkPosOf happy_var_1) happy_var_2
        )
happyReduction_69 _ _  = notHappyAtAll 

happyReduce_70 = happyReduce 6# 17# happyReduction_70
happyReduction_70 (_ `HappyStk`
        (HappyAbsSyn9  happy_var_5) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn22  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn22
                 (Ctr__Expr__20 (rtkPosOf happy_var_1) happy_var_2 (reverse happy_var_5)
        ) `HappyStk` happyRest

happyReduce_71 = happySpecReduce_1  17# happyReduction_71
happyReduction_71 (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn22
                 (happy_var_1
        )
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  18# happyReduction_72
happyReduction_72 (HappyTerminal happy_var_1)
         =  HappyAbsSyn23
                 (Anti_Field (tkVal_qq_Field happy_var_1)
        )
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  18# happyReduction_73
happyReduction_73 (HappyAbsSyn35  happy_var_1)
         =  HappyAbsSyn23
                 (Ctr__Field__0 (rtkPosOf happy_var_1) happy_var_1
        )
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  19# happyReduction_74
happyReduction_74 (HappyTerminal happy_var_1)
         =  HappyAbsSyn24
                 (Anti_Field (tkVal_qq_FieldList happy_var_1)
        )
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  19# happyReduction_75
happyReduction_75 (HappyAbsSyn23  happy_var_1)
         =  HappyAbsSyn24
                 (happy_var_1
        )
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_0  20# happyReduction_76
happyReduction_76  =  HappyAbsSyn25
                 ([]
        )

happyReduce_77 = happySpecReduce_2  20# happyReduction_77
happyReduction_77 (HappyAbsSyn24  happy_var_2)
        (HappyAbsSyn25  happy_var_1)
         =  HappyAbsSyn25
                 (happy_var_2 : happy_var_1
        )
happyReduction_77 _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  21# happyReduction_78
happyReduction_78 (HappyTerminal happy_var_1)
         =  HappyAbsSyn26
                 (Anti_Id (tkVal_qq_Id happy_var_1)
        )
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  21# happyReduction_79
happyReduction_79 (HappyTerminal happy_var_1)
         =  HappyAbsSyn26
                 (Ctr__Id__0 (rtkPosOf happy_var_1) (tkVal_id happy_var_1)
        )
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  22# happyReduction_80
happyReduction_80 (HappyTerminal happy_var_1)
         =  HappyAbsSyn27
                 (Anti_PArg (tkVal_qq_PArg happy_var_1)
        )
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  22# happyReduction_81
happyReduction_81 (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn27
                 (Ctr__PArg__0 (rtkPosOf happy_var_1) happy_var_1
        )
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  23# happyReduction_82
happyReduction_82 (HappyTerminal happy_var_1)
         =  HappyAbsSyn28
                 (Anti_PArg (tkVal_qq_PArgs happy_var_1)
        )
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  23# happyReduction_83
happyReduction_83 (HappyAbsSyn27  happy_var_1)
         =  HappyAbsSyn28
                 (happy_var_1
        )
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  24# happyReduction_84
happyReduction_84 (HappyAbsSyn28  happy_var_1)
         =  HappyAbsSyn29
                 ([happy_var_1]
        )
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_2  24# happyReduction_85
happyReduction_85 (HappyAbsSyn28  happy_var_2)
        (HappyAbsSyn29  happy_var_1)
         =  HappyAbsSyn29
                 (happy_var_2 : happy_var_1
        )
happyReduction_85 _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  25# happyReduction_86
happyReduction_86 (HappyTerminal happy_var_1)
         =  HappyAbsSyn30
                 (Anti_Param (tkVal_qq_Param happy_var_1)
        )
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  25# happyReduction_87
happyReduction_87 (HappyAbsSyn26  happy_var_1)
         =  HappyAbsSyn30
                 (Ctr__Param__0 (rtkPosOf happy_var_1) happy_var_1
        )
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  26# happyReduction_88
happyReduction_88 (HappyTerminal happy_var_1)
         =  HappyAbsSyn31
                 (Anti_Param (tkVal_qq_Params happy_var_1)
        )
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_1  26# happyReduction_89
happyReduction_89 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn31
                 (happy_var_1
        )
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  27# happyReduction_90
happyReduction_90 (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn32
                 ([happy_var_1]
        )
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_2  27# happyReduction_91
happyReduction_91 (HappyAbsSyn31  happy_var_2)
        (HappyAbsSyn32  happy_var_1)
         =  HappyAbsSyn32
                 (happy_var_2 : happy_var_1
        )
happyReduction_91 _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  28# happyReduction_92
happyReduction_92 (HappyTerminal happy_var_1)
         =  HappyAbsSyn33
                 (Anti_Pat (tkVal_qq_Pat happy_var_1)
        )
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  28# happyReduction_93
happyReduction_93 (HappyTerminal happy_var_1)
         =  HappyAbsSyn33
                 (Ctr__Pat__0 (rtkPosOf happy_var_1) (tkVal_num happy_var_1)
        )
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  28# happyReduction_94
happyReduction_94 (HappyTerminal happy_var_1)
         =  HappyAbsSyn33
                 (Ctr__Pat__1 (rtkPosOf happy_var_1)
        )
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_1  28# happyReduction_95
happyReduction_95 (HappyTerminal happy_var_1)
         =  HappyAbsSyn33
                 (Ctr__Pat__2 (rtkPosOf happy_var_1)
        )
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  28# happyReduction_96
happyReduction_96 (HappyTerminal happy_var_1)
         =  HappyAbsSyn33
                 (Ctr__Pat__3 (rtkPosOf happy_var_1)
        )
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_1  28# happyReduction_97
happyReduction_97 (HappyAbsSyn26  happy_var_1)
         =  HappyAbsSyn33
                 (Ctr__Pat__4 (rtkPosOf happy_var_1) happy_var_1
        )
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_1  28# happyReduction_98
happyReduction_98 (HappyAbsSyn13  happy_var_1)
         =  HappyAbsSyn33
                 (Ctr__Pat__5 (rtkPosOf happy_var_1) happy_var_1
        )
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_3  28# happyReduction_99
happyReduction_99 _
        (HappyAbsSyn34  happy_var_2)
        _
         =  HappyAbsSyn33
                 (happy_var_2
        )
happyReduction_99 _ _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_2  29# happyReduction_100
happyReduction_100 (HappyAbsSyn29  happy_var_2)
        (HappyAbsSyn13  happy_var_1)
         =  HappyAbsSyn34
                 (Ctr__Pat__7 (rtkPosOf happy_var_1) happy_var_1 (reverse happy_var_2)
        )
happyReduction_100 _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_1  29# happyReduction_101
happyReduction_101 (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_101 _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  30# happyReduction_102
happyReduction_102 (HappyTerminal happy_var_1)
         =  HappyAbsSyn35
                 (Anti_Ty (tkVal_qq_Ty happy_var_1)
        )
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_1  30# happyReduction_103
happyReduction_103 (HappyAbsSyn13  happy_var_1)
         =  HappyAbsSyn35
                 (Ctr__Ty__0 (rtkPosOf happy_var_1) happy_var_1
        )
happyReduction_103 _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_1  30# happyReduction_104
happyReduction_104 (HappyAbsSyn26  happy_var_1)
         =  HappyAbsSyn35
                 (Ctr__Ty__1 (rtkPosOf happy_var_1) happy_var_1
        )
happyReduction_104 _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_3  30# happyReduction_105
happyReduction_105 _
        (HappyAbsSyn37  happy_var_2)
        _
         =  HappyAbsSyn35
                 (happy_var_2
        )
happyReduction_105 _ _ _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_2  31# happyReduction_106
happyReduction_106 (HappyAbsSyn35  happy_var_2)
        (HappyAbsSyn36  happy_var_1)
         =  HappyAbsSyn36
                 (Ctr__Ty__3 (rtkPosOf happy_var_1) happy_var_1 happy_var_2
        )
happyReduction_106 _ _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_1  31# happyReduction_107
happyReduction_107 (HappyAbsSyn35  happy_var_1)
         =  HappyAbsSyn36
                 (happy_var_1
        )
happyReduction_107 _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_3  32# happyReduction_108
happyReduction_108 (HappyAbsSyn37  happy_var_3)
        _
        (HappyAbsSyn36  happy_var_1)
         =  HappyAbsSyn37
                 (Ctr__Ty__5 (rtkPosOf happy_var_1) happy_var_1 happy_var_3
        )
happyReduction_108 _ _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_1  32# happyReduction_109
happyReduction_109 (HappyAbsSyn36  happy_var_1)
         =  HappyAbsSyn37
                 (happy_var_1
        )
happyReduction_109 _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_1  33# happyReduction_110
happyReduction_110 (HappyTerminal happy_var_1)
         =  HappyAbsSyn38
                 (Anti_TyVar (tkVal_qq_TyVar happy_var_1)
        )
happyReduction_110 _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  33# happyReduction_111
happyReduction_111 (HappyAbsSyn26  happy_var_1)
         =  HappyAbsSyn38
                 (Ctr__TyVar__0 (rtkPosOf happy_var_1) happy_var_1
        )
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_1  34# happyReduction_112
happyReduction_112 (HappyTerminal happy_var_1)
         =  HappyAbsSyn39
                 (Anti_TyVar (tkVal_qq_TyVarList happy_var_1)
        )
happyReduction_112 _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_1  34# happyReduction_113
happyReduction_113 (HappyAbsSyn38  happy_var_1)
         =  HappyAbsSyn39
                 (happy_var_1
        )
happyReduction_113 _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_0  35# happyReduction_114
happyReduction_114  =  HappyAbsSyn40
                 ([]
        )

happyReduce_115 = happySpecReduce_2  35# happyReduction_115
happyReduction_115 (HappyAbsSyn39  happy_var_2)
        (HappyAbsSyn40  happy_var_1)
         =  HappyAbsSyn40
                 (happy_var_2 : happy_var_1
        )
happyReduction_115 _ _  = notHappyAtAll 

happyTerminalToTok term = case term of {
        L.PosToken _ L.EndOfFile -> 2#;
        L.PosToken _ L.Tk__tok_Alt_dummy_25 -> 3#;
        L.PosToken _ L.Tk__tok_AltList_dummy_24 -> 4#;
        L.PosToken _ L.Tk__tok_ConDef_dummy_23 -> 5#;
        L.PosToken _ L.Tk__tok_ConDefList_dummy_22 -> 6#;
        L.PosToken _ L.Tk__tok_ConId_dummy_21 -> 7#;
        L.PosToken _ L.Tk__tok_Decl_dummy_20 -> 8#;
        L.PosToken _ L.Tk__tok_DeclList_dummy_19 -> 9#;
        L.PosToken _ L.Tk__tok_Expr_dummy_18 -> 10#;
        L.PosToken _ L.Tk__tok_Field_dummy_17 -> 11#;
        L.PosToken _ L.Tk__tok_FieldList_dummy_16 -> 12#;
        L.PosToken _ L.Tk__tok_Id_dummy_15 -> 13#;
        L.PosToken _ L.Tk__tok_PArg_dummy_14 -> 14#;
        L.PosToken _ L.Tk__tok_PArgs_dummy_13 -> 15#;
        L.PosToken _ L.Tk__tok_Param_dummy_12 -> 16#;
        L.PosToken _ L.Tk__tok_Params_dummy_11 -> 17#;
        L.PosToken _ L.Tk__tok_Pat_dummy_10 -> 18#;
        L.PosToken _ L.Tk__tok_Proto_dummy_26 -> 19#;
        L.PosToken _ L.Tk__tok_Ty_dummy_9 -> 20#;
        L.PosToken _ L.Tk__tok_TyVar_dummy_8 -> 21#;
        L.PosToken _ L.Tk__tok_TyVarList_dummy_7 -> 22#;
        L.PosToken _ L.Tk__tok__symbol__18 -> 23#;
        L.PosToken _ L.Tk__tok__pipe__5 -> 24#;
        L.PosToken _ L.Tk__tok__symbol__17 -> 25#;
        L.PosToken _ L.Tk__tok_true_20 -> 26#;
        L.PosToken _ L.Tk__tok_then_12 -> 27#;
        L.PosToken _ L.Tk__tok_rec_4 -> 28#;
        L.PosToken _ L.Tk__tok_of_16 -> 29#;
        L.PosToken _ L.Tk__tok_let_3 -> 30#;
        L.PosToken _ L.Tk__tok_in_10 -> 31#;
        L.PosToken _ L.Tk__tok_if_11 -> 32#;
        L.PosToken _ L.Tk__tok_fix_14 -> 33#;
        L.PosToken _ L.Tk__tok_false_21 -> 34#;
        L.PosToken _ L.Tk__tok_else_13 -> 35#;
        L.PosToken _ L.Tk__tok_data_0 -> 36#;
        L.PosToken _ L.Tk__tok_case_15 -> 37#;
        L.PosToken _ L.Tk__tok__symbol__19 -> 38#;
        L.PosToken _ L.Tk__tok__symbol__9 -> 39#;
        L.PosToken _ L.Tk__tok__eql__eql__22 -> 40#;
        L.PosToken _ L.Tk__tok__eql__1 -> 41#;
        L.PosToken _ L.Tk__tok__semi__2 -> 42#;
        L.PosToken _ L.Tk__tok__minus__symbol__6 -> 43#;
        L.PosToken _ L.Tk__tok__minus__24 -> 44#;
        L.PosToken _ L.Tk__tok__plus__23 -> 45#;
        L.PosToken _ L.Tk__tok__star__25 -> 46#;
        L.PosToken _ L.Tk__tok__rparen__8 -> 47#;
        L.PosToken _ L.Tk__tok__lparen__7 -> 48#;
        L.PosToken _ (L.Tk__conid _) -> 49#;
        L.PosToken _ (L.Tk__id _) -> 50#;
        L.PosToken _ (L.Tk__num _) -> 51#;
        L.PosToken _ (L.Tk__qq_Id _) -> 52#;
        L.PosToken _ (L.Tk__qq_ConId _) -> 53#;
        L.PosToken _ (L.Tk__qq_Param _) -> 54#;
        L.PosToken _ (L.Tk__qq_Params _) -> 55#;
        L.PosToken _ (L.Tk__qq_Expr _) -> 56#;
        L.PosToken _ (L.Tk__qq_PArg _) -> 57#;
        L.PosToken _ (L.Tk__qq_PArgs _) -> 58#;
        L.PosToken _ (L.Tk__qq_Pat _) -> 59#;
        L.PosToken _ (L.Tk__qq_Alt _) -> 60#;
        L.PosToken _ (L.Tk__qq_AltList _) -> 61#;
        L.PosToken _ (L.Tk__qq_Ty _) -> 62#;
        L.PosToken _ (L.Tk__qq_Field _) -> 63#;
        L.PosToken _ (L.Tk__qq_FieldList _) -> 64#;
        L.PosToken _ (L.Tk__qq_ConDef _) -> 65#;
        L.PosToken _ (L.Tk__qq_ConDefList _) -> 66#;
        L.PosToken _ (L.Tk__qq_TyVar _) -> 67#;
        L.PosToken _ (L.Tk__qq_TyVarList _) -> 68#;
        L.PosToken _ (L.Tk__qq_Decl _) -> 69#;
        L.PosToken _ (L.Tk__qq_DeclList _) -> 70#;
        L.PosToken _ (L.Tk__qq_Proto _) -> 71#;
        _ -> -1#;
        }
{-# NOINLINE happyTerminalToTok #-}

happyLex kend  _kmore []       = kend notHappyAtAll []
happyLex _kend kmore  (tk:tks) = kmore (happyTerminalToTok tk) tk tks
{-# INLINE happyLex #-}

happyNewToken action sts stk = happyLex (\tk -> happyDoAction 72# notHappyAtAll action sts stk) (\i tk -> happyDoAction i tk action sts stk)

happyReport 72# tk explist resume tks = happyReport' tks explist resume
happyReport _ tk explist resume tks = happyReport' (tk:tks) explist (\tks -> resume (Happy_Prelude.tail tks))


happyThen :: () => (Either String a) -> (a -> (Either String b)) -> (Either String b)
happyThen = (Happy_Prelude.>>=)
happyReturn :: () => a -> (Either String a)
happyReturn = (Happy_Prelude.return)
happyThen1 m k tks = (Happy_Prelude.>>=) m (\a -> k a tks)
happyFmap1 f m tks = happyThen (m tks) (\a -> happyReturn (f a))
happyReturn1 :: () => a -> b -> (Either String a)
happyReturn1 = \a tks -> (Happy_Prelude.return) a
happyReport' :: () => [(L.PosToken)] -> [Happy_Prelude.String] -> ([(L.PosToken)] -> (Either String a)) -> (Either String a)
happyReport' = (\tokens expected resume -> (parseError) tokens)

happyAbort :: () => [(L.PosToken)] -> (Either String a)
happyAbort = Happy_Prelude.error "Called abort handler in non-resumptive parser"

parseProto tks = happySomeParser where
 happySomeParser = happyThen (happyDoParse 0# tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [L.PosToken] -> Either String a
parseError [] = Left "unexpected end of input"
parseError (L.PosToken (L.AlexPn _ line col) tok : _) =
    Left $ show line ++ ":" ++ show col ++ ":unexpected " ++ showRtkToken tok

-- Render a token the way it appears in the source, for error messages
showRtkToken :: L.Token -> String
showRtkToken L.EndOfFile = "end of input"
showRtkToken L.Tk__tok_Alt_dummy_25 = "'tok_Alt_dummy_25'"
showRtkToken L.Tk__tok_AltList_dummy_24 = "'tok_AltList_dummy_24'"
showRtkToken L.Tk__tok_ConDef_dummy_23 = "'tok_ConDef_dummy_23'"
showRtkToken L.Tk__tok_ConDefList_dummy_22 = "'tok_ConDefList_dummy_22'"
showRtkToken L.Tk__tok_ConId_dummy_21 = "'tok_ConId_dummy_21'"
showRtkToken L.Tk__tok_Decl_dummy_20 = "'tok_Decl_dummy_20'"
showRtkToken L.Tk__tok_DeclList_dummy_19 = "'tok_DeclList_dummy_19'"
showRtkToken L.Tk__tok_Expr_dummy_18 = "'tok_Expr_dummy_18'"
showRtkToken L.Tk__tok_Field_dummy_17 = "'tok_Field_dummy_17'"
showRtkToken L.Tk__tok_FieldList_dummy_16 = "'tok_FieldList_dummy_16'"
showRtkToken L.Tk__tok_Id_dummy_15 = "'tok_Id_dummy_15'"
showRtkToken L.Tk__tok_PArg_dummy_14 = "'tok_PArg_dummy_14'"
showRtkToken L.Tk__tok_PArgs_dummy_13 = "'tok_PArgs_dummy_13'"
showRtkToken L.Tk__tok_Param_dummy_12 = "'tok_Param_dummy_12'"
showRtkToken L.Tk__tok_Params_dummy_11 = "'tok_Params_dummy_11'"
showRtkToken L.Tk__tok_Pat_dummy_10 = "'tok_Pat_dummy_10'"
showRtkToken L.Tk__tok_Proto_dummy_26 = "'tok_Proto_dummy_26'"
showRtkToken L.Tk__tok_Ty_dummy_9 = "'tok_Ty_dummy_9'"
showRtkToken L.Tk__tok_TyVar_dummy_8 = "'tok_TyVar_dummy_8'"
showRtkToken L.Tk__tok_TyVarList_dummy_7 = "'tok_TyVarList_dummy_7'"
showRtkToken L.Tk__tok__symbol__18 = "'}'"
showRtkToken L.Tk__tok__pipe__5 = "'|'"
showRtkToken L.Tk__tok__symbol__17 = "'{'"
showRtkToken L.Tk__tok_true_20 = "'true'"
showRtkToken L.Tk__tok_then_12 = "'then'"
showRtkToken L.Tk__tok_rec_4 = "'rec'"
showRtkToken L.Tk__tok_of_16 = "'of'"
showRtkToken L.Tk__tok_let_3 = "'let'"
showRtkToken L.Tk__tok_in_10 = "'in'"
showRtkToken L.Tk__tok_if_11 = "'if'"
showRtkToken L.Tk__tok_fix_14 = "'fix'"
showRtkToken L.Tk__tok_false_21 = "'false'"
showRtkToken L.Tk__tok_else_13 = "'else'"
showRtkToken L.Tk__tok_data_0 = "'data'"
showRtkToken L.Tk__tok_case_15 = "'case'"
showRtkToken L.Tk__tok__symbol__19 = "'_'"
showRtkToken L.Tk__tok__symbol__9 = "'\\'"
showRtkToken L.Tk__tok__eql__eql__22 = "'=='"
showRtkToken L.Tk__tok__eql__1 = "'='"
showRtkToken L.Tk__tok__semi__2 = "';'"
showRtkToken L.Tk__tok__minus__symbol__6 = "'->'"
showRtkToken L.Tk__tok__minus__24 = "'-'"
showRtkToken L.Tk__tok__plus__23 = "'+'"
showRtkToken L.Tk__tok__star__25 = "'*'"
showRtkToken L.Tk__tok__rparen__8 = "')'"
showRtkToken L.Tk__tok__lparen__7 = "'('"
showRtkToken (L.Tk__conid v) = "conid " ++ show v
showRtkToken (L.Tk__id v) = "id " ++ show v
showRtkToken (L.Tk__num v) = "num " ++ show v
showRtkToken (L.Tk__qq_Id v) = "qq_Id " ++ show v
showRtkToken (L.Tk__qq_ConId v) = "qq_ConId " ++ show v
showRtkToken (L.Tk__qq_Param v) = "qq_Param " ++ show v
showRtkToken (L.Tk__qq_Params v) = "qq_Params " ++ show v
showRtkToken (L.Tk__qq_Expr v) = "qq_Expr " ++ show v
showRtkToken (L.Tk__qq_PArg v) = "qq_PArg " ++ show v
showRtkToken (L.Tk__qq_PArgs v) = "qq_PArgs " ++ show v
showRtkToken (L.Tk__qq_Pat v) = "qq_Pat " ++ show v
showRtkToken (L.Tk__qq_Alt v) = "qq_Alt " ++ show v
showRtkToken (L.Tk__qq_AltList v) = "qq_AltList " ++ show v
showRtkToken (L.Tk__qq_Ty v) = "qq_Ty " ++ show v
showRtkToken (L.Tk__qq_Field v) = "qq_Field " ++ show v
showRtkToken (L.Tk__qq_FieldList v) = "qq_FieldList " ++ show v
showRtkToken (L.Tk__qq_ConDef v) = "qq_ConDef " ++ show v
showRtkToken (L.Tk__qq_ConDefList v) = "qq_ConDefList " ++ show v
showRtkToken (L.Tk__qq_TyVar v) = "qq_TyVar " ++ show v
showRtkToken (L.Tk__qq_TyVarList v) = "qq_TyVarList " ++ show v
showRtkToken (L.Tk__qq_Decl v) = "qq_Decl " ++ show v
showRtkToken (L.Tk__qq_DeclList v) = "qq_DeclList " ++ show v
showRtkToken (L.Tk__qq_Proto v) = "qq_Proto " ++ show v

-- Source position of a node: every constructor except the Anti_* splice
-- artifacts stores the position of its alternative's first symbol in its
-- first field. Positions are transparent for equality and ordering, so two
-- ASTs that differ only in source positions (e.g. a quasi-quote parsed at
-- compile time vs the same construct parsed at run time) compare equal.
newtype RtkPos = RtkPos L.AlexPosn deriving (Show, Gen.Data, Gen.Typeable)
instance Eq RtkPos where _ == _ = True
instance Ord RtkPos where compare _ _ = EQ

-- The position used where no source token exists: empty productions, empty
-- lists, absent optionals and Anti_* quasi-quote splices
rtkNoPos :: RtkPos
rtkNoPos = RtkPos (L.AlexPn 0 0 0)

class RtkPosOf a where
    rtkPosOf :: a -> RtkPos
instance RtkPosOf L.PosToken where
    rtkPosOf (L.PosToken p _) = RtkPos p
instance RtkPosOf a => RtkPosOf [a] where
    rtkPosOf (x : _) = rtkPosOf x
    rtkPosOf []      = rtkNoPos
instance RtkPosOf a => RtkPosOf (Maybe a) where
    rtkPosOf (Just x) = rtkPosOf x
    rtkPosOf Nothing  = rtkNoPos
-- A Char carries no position; this also covers String token payloads
instance RtkPosOf Char where
    rtkPosOf _ = rtkNoPos
instance RtkPosOf Int where rtkPosOf _ = rtkNoPos

-- Recover a token's payload from the whole positioned token: %token
-- bindings keep the L.PosToken so semantic actions can read its position
tkVal_conid :: L.PosToken -> String
tkVal_conid (L.PosToken _ (L.Tk__conid v)) = v
tkVal_conid t = error ("rtk internal error: token conid expected, got " ++ showRtkToken (L.ptToken t))
tkVal_id :: L.PosToken -> String
tkVal_id (L.PosToken _ (L.Tk__id v)) = v
tkVal_id t = error ("rtk internal error: token id expected, got " ++ showRtkToken (L.ptToken t))
tkVal_num :: L.PosToken -> Int
tkVal_num (L.PosToken _ (L.Tk__num v)) = v
tkVal_num t = error ("rtk internal error: token num expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Id :: L.PosToken -> String
tkVal_qq_Id (L.PosToken _ (L.Tk__qq_Id v)) = v
tkVal_qq_Id t = error ("rtk internal error: token qq_Id expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ConId :: L.PosToken -> String
tkVal_qq_ConId (L.PosToken _ (L.Tk__qq_ConId v)) = v
tkVal_qq_ConId t = error ("rtk internal error: token qq_ConId expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Param :: L.PosToken -> String
tkVal_qq_Param (L.PosToken _ (L.Tk__qq_Param v)) = v
tkVal_qq_Param t = error ("rtk internal error: token qq_Param expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Params :: L.PosToken -> String
tkVal_qq_Params (L.PosToken _ (L.Tk__qq_Params v)) = v
tkVal_qq_Params t = error ("rtk internal error: token qq_Params expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Expr :: L.PosToken -> String
tkVal_qq_Expr (L.PosToken _ (L.Tk__qq_Expr v)) = v
tkVal_qq_Expr t = error ("rtk internal error: token qq_Expr expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_PArg :: L.PosToken -> String
tkVal_qq_PArg (L.PosToken _ (L.Tk__qq_PArg v)) = v
tkVal_qq_PArg t = error ("rtk internal error: token qq_PArg expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_PArgs :: L.PosToken -> String
tkVal_qq_PArgs (L.PosToken _ (L.Tk__qq_PArgs v)) = v
tkVal_qq_PArgs t = error ("rtk internal error: token qq_PArgs expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Pat :: L.PosToken -> String
tkVal_qq_Pat (L.PosToken _ (L.Tk__qq_Pat v)) = v
tkVal_qq_Pat t = error ("rtk internal error: token qq_Pat expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Alt :: L.PosToken -> String
tkVal_qq_Alt (L.PosToken _ (L.Tk__qq_Alt v)) = v
tkVal_qq_Alt t = error ("rtk internal error: token qq_Alt expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_AltList :: L.PosToken -> String
tkVal_qq_AltList (L.PosToken _ (L.Tk__qq_AltList v)) = v
tkVal_qq_AltList t = error ("rtk internal error: token qq_AltList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Ty :: L.PosToken -> String
tkVal_qq_Ty (L.PosToken _ (L.Tk__qq_Ty v)) = v
tkVal_qq_Ty t = error ("rtk internal error: token qq_Ty expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Field :: L.PosToken -> String
tkVal_qq_Field (L.PosToken _ (L.Tk__qq_Field v)) = v
tkVal_qq_Field t = error ("rtk internal error: token qq_Field expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_FieldList :: L.PosToken -> String
tkVal_qq_FieldList (L.PosToken _ (L.Tk__qq_FieldList v)) = v
tkVal_qq_FieldList t = error ("rtk internal error: token qq_FieldList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ConDef :: L.PosToken -> String
tkVal_qq_ConDef (L.PosToken _ (L.Tk__qq_ConDef v)) = v
tkVal_qq_ConDef t = error ("rtk internal error: token qq_ConDef expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ConDefList :: L.PosToken -> String
tkVal_qq_ConDefList (L.PosToken _ (L.Tk__qq_ConDefList v)) = v
tkVal_qq_ConDefList t = error ("rtk internal error: token qq_ConDefList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TyVar :: L.PosToken -> String
tkVal_qq_TyVar (L.PosToken _ (L.Tk__qq_TyVar v)) = v
tkVal_qq_TyVar t = error ("rtk internal error: token qq_TyVar expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TyVarList :: L.PosToken -> String
tkVal_qq_TyVarList (L.PosToken _ (L.Tk__qq_TyVarList v)) = v
tkVal_qq_TyVarList t = error ("rtk internal error: token qq_TyVarList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Decl :: L.PosToken -> String
tkVal_qq_Decl (L.PosToken _ (L.Tk__qq_Decl v)) = v
tkVal_qq_Decl t = error ("rtk internal error: token qq_Decl expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_DeclList :: L.PosToken -> String
tkVal_qq_DeclList (L.PosToken _ (L.Tk__qq_DeclList v)) = v
tkVal_qq_DeclList t = error ("rtk internal error: token qq_DeclList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Proto :: L.PosToken -> String
tkVal_qq_Proto (L.PosToken _ (L.Tk__qq_Proto v)) = v
tkVal_qq_Proto t = error ("rtk internal error: token qq_Proto expected, got " ++ showRtkToken (L.ptToken t))

data Proto = Ctr__Proto__0 RtkPos Proto |
             Ctr__Proto__1 RtkPos Alt |
             Ctr__Proto__2 RtkPos AltList |
             Ctr__Proto__3 RtkPos ConDef |
             Ctr__Proto__4 RtkPos ConDefList |
             Ctr__Proto__5 RtkPos ConId |
             Ctr__Proto__6 RtkPos Decl |
             Ctr__Proto__7 RtkPos DeclList |
             Ctr__Proto__8 RtkPos Expr |
             Ctr__Proto__9 RtkPos Field |
             Ctr__Proto__10 RtkPos FieldList |
             Ctr__Proto__11 RtkPos Id |
             Ctr__Proto__12 RtkPos PArg |
             Ctr__Proto__13 RtkPos PArgs |
             Ctr__Proto__14 RtkPos Param |
             Ctr__Proto__15 RtkPos Params |
             Ctr__Proto__16 RtkPos Pat |
             Ctr__Proto__17 RtkPos Ty |
             Ctr__Proto__18 RtkPos TyVar |
             Ctr__Proto__19 RtkPos TyVarList |
             Anti_Proto String |
             Ctr__Proto__20 RtkPos DeclList
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Proto where
    rtkPosOf (Ctr__Proto__0 p _) = p
    rtkPosOf (Ctr__Proto__1 p _) = p
    rtkPosOf (Ctr__Proto__2 p _) = p
    rtkPosOf (Ctr__Proto__3 p _) = p
    rtkPosOf (Ctr__Proto__4 p _) = p
    rtkPosOf (Ctr__Proto__5 p _) = p
    rtkPosOf (Ctr__Proto__6 p _) = p
    rtkPosOf (Ctr__Proto__7 p _) = p
    rtkPosOf (Ctr__Proto__8 p _) = p
    rtkPosOf (Ctr__Proto__9 p _) = p
    rtkPosOf (Ctr__Proto__10 p _) = p
    rtkPosOf (Ctr__Proto__11 p _) = p
    rtkPosOf (Ctr__Proto__12 p _) = p
    rtkPosOf (Ctr__Proto__13 p _) = p
    rtkPosOf (Ctr__Proto__14 p _) = p
    rtkPosOf (Ctr__Proto__15 p _) = p
    rtkPosOf (Ctr__Proto__16 p _) = p
    rtkPosOf (Ctr__Proto__17 p _) = p
    rtkPosOf (Ctr__Proto__18 p _) = p
    rtkPosOf (Ctr__Proto__19 p _) = p
    rtkPosOf (Anti_Proto _) = rtkNoPos
    rtkPosOf (Ctr__Proto__20 p _) = p
data Alt = Anti_Alt String |
           Ctr__Alt__0 RtkPos Pat Expr
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Alt where
    rtkPosOf (Anti_Alt _) = rtkNoPos
    rtkPosOf (Ctr__Alt__0 p _ _) = p
type AltList = [Alt]
data ConDef = Anti_ConDef String |
              Ctr__ConDef__0 RtkPos ConId FieldList
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ConDef where
    rtkPosOf (Anti_ConDef _) = rtkNoPos
    rtkPosOf (Ctr__ConDef__0 p _ _) = p
type ConDefList = [ConDef]
data ConId = Anti_ConId String |
             Ctr__ConId__0 RtkPos String
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ConId where
    rtkPosOf (Anti_ConId _) = rtkNoPos
    rtkPosOf (Ctr__ConId__0 p _) = p
data Decl = Anti_Decl String |
            Ctr__Decl__0 RtkPos ConId TyVarList ConDefList |
            Ctr__Decl__1 RtkPos Id Expr |
            Ctr__Decl__2 RtkPos Id Params Expr |
            Ctr__Decl__3 RtkPos Id Expr |
            Ctr__Decl__4 RtkPos Id Params Expr |
            Ctr__Decl__5 RtkPos Expr
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Decl where
    rtkPosOf (Anti_Decl _) = rtkNoPos
    rtkPosOf (Ctr__Decl__0 p _ _ _) = p
    rtkPosOf (Ctr__Decl__1 p _ _) = p
    rtkPosOf (Ctr__Decl__2 p _ _ _) = p
    rtkPosOf (Ctr__Decl__3 p _ _) = p
    rtkPosOf (Ctr__Decl__4 p _ _ _) = p
    rtkPosOf (Ctr__Decl__5 p _) = p
type DeclList = [Decl]
data Expr = Anti_Expr String |
            Ctr__Expr__0 RtkPos Int |
            Ctr__Expr__1 RtkPos |
            Ctr__Expr__2 RtkPos |
            Ctr__Expr__3 RtkPos Id |
            Ctr__Expr__4 RtkPos ConId |
            Ctr__Expr__6 RtkPos Expr Expr |
            Ctr__Expr__8 RtkPos Expr Expr |
            Ctr__Expr__10 RtkPos Expr Expr |
            Ctr__Expr__11 RtkPos Expr Expr |
            Ctr__Expr__13 RtkPos Expr Expr |
            Ctr__Expr__15 RtkPos Params Expr |
            Ctr__Expr__16 RtkPos Id Expr Expr |
            Ctr__Expr__17 RtkPos Id Expr Expr |
            Ctr__Expr__18 RtkPos Expr Expr Expr |
            Ctr__Expr__19 RtkPos Expr |
            Ctr__Expr__20 RtkPos Expr AltList
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Expr where
    rtkPosOf (Anti_Expr _) = rtkNoPos
    rtkPosOf (Ctr__Expr__0 p _) = p
    rtkPosOf (Ctr__Expr__1 p) = p
    rtkPosOf (Ctr__Expr__2 p) = p
    rtkPosOf (Ctr__Expr__3 p _) = p
    rtkPosOf (Ctr__Expr__4 p _) = p
    rtkPosOf (Ctr__Expr__6 p _ _) = p
    rtkPosOf (Ctr__Expr__8 p _ _) = p
    rtkPosOf (Ctr__Expr__10 p _ _) = p
    rtkPosOf (Ctr__Expr__11 p _ _) = p
    rtkPosOf (Ctr__Expr__13 p _ _) = p
    rtkPosOf (Ctr__Expr__15 p _ _) = p
    rtkPosOf (Ctr__Expr__16 p _ _ _) = p
    rtkPosOf (Ctr__Expr__17 p _ _ _) = p
    rtkPosOf (Ctr__Expr__18 p _ _ _) = p
    rtkPosOf (Ctr__Expr__19 p _) = p
    rtkPosOf (Ctr__Expr__20 p _ _) = p
data Field = Anti_Field String |
             Ctr__Field__0 RtkPos Ty
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Field where
    rtkPosOf (Anti_Field _) = rtkNoPos
    rtkPosOf (Ctr__Field__0 p _) = p
type FieldList = [Field]
data Id = Anti_Id String |
          Ctr__Id__0 RtkPos String
          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Id where
    rtkPosOf (Anti_Id _) = rtkNoPos
    rtkPosOf (Ctr__Id__0 p _) = p
data PArg = Anti_PArg String |
            Ctr__PArg__0 RtkPos Pat
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf PArg where
    rtkPosOf (Anti_PArg _) = rtkNoPos
    rtkPosOf (Ctr__PArg__0 p _) = p
type PArgs = [PArg]
data Param = Anti_Param String |
             Ctr__Param__0 RtkPos Id
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Param where
    rtkPosOf (Anti_Param _) = rtkNoPos
    rtkPosOf (Ctr__Param__0 p _) = p
type Params = [Param]
data Pat = Anti_Pat String |
           Ctr__Pat__0 RtkPos Int |
           Ctr__Pat__1 RtkPos |
           Ctr__Pat__2 RtkPos |
           Ctr__Pat__3 RtkPos |
           Ctr__Pat__4 RtkPos Id |
           Ctr__Pat__5 RtkPos ConId |
           Ctr__Pat__7 RtkPos ConId PArgs
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Pat where
    rtkPosOf (Anti_Pat _) = rtkNoPos
    rtkPosOf (Ctr__Pat__0 p _) = p
    rtkPosOf (Ctr__Pat__1 p) = p
    rtkPosOf (Ctr__Pat__2 p) = p
    rtkPosOf (Ctr__Pat__3 p) = p
    rtkPosOf (Ctr__Pat__4 p _) = p
    rtkPosOf (Ctr__Pat__5 p _) = p
    rtkPosOf (Ctr__Pat__7 p _ _) = p
data Ty = Anti_Ty String |
          Ctr__Ty__0 RtkPos ConId |
          Ctr__Ty__1 RtkPos Id |
          Ctr__Ty__3 RtkPos Ty Ty |
          Ctr__Ty__5 RtkPos Ty Ty
          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Ty where
    rtkPosOf (Anti_Ty _) = rtkNoPos
    rtkPosOf (Ctr__Ty__0 p _) = p
    rtkPosOf (Ctr__Ty__1 p _) = p
    rtkPosOf (Ctr__Ty__3 p _ _) = p
    rtkPosOf (Ctr__Ty__5 p _ _) = p
data TyVar = Anti_TyVar String |
             Ctr__TyVar__0 RtkPos Id
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TyVar where
    rtkPosOf (Anti_TyVar _) = rtkNoPos
    rtkPosOf (Ctr__TyVar__0 p _) = p
type TyVarList = [TyVar]
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Happy_Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Happy_Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Happy_Prelude.Bool)
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define INVALID_TOK -1#
#define ERROR_TOK 0#
#define CATCH_TOK 1#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) Happy_Prelude.$
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO Happy_Prelude.$ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    Happy_Prelude.return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyDoParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++
              ",\ttoken: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# rule) Happy_Prelude.++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Happy_Prelude.Just (Happy_GHC_Exts.I# act) -> act
  Happy_Prelude.Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(i, 0#), GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  -- i >= 0:   Guard against INVALID_TOK (do the default action, which ultimately errors)
  -- off >= 0: Otherwise it's a default action
  -- equality check: Ensure that the entry in the compressed array is owned by st
  = Happy_Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | Happy_Prelude.otherwise
  = Happy_Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state
  deriving Happy_Prelude.Show

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | Happy_Prelude.otherwise = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

happyIndexRuleArr :: Happy_Int -> (# Happy_Int, Happy_Int #)
happyIndexRuleArr r = (# nt, len #)
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts
    offs = TIMES(MINUS(r,n_starts),2#)
    nt = happyIndexOffAddr happyRuleArr offs
    len = happyIndexOffAddr happyRuleArr PLUS(offs,1#)

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     -- See "Error Fixup" below
     let i = GET_ERROR_TOKEN(x) in
     DEBUG_TRACE("shifting the error token")
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 nt fn j tk st sts stk
     = happySeq fn (happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk))

happySpecReduce_1 nt fn j tk old_st sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_2 nt fn j tk old_st
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_3 nt fn j tk old_st
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                st `happyTcHack` happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          j `happyTcHack` happyThen1 (fn stk tk)
                                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyIndexOffAddr happyGotoOffsets st1
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            j `happyTcHack` happyThen1 (fn stk tk)
                                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

{- Note [Error recovery]
~~~~~~~~~~~~~~~~~~~~~~~~
When there is no applicable action for the current lookahead token `tk`,
happy enters error recovery mode. Depending on whether the grammar file
declares the two action form `%error { abort } { report }` for
    Resumptive Error Handling,
it works in one (not resumptive) or two phases (resumptive):

 1. Fixup mode:
    Try to see if there is an action for the error token ERROR_TOK. If there
    is, do *not* emit an error and pretend instead that an `error` token was
    inserted.
    When there is no ERROR_TOK action, report an error.

    In non-resumptive error handling, calling the single error handler
    (e.g. `happyError`) will throw an exception and abort the parser.
    However, in resumptive error handling we enter *error resumption mode*.

 2. Error resumption mode:
    After reporting the error (with `report`), happy will attempt to find
    a good state stack to resume parsing in.
    For each candidate stack, it discards input until one of the candidates
    resumes (i.e. shifts the current input).
    If no candidate resumes before the end of input, resumption failed and
    calls the `abort` function, to much the same effect as in non-resumptive
    error handling.

    Candidate stacks are declared by the grammar author using the special
    `catch` terminal and called "catch frames".
    This mechanism is described in detail in Note [happyResume].

The `catch` resumption mechanism (2) is what usually is associated with
`error` in `bison` or `menhir`. Since `error` is used for the Fixup mechanism
(1) above, we call the corresponding token `catch`.
Furthermore, in constrast to `bison`, our implementation of `catch`
non-deterministically considers multiple catch frames on the stack for
resumption (See Note [Multiple catch frames]).

Note [happyResume]
~~~~~~~~~~~~~~~~~~
`happyResume` implements the resumption mechanism from Note [Error recovery].
It is best understood by example. Consider

Exp :: { String }
Exp : '1'                { "1" }
    | catch              { "catch" }
    | Exp '+' Exp %shift { $1 Happy_Prelude.++ " + " Happy_Prelude.++ $3 } -- %shift: associate 1 + 1 + 1 to the right
    | '(' Exp ')'        { "(" Happy_Prelude.++ $2 Happy_Prelude.++ ")" }

The idea of the use of `catch` here is that upon encountering a parse error
during expression parsing, we can gracefully degrade using the `catch` rule,
still producing a partial syntax tree and keep on parsing to find further
syntax errors.

Let's trace the parser state for input 11+1, which will error out after shifting 1.
After shifting, we have the following item stack (growing downwards and omitting
transitive closure items):

  State 0: %start_parseExp -> . Exp
  State 5: Exp -> '1' .

(Stack as a list of state numbers: [5,0].)
As Note [Error recovery] describes, we will first try Fixup mode.
That fails because no production can shift the `error` token.
Next we try Error resumption mode. This works as follows:

  1. Pop off the item stack until we find an item that can shift the `catch`
     token. (Implemented in `pop_items`.)
       * State 5 cannot shift catch. Pop.
       * State 0 can shift catch, which would transition into
          State 4: Exp -> catch .
     So record the *stack* `[4,0]` after doing the shift transition.
     We call this a *catch frame*, where the top is a *catch state*,
     corresponding to an item in which we just shifted a `catch` token.
     There can be multiple such catch stacks, see Note [Multiple catch frames].

  2. Discard tokens from the input until the lookahead can be shifted in one
     of the catch stacks. (Implemented in `discard_input_until_exp` and
     `some_catch_state_shifts`.)
       * We cannot shift the current lookahead '1' in state 4, so we discard
       * We *can* shift the next lookahead '+' in state 4, but only after
         reducing, which pops State 4 and goes to State 3:
           State 3: %start_parseExp -> Exp .
                    Exp -> Exp . '+' Exp
         Here we can shift '+'.
     As you can see, to implement this machinery we need to simulate
     the operation of the LALR automaton, especially reduction
     (`happySimulateReduce`).

Note [Multiple catch frames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For fewer spurious error messages, it can be beneficial to trace multiple catch
items. Consider

Exp : '1'
    | catch
    | Exp '+' Exp %shift
    | '(' Exp ')'

Let's trace the parser state for input (;+1, which will error out after shifting (.
After shifting, we have the following item stack (growing downwards):

  State 0: %start_parseExp -> . Exp
  State 6: Exp -> '(' . Exp ')'

Upon error, we want to find items in the stack which can shift a catch token.
Note that both State 0 and State 6 can shift a catch token, transitioning into
  State 4: Exp -> catch .
Hence we record the catch frames `[4,6,0]` and `[4,0]` for possible resumption.

Which catch frame do we pick for resumption?
Note that resuming catch frame `[4,0]` will parse as "catch+1", whereas
resuming the innermost frame `[4,6,0]` corresponds to parsing "(catch+1".
The latter would keep discarding input until the closing ')' is found.
So we will discard + and 1, leading to a spurious syntax error at the end of
input, aborting the parse and never producing a partial syntax tree. Bad!

It is far preferable to resume with catch frame `[4,0]`, where we can resume
successfully on input +, so that is what we do.

In general, we pick the catch frame for resumption that discards the least
amount of input for a successful shift, preferring the topmost such catch frame.
-}

-- happyFail :: Happy_Int -> Token -> Happy_Int -> _
-- This function triggers Note [Error recovery].
-- If the current token is ERROR_TOK, phase (1) has failed and we might try
-- phase (2).
happyFail ERROR_TOK = happyFixupFailed
happyFail i         = happyTryFixup i

-- Enter Error Fixup (see Note [Error recovery]):
-- generate an error token, save the old token and carry on.
-- When a `happyShift` accepts the error token, we will pop off the error token
-- to resume parsing with the current lookahead `i`.
happyTryFixup i tk action sts stk =
  DEBUG_TRACE("entering `error` fixup.\n")
  happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)
  -- NB: `happyShift` will simply pop the error token and carry on with
  --     `tk`. Hence we don't change `tk` in the call here

-- See Note [Error recovery], phase (2).
-- Enter resumption mode after reporting the error by calling `happyResume`.
happyFixupFailed tk st sts (x `HappyStk` stk) =
  let i = GET_ERROR_TOKEN(x) in
  DEBUG_TRACE("`error` fixup failed.\n")
  let resume   = happyResume i tk st sts stk
      expected = happyExpectedTokens st sts in
  happyReport i tk expected resume

-- happyResume :: Happy_Int -> Token -> Happy_Int -> _
-- See Note [happyResume]
happyResume i tk st sts stk = pop_items [] st sts stk
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts   -- this is to test whether we have a start token
    !(Happy_GHC_Exts.I# eof_i) = happy_n_terms Happy_Prelude.- 1   -- this is the token number of the EOF token
    happy_list_to_list :: Happy_IntList -> [Happy_Prelude.Int]
    happy_list_to_list (HappyCons st sts)
      | LT(st, n_starts)
      = [(Happy_GHC_Exts.I# st)]
      | Happy_Prelude.otherwise
      = (Happy_GHC_Exts.I# st) : happy_list_to_list sts

    -- See (1) of Note [happyResume]
    pop_items catch_frames st sts stk
      | LT(st, n_starts)
      = DEBUG_TRACE("reached start state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", ")
        if Happy_Prelude.null catch_frames_new
          then DEBUG_TRACE("no resumption.\n")
               happyAbort
          else DEBUG_TRACE("now discard input, trying to anchor in states " Happy_Prelude.++ Happy_Prelude.show (Happy_Prelude.map (happy_list_to_list . Happy_Prelude.fst) (Happy_Prelude.reverse catch_frames_new)) Happy_Prelude.++ ".\n")
               discard_input_until_exp i tk (Happy_Prelude.reverse catch_frames_new)
      | (HappyCons st1 sts1) <- sts, _ `HappyStk` stk1 <- stk
      = pop_items catch_frames_new st1 sts1 stk1
      where
        !catch_frames_new
          | HappyShift new_state <- happyDecodeAction (happyNextAction CATCH_TOK st)
          , DEBUG_TRACE("can shift catch token in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", into state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
            Happy_Prelude.null (Happy_Prelude.filter (\(HappyCons _ (HappyCons h _),_) -> EQ(st,h)) catch_frames)
          = (HappyCons new_state (HappyCons st sts), MK_ERROR_TOKEN(i) `HappyStk` stk):catch_frames -- MK_ERROR_TOKEN(i) is just some dummy that should not be accessed by user code
          | Happy_Prelude.otherwise
          = DEBUG_TRACE("already shifted or can't shift catch in " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ "\n")
            catch_frames

    -- See (2) of Note [happyResume]
    discard_input_until_exp i tk catch_frames
      | Happy_Prelude.Just (HappyCons st (HappyCons catch_st sts), catch_frame) <- some_catch_state_shifts i catch_frames
      = DEBUG_TRACE("found expected token in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ " after shifting from " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# catch_st) Happy_Prelude.++ ": " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ "\n")
        happyDoAction i tk st (HappyCons catch_st sts) catch_frame
      | EQ(i,eof_i) -- is i EOF?
      = DEBUG_TRACE("reached EOF, cannot resume. abort parse :(\n")
        happyAbort
      | Happy_Prelude.otherwise
      = DEBUG_TRACE("discard token " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ "\n")
        happyLex (\eof_tk -> discard_input_until_exp eof_i eof_tk catch_frames) -- eof
                 (\i tk   -> discard_input_until_exp i tk catch_frames)         -- not eof

    some_catch_state_shifts _ [] = DEBUG_TRACE("no catch state could shift.\n") Happy_Prelude.Nothing
    some_catch_state_shifts i catch_frames@(((HappyCons st sts),_):_) = try_head i st sts catch_frames
      where
        try_head i st sts catch_frames = -- PRECONDITION: head catch_frames = (HappyCons st sts)
          DEBUG_TRACE("trying token " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ " in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ": ")
          case happyDecodeAction (happyNextAction i st) of
            HappyFail     -> DEBUG_TRACE("fail.\n")   some_catch_state_shifts i (Happy_Prelude.tail catch_frames)
            HappyAccept   -> DEBUG_TRACE("accept.\n") Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyShift _  -> DEBUG_TRACE("shift.\n")  Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyReduce r -> case happySimulateReduce r st sts of
              (HappyCons st1 sts1) -> try_head i st1 sts1 catch_frames

happySimulateReduce r st sts =
  DEBUG_TRACE("simulate reduction of rule " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# r) Happy_Prelude.++ ", ")
  let (# nt, len #) = happyIndexRuleArr r in
  DEBUG_TRACE("nt " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# nt) Happy_Prelude.++ ", len: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# len) Happy_Prelude.++ ", new_st ")
  let !(sts1@(HappyCons st1 _)) = happyDrop len (HappyCons st sts)
      new_st = happyIndexGotoTable nt st1 in
  DEBUG_TRACE(Happy_Prelude.show (Happy_GHC_Exts.I# new_st) Happy_Prelude.++ ".\n")
  (HappyCons new_st sts1)

happyTokenToString :: Happy_Prelude.Int -> Happy_Prelude.String
happyTokenToString i = happyTokenStrings Happy_Prelude.!! (i Happy_Prelude.- 2) -- 2: errorTok, catchTok

happyExpectedTokens :: Happy_Int -> Happy_IntList -> [Happy_Prelude.String]
-- Upon a parse error, we want to suggest tokens that are expected in that
-- situation. This function computes such tokens.
-- It works by examining the top of the state stack.
-- For every token number that does a shift transition, record that token number.
-- For every token number that does a reduce transition, simulate that reduction
-- on the state state stack and repeat.
-- The recorded token numbers are then formatted with 'happyTokenToString' and
-- returned.
happyExpectedTokens st sts =
  DEBUG_TRACE("constructing expected tokens.\n")
  Happy_Prelude.map happyTokenToString (search_shifts st sts [])
  where
    search_shifts st sts shifts = Happy_Prelude.foldr (add_action st sts) shifts (distinct_actions st)
    add_action st sts (Happy_GHC_Exts.I# i, Happy_GHC_Exts.I# act) shifts =
      DEBUG_TRACE("found action in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", input " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ ", " Happy_Prelude.++ Happy_Prelude.show (happyDecodeAction act) Happy_Prelude.++ "\n")
      case happyDecodeAction act of
        HappyFail     -> shifts
        HappyAccept   -> shifts -- This would always be %eof or error... Not helpful
        HappyShift _  -> Happy_Prelude.insert (Happy_GHC_Exts.I# i) shifts
        HappyReduce r -> case happySimulateReduce r st sts of
          (HappyCons st1 sts1) -> search_shifts st1 sts1 shifts
    distinct_actions st
      -- The (token number, action) pairs of all actions in the given state
      = ((-1), (Happy_GHC_Exts.I# (happyIndexOffAddr happyDefActions st)))
      : [ (i, act) | i <- [begin_i..happy_n_terms], act <- get_act row_off i ]
      where
        row_off = happyIndexOffAddr happyActOffsets st
        begin_i = 2 -- +2: errorTok,catchTok
    get_act off (Happy_GHC_Exts.I# i) -- happyIndexActionTable with cached row offset
      | let off_i = PLUS(off,i)
      , GTE(off_i,0#)
      , EQ(happyIndexOffAddr happyCheck off_i,i)
      = [(Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off_i))]
      | Happy_Prelude.otherwise
      = []

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Happy_Prelude.error "Internal Happy parser panic. This is not supposed to happen! Please open a bug report at https://github.com/haskell/happy/issues.\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Happy_GHC_Exts.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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
