{
{-# LANGUAGE DeriveDataTypeable #-}
module HaskellParser where
import qualified Data.Generics as Gen
import qualified HaskellLexer as L (Token(..), PosToken(..), AlexPosn(..), alexScanTokens)
}

%name parseHaskell
%tokentype { L.PosToken }
%monad { Either String }
%error { parseError }

%token

rtk__eof { L.PosToken _ L.EndOfFile }
tok_AType_dummy_111 { L.PosToken _ L.Tk__tok_AType_dummy_111 }
tok_ATypeList_dummy_110 { L.PosToken _ L.Tk__tok_ATypeList_dummy_110 }
tok_BType_dummy_109 { L.PosToken _ L.Tk__tok_BType_dummy_109 }
tok_Body_dummy_108 { L.PosToken _ L.Tk__tok_Body_dummy_108 }
tok_CName_dummy_107 { L.PosToken _ L.Tk__tok_CName_dummy_107 }
tok_CNameList_dummy_106 { L.PosToken _ L.Tk__tok_CNameList_dummy_106 }
tok_Class_dummy_105 { L.PosToken _ L.Tk__tok_Class_dummy_105 }
tok_ClassList_dummy_104 { L.PosToken _ L.Tk__tok_ClassList_dummy_104 }
tok_Con_dummy_103 { L.PosToken _ L.Tk__tok_Con_dummy_103 }
tok_Constr_dummy_102 { L.PosToken _ L.Tk__tok_Constr_dummy_102 }
tok_Constrs_dummy_101 { L.PosToken _ L.Tk__tok_Constrs_dummy_101 }
tok_Context_dummy_100 { L.PosToken _ L.Tk__tok_Context_dummy_100 }
tok_DClass_dummy_99 { L.PosToken _ L.Tk__tok_DClass_dummy_99 }
tok_DClassList_dummy_98 { L.PosToken _ L.Tk__tok_DClassList_dummy_98 }
tok_Decl_dummy_97 { L.PosToken _ L.Tk__tok_Decl_dummy_97 }
tok_DeclList_dummy_96 { L.PosToken _ L.Tk__tok_DeclList_dummy_96 }
tok_Decls_dummy_95 { L.PosToken _ L.Tk__tok_Decls_dummy_95 }
tok_Deriving_dummy_94 { L.PosToken _ L.Tk__tok_Deriving_dummy_94 }
tok_Exp_dummy_93 { L.PosToken _ L.Tk__tok_Exp_dummy_93 }
tok_ExpI_dummy_92 { L.PosToken _ L.Tk__tok_ExpI_dummy_92 }
tok_Export_dummy_91 { L.PosToken _ L.Tk__tok_Export_dummy_91 }
tok_ExportsList_dummy_90 { L.PosToken _ L.Tk__tok_ExportsList_dummy_90 }
tok_ExportsOpt_dummy_89 { L.PosToken _ L.Tk__tok_ExportsOpt_dummy_89 }
tok_FieldDecl_dummy_88 { L.PosToken _ L.Tk__tok_FieldDecl_dummy_88 }
tok_FieldDeclList_dummy_87 { L.PosToken _ L.Tk__tok_FieldDeclList_dummy_87 }
tok_Fixity_dummy_86 { L.PosToken _ L.Tk__tok_Fixity_dummy_86 }
tok_FunLhs_dummy_85 { L.PosToken _ L.Tk__tok_FunLhs_dummy_85 }
tok_GTyCon_dummy_84 { L.PosToken _ L.Tk__tok_GTyCon_dummy_84 }
tok_Gd_dummy_83 { L.PosToken _ L.Tk__tok_Gd_dummy_83 }
tok_GdRhs_dummy_82 { L.PosToken _ L.Tk__tok_GdRhs_dummy_82 }
tok_GenDecl_dummy_81 { L.PosToken _ L.Tk__tok_GenDecl_dummy_81 }
tok_Haskell_dummy_112 { L.PosToken _ L.Tk__tok_Haskell_dummy_112 }
tok_ImpDecl_dummy_80 { L.PosToken _ L.Tk__tok_ImpDecl_dummy_80 }
tok_ImpDeclList_dummy_79 { L.PosToken _ L.Tk__tok_ImpDeclList_dummy_79 }
tok_Import_dummy_78 { L.PosToken _ L.Tk__tok_Import_dummy_78 }
tok_ImportList_dummy_77 { L.PosToken _ L.Tk__tok_ImportList_dummy_77 }
tok_ModId_dummy_76 { L.PosToken _ L.Tk__tok_ModId_dummy_76 }
tok_ModIdList_dummy_75 { L.PosToken _ L.Tk__tok_ModIdList_dummy_75 }
tok_Module_dummy_74 { L.PosToken _ L.Tk__tok_Module_dummy_74 }
tok_Op_dummy_73 { L.PosToken _ L.Tk__tok_Op_dummy_73 }
tok_Ops_dummy_72 { L.PosToken _ L.Tk__tok_Ops_dummy_72 }
tok_OptContext_dummy_71 { L.PosToken _ L.Tk__tok_OptContext_dummy_71 }
tok_OptDeriving_dummy_70 { L.PosToken _ L.Tk__tok_OptDeriving_dummy_70 }
tok_OptExpTypeSignature_dummy_69 { L.PosToken _ L.Tk__tok_OptExpTypeSignature_dummy_69 }
tok_OptGdRhs_dummy_68 { L.PosToken _ L.Tk__tok_OptGdRhs_dummy_68 }
tok_OptImpSpec_dummy_67 { L.PosToken _ L.Tk__tok_OptImpSpec_dummy_67 }
tok_OptInteger_dummy_66 { L.PosToken _ L.Tk__tok_OptInteger_dummy_66 }
tok_OptQualified_dummy_65 { L.PosToken _ L.Tk__tok_OptQualified_dummy_65 }
tok_OptQualifiedAs_dummy_64 { L.PosToken _ L.Tk__tok_OptQualifiedAs_dummy_64 }
tok_OptWhere_dummy_63 { L.PosToken _ L.Tk__tok_OptWhere_dummy_63 }
tok_Pat_dummy_62 { L.PosToken _ L.Tk__tok_Pat_dummy_62 }
tok_QOp_dummy_61 { L.PosToken _ L.Tk__tok_QOp_dummy_61 }
tok_QTyCls_dummy_60 { L.PosToken _ L.Tk__tok_QTyCls_dummy_60 }
tok_QTyCon_dummy_59 { L.PosToken _ L.Tk__tok_QTyCon_dummy_59 }
tok_QVar_dummy_58 { L.PosToken _ L.Tk__tok_QVar_dummy_58 }
tok_QVarId_dummy_57 { L.PosToken _ L.Tk__tok_QVarId_dummy_57 }
tok_QVarList_dummy_56 { L.PosToken _ L.Tk__tok_QVarList_dummy_56 }
tok_Rhs_dummy_55 { L.PosToken _ L.Tk__tok_Rhs_dummy_55 }
tok_SimpleType_dummy_54 { L.PosToken _ L.Tk__tok_SimpleType_dummy_54 }
tok_TopDecl_dummy_53 { L.PosToken _ L.Tk__tok_TopDecl_dummy_53 }
tok_TopDecls_dummy_52 { L.PosToken _ L.Tk__tok_TopDecls_dummy_52 }
tok_TyCls_dummy_51 { L.PosToken _ L.Tk__tok_TyCls_dummy_51 }
tok_TyCon_dummy_50 { L.PosToken _ L.Tk__tok_TyCon_dummy_50 }
tok_TyVar_dummy_49 { L.PosToken _ L.Tk__tok_TyVar_dummy_49 }
tok_TyVars_dummy_48 { L.PosToken _ L.Tk__tok_TyVars_dummy_48 }
tok_Type_dummy_47 { L.PosToken _ L.Tk__tok_Type_dummy_47 }
tok_TypeList_dummy_46 { L.PosToken _ L.Tk__tok_TypeList_dummy_46 }
tok_Var_dummy_45 { L.PosToken _ L.Tk__tok_Var_dummy_45 }
tok_Vars_dummy_44 { L.PosToken _ L.Tk__tok_Vars_dummy_44 }
tok__symbol__8 { L.PosToken _ L.Tk__tok__symbol__8 }
tok__pipe__21 { L.PosToken _ L.Tk__tok__pipe__21 }
tok__symbol__6 { L.PosToken _ L.Tk__tok__symbol__6 }
tok_where_1 { L.PosToken _ L.Tk__tok_where_1 }
tok_type_13 { L.PosToken _ L.Tk__tok_type_13 }
tok_qualified_10 { L.PosToken _ L.Tk__tok_qualified_10 }
tok_module_0 { L.PosToken _ L.Tk__tok_module_0 }
tok_infixr_19 { L.PosToken _ L.Tk__tok_infixr_19 }
tok_infixl_18 { L.PosToken _ L.Tk__tok_infixl_18 }
tok_infix_20 { L.PosToken _ L.Tk__tok_infix_20 }
tok_import_12 { L.PosToken _ L.Tk__tok_import_12 }
tok_deriving_23 { L.PosToken _ L.Tk__tok_deriving_23 }
tok_data_15 { L.PosToken _ L.Tk__tok_data_15 }
tok_as_11 { L.PosToken _ L.Tk__tok_as_11 }
tok__sq_bkt_r__26 { L.PosToken _ L.Tk__tok__sq_bkt_r__26 }
tok__sq_bkt_l__25 { L.PosToken _ L.Tk__tok__sq_bkt_l__25 }
tok__eql__symbol__16 { L.PosToken _ L.Tk__tok__eql__symbol__16 }
tok__eql__14 { L.PosToken _ L.Tk__tok__eql__14 }
tok__semi__7 { L.PosToken _ L.Tk__tok__semi__7 }
tok__colon__colon__17 { L.PosToken _ L.Tk__tok__colon__colon__17 }
tok__dot__dot__5 { L.PosToken _ L.Tk__tok__dot__dot__5 }
tok__dot__9 { L.PosToken _ L.Tk__tok__dot__9 }
tok__minus__symbol__24 { L.PosToken _ L.Tk__tok__minus__symbol__24 }
tok__coma__3 { L.PosToken _ L.Tk__tok__coma__3 }
tok__rparen__4 { L.PosToken _ L.Tk__tok__rparen__4 }
tok__lparen__2 { L.PosToken _ L.Tk__tok__lparen__2 }
tok__exclamation__22 { L.PosToken _ L.Tk__tok__exclamation__22 }
th { L.PosToken _ (L.Tk__th _) }
ncomment { L.PosToken _ (L.Tk__ncomment _) }
whitespace { L.PosToken _ (L.Tk__whitespace _) }
integer { L.PosToken _ (L.Tk__integer _) }
hexadecimal { L.PosToken _ (L.Tk__hexadecimal _) }
octal { L.PosToken _ (L.Tk__octal _) }
decimal { L.PosToken _ (L.Tk__decimal _) }
qq_QOp { L.PosToken _ (L.Tk__qq_QOp _) }
qq_Op { L.PosToken _ (L.Tk__qq_Op _) }
qq_TyCls { L.PosToken _ (L.Tk__qq_TyCls _) }
qq_ModId { L.PosToken _ (L.Tk__qq_ModId _) }
qq_TyCon { L.PosToken _ (L.Tk__qq_TyCon _) }
qq_TyVar { L.PosToken _ (L.Tk__qq_TyVar _) }
varid { L.PosToken _ (L.Tk__varid _) }
conid { L.PosToken _ (L.Tk__conid _) }
qq_TyVars { L.PosToken _ (L.Tk__qq_TyVars _) }
qq_SimpleType { L.PosToken _ (L.Tk__qq_SimpleType _) }
qq_TypeList { L.PosToken _ (L.Tk__qq_TypeList _) }
qq_GTyCon { L.PosToken _ (L.Tk__qq_GTyCon _) }
qq_AType { L.PosToken _ (L.Tk__qq_AType _) }
qq_ATypeList { L.PosToken _ (L.Tk__qq_ATypeList _) }
qq_BType { L.PosToken _ (L.Tk__qq_BType _) }
qq_Type { L.PosToken _ (L.Tk__qq_Type _) }
qq_Class { L.PosToken _ (L.Tk__qq_Class _) }
qq_ClassList { L.PosToken _ (L.Tk__qq_ClassList _) }
qq_Context { L.PosToken _ (L.Tk__qq_Context _) }
qq_DClass { L.PosToken _ (L.Tk__qq_DClass _) }
qq_DClassList { L.PosToken _ (L.Tk__qq_DClassList _) }
qq_Deriving { L.PosToken _ (L.Tk__qq_Deriving _) }
qq_OptDeriving { L.PosToken _ (L.Tk__qq_OptDeriving _) }
qq_Vars { L.PosToken _ (L.Tk__qq_Vars _) }
qq_FieldDecl { L.PosToken _ (L.Tk__qq_FieldDecl _) }
qq_FieldDeclList { L.PosToken _ (L.Tk__qq_FieldDeclList _) }
qq_Constr { L.PosToken _ (L.Tk__qq_Constr _) }
qq_Constrs { L.PosToken _ (L.Tk__qq_Constrs _) }
qq_GdRhs { L.PosToken _ (L.Tk__qq_GdRhs _) }
qq_ExpI { L.PosToken _ (L.Tk__qq_ExpI _) }
qq_Exp { L.PosToken _ (L.Tk__qq_Exp _) }
qq_OptExpTypeSignature { L.PosToken _ (L.Tk__qq_OptExpTypeSignature _) }
qq_Gd { L.PosToken _ (L.Tk__qq_Gd _) }
qq_OptGdRhs { L.PosToken _ (L.Tk__qq_OptGdRhs _) }
qq_Rhs { L.PosToken _ (L.Tk__qq_Rhs _) }
qq_Decls { L.PosToken _ (L.Tk__qq_Decls _) }
qq_DeclList { L.PosToken _ (L.Tk__qq_DeclList _) }
qq_OptWhere { L.PosToken _ (L.Tk__qq_OptWhere _) }
qq_Pat { L.PosToken _ (L.Tk__qq_Pat _) }
qq_FunLhs { L.PosToken _ (L.Tk__qq_FunLhs _) }
qq_Fixity { L.PosToken _ (L.Tk__qq_Fixity _) }
qq_Ops { L.PosToken _ (L.Tk__qq_Ops _) }
qq_OptInteger { L.PosToken _ (L.Tk__qq_OptInteger _) }
qq_GenDecl { L.PosToken _ (L.Tk__qq_GenDecl _) }
qq_OptContext { L.PosToken _ (L.Tk__qq_OptContext _) }
qq_Decl { L.PosToken _ (L.Tk__qq_Decl _) }
qq_TopDecl { L.PosToken _ (L.Tk__qq_TopDecl _) }
qq_TopDecls { L.PosToken _ (L.Tk__qq_TopDecls _) }
qq_ImpDecl { L.PosToken _ (L.Tk__qq_ImpDecl _) }
qq_OptImpSpec { L.PosToken _ (L.Tk__qq_OptImpSpec _) }
qq_OptQualifiedAs { L.PosToken _ (L.Tk__qq_OptQualifiedAs _) }
qq_OptQualified { L.PosToken _ (L.Tk__qq_OptQualified _) }
qq_Import { L.PosToken _ (L.Tk__qq_Import _) }
qq_QVarList { L.PosToken _ (L.Tk__qq_QVarList _) }
qq_CNameList { L.PosToken _ (L.Tk__qq_CNameList _) }
qq_CName { L.PosToken _ (L.Tk__qq_CName _) }
qq_QTyCon { L.PosToken _ (L.Tk__qq_QTyCon _) }
qq_QTyCls { L.PosToken _ (L.Tk__qq_QTyCls _) }
qq_QVar { L.PosToken _ (L.Tk__qq_QVar _) }
qq_QVarId { L.PosToken _ (L.Tk__qq_QVarId _) }
qq_ModIdList { L.PosToken _ (L.Tk__qq_ModIdList _) }
qq_Con { L.PosToken _ (L.Tk__qq_Con _) }
qq_Var { L.PosToken _ (L.Tk__qq_Var _) }
qq_ImportList { L.PosToken _ (L.Tk__qq_ImportList _) }
qq_ImpDeclList { L.PosToken _ (L.Tk__qq_ImpDeclList _) }
qq_Body { L.PosToken _ (L.Tk__qq_Body _) }
qq_Export { L.PosToken _ (L.Tk__qq_Export _) }
qq_ExportsList { L.PosToken _ (L.Tk__qq_ExportsList _) }
qq_ExportsOpt { L.PosToken _ (L.Tk__qq_ExportsOpt _) }
qq_Module { L.PosToken _ (L.Tk__qq_Module _) }
qq_Haskell { L.PosToken _ (L.Tk__qq_Haskell _) }

%%

Haskell__top : Haskell rtk__eof { $1 }

Haskell : tok_Haskell_dummy_112 Haskell tok_Haskell_dummy_112 { Ctr__Haskell__0 (rtkPosOf $1) $2 } |
          tok_AType_dummy_111 AType tok_AType_dummy_111 { Ctr__Haskell__1 (rtkPosOf $1) $2 } |
          tok_ATypeList_dummy_110 ATypeList tok_ATypeList_dummy_110 { Ctr__Haskell__2 (rtkPosOf $1) (reverse $2) } |
          tok_BType_dummy_109 BType tok_BType_dummy_109 { Ctr__Haskell__3 (rtkPosOf $1) $2 } |
          tok_Body_dummy_108 Body tok_Body_dummy_108 { Ctr__Haskell__4 (rtkPosOf $1) $2 } |
          tok_CName_dummy_107 CName tok_CName_dummy_107 { Ctr__Haskell__5 (rtkPosOf $1) $2 } |
          tok_CNameList_dummy_106 CNameList tok_CNameList_dummy_106 { Ctr__Haskell__6 (rtkPosOf $1) $2 } |
          tok_Class_dummy_105 Class tok_Class_dummy_105 { Ctr__Haskell__7 (rtkPosOf $1) $2 } |
          tok_ClassList_dummy_104 ClassList tok_ClassList_dummy_104 { Ctr__Haskell__8 (rtkPosOf $1) $2 } |
          tok_Con_dummy_103 Con tok_Con_dummy_103 { Ctr__Haskell__9 (rtkPosOf $1) $2 } |
          tok_Constr_dummy_102 Constr tok_Constr_dummy_102 { Ctr__Haskell__10 (rtkPosOf $1) $2 } |
          tok_Constrs_dummy_101 Constrs tok_Constrs_dummy_101 { Ctr__Haskell__11 (rtkPosOf $1) $2 } |
          tok_Context_dummy_100 Context tok_Context_dummy_100 { Ctr__Haskell__12 (rtkPosOf $1) $2 } |
          tok_DClass_dummy_99 DClass tok_DClass_dummy_99 { Ctr__Haskell__13 (rtkPosOf $1) $2 } |
          tok_DClassList_dummy_98 DClassList tok_DClassList_dummy_98 { Ctr__Haskell__14 (rtkPosOf $1) $2 } |
          tok_Decl_dummy_97 Decl tok_Decl_dummy_97 { Ctr__Haskell__15 (rtkPosOf $1) $2 } |
          tok_DeclList_dummy_96 DeclList tok_DeclList_dummy_96 { Ctr__Haskell__16 (rtkPosOf $1) $2 } |
          tok_Decls_dummy_95 Decls tok_Decls_dummy_95 { Ctr__Haskell__17 (rtkPosOf $1) $2 } |
          tok_Deriving_dummy_94 Deriving tok_Deriving_dummy_94 { Ctr__Haskell__18 (rtkPosOf $1) $2 } |
          tok_Exp_dummy_93 Exp tok_Exp_dummy_93 { Ctr__Haskell__19 (rtkPosOf $1) $2 } |
          tok_ExpI_dummy_92 ExpI tok_ExpI_dummy_92 { Ctr__Haskell__20 (rtkPosOf $1) $2 } |
          tok_Export_dummy_91 Export tok_Export_dummy_91 { Ctr__Haskell__21 (rtkPosOf $1) $2 } |
          tok_ExportsList_dummy_90 ExportsList tok_ExportsList_dummy_90 { Ctr__Haskell__22 (rtkPosOf $1) $2 } |
          tok_ExportsOpt_dummy_89 ExportsOpt tok_ExportsOpt_dummy_89 { Ctr__Haskell__23 (rtkPosOf $1) $2 } |
          tok_FieldDecl_dummy_88 FieldDecl tok_FieldDecl_dummy_88 { Ctr__Haskell__24 (rtkPosOf $1) $2 } |
          tok_FieldDeclList_dummy_87 FieldDeclList tok_FieldDeclList_dummy_87 { Ctr__Haskell__25 (rtkPosOf $1) $2 } |
          tok_Fixity_dummy_86 Fixity tok_Fixity_dummy_86 { Ctr__Haskell__26 (rtkPosOf $1) $2 } |
          tok_FunLhs_dummy_85 FunLhs tok_FunLhs_dummy_85 { Ctr__Haskell__27 (rtkPosOf $1) $2 } |
          tok_GTyCon_dummy_84 GTyCon tok_GTyCon_dummy_84 { Ctr__Haskell__28 (rtkPosOf $1) $2 } |
          tok_Gd_dummy_83 Gd tok_Gd_dummy_83 { Ctr__Haskell__29 (rtkPosOf $1) $2 } |
          tok_GdRhs_dummy_82 GdRhs tok_GdRhs_dummy_82 { Ctr__Haskell__30 (rtkPosOf $1) $2 } |
          tok_GenDecl_dummy_81 GenDecl tok_GenDecl_dummy_81 { Ctr__Haskell__31 (rtkPosOf $1) $2 } |
          tok_ImpDecl_dummy_80 ImpDecl tok_ImpDecl_dummy_80 { Ctr__Haskell__32 (rtkPosOf $1) $2 } |
          tok_ImpDeclList_dummy_79 ImpDeclList tok_ImpDeclList_dummy_79 { Ctr__Haskell__33 (rtkPosOf $1) $2 } |
          tok_Import_dummy_78 Import tok_Import_dummy_78 { Ctr__Haskell__34 (rtkPosOf $1) $2 } |
          tok_ImportList_dummy_77 ImportList tok_ImportList_dummy_77 { Ctr__Haskell__35 (rtkPosOf $1) $2 } |
          tok_ModId_dummy_76 ModId tok_ModId_dummy_76 { Ctr__Haskell__36 (rtkPosOf $1) $2 } |
          tok_ModIdList_dummy_75 ModIdList tok_ModIdList_dummy_75 { Ctr__Haskell__37 (rtkPosOf $1) (reverse $2) } |
          tok_Module_dummy_74 Module tok_Module_dummy_74 { Ctr__Haskell__38 (rtkPosOf $1) $2 } |
          tok_Op_dummy_73 Op tok_Op_dummy_73 { Ctr__Haskell__39 (rtkPosOf $1) $2 } |
          tok_Ops_dummy_72 Ops tok_Ops_dummy_72 { Ctr__Haskell__40 (rtkPosOf $1) $2 } |
          tok_OptContext_dummy_71 OptContext tok_OptContext_dummy_71 { Ctr__Haskell__41 (rtkPosOf $1) $2 } |
          tok_OptDeriving_dummy_70 OptDeriving tok_OptDeriving_dummy_70 { Ctr__Haskell__42 (rtkPosOf $1) $2 } |
          tok_OptExpTypeSignature_dummy_69 OptExpTypeSignature tok_OptExpTypeSignature_dummy_69 { Ctr__Haskell__43 (rtkPosOf $1) $2 } |
          tok_OptGdRhs_dummy_68 OptGdRhs tok_OptGdRhs_dummy_68 { Ctr__Haskell__44 (rtkPosOf $1) $2 } |
          tok_OptImpSpec_dummy_67 OptImpSpec tok_OptImpSpec_dummy_67 { Ctr__Haskell__45 (rtkPosOf $1) $2 } |
          tok_OptInteger_dummy_66 OptInteger tok_OptInteger_dummy_66 { Ctr__Haskell__46 (rtkPosOf $1) $2 } |
          tok_OptQualified_dummy_65 OptQualified tok_OptQualified_dummy_65 { Ctr__Haskell__47 (rtkPosOf $1) $2 } |
          tok_OptQualifiedAs_dummy_64 OptQualifiedAs tok_OptQualifiedAs_dummy_64 { Ctr__Haskell__48 (rtkPosOf $1) $2 } |
          tok_OptWhere_dummy_63 OptWhere tok_OptWhere_dummy_63 { Ctr__Haskell__49 (rtkPosOf $1) $2 } |
          tok_Pat_dummy_62 Pat tok_Pat_dummy_62 { Ctr__Haskell__50 (rtkPosOf $1) $2 } |
          tok_QOp_dummy_61 QOp tok_QOp_dummy_61 { Ctr__Haskell__51 (rtkPosOf $1) $2 } |
          tok_QTyCls_dummy_60 QTyCls tok_QTyCls_dummy_60 { Ctr__Haskell__52 (rtkPosOf $1) $2 } |
          tok_QTyCon_dummy_59 QTyCon tok_QTyCon_dummy_59 { Ctr__Haskell__53 (rtkPosOf $1) $2 } |
          tok_QVar_dummy_58 QVar tok_QVar_dummy_58 { Ctr__Haskell__54 (rtkPosOf $1) $2 } |
          tok_QVarId_dummy_57 QVarId tok_QVarId_dummy_57 { Ctr__Haskell__55 (rtkPosOf $1) $2 } |
          tok_QVarList_dummy_56 QVarList tok_QVarList_dummy_56 { Ctr__Haskell__56 (rtkPosOf $1) $2 } |
          tok_Rhs_dummy_55 Rhs tok_Rhs_dummy_55 { Ctr__Haskell__57 (rtkPosOf $1) $2 } |
          tok_SimpleType_dummy_54 SimpleType tok_SimpleType_dummy_54 { Ctr__Haskell__58 (rtkPosOf $1) $2 } |
          tok_TopDecl_dummy_53 TopDecl tok_TopDecl_dummy_53 { Ctr__Haskell__59 (rtkPosOf $1) $2 } |
          tok_TopDecls_dummy_52 TopDecls tok_TopDecls_dummy_52 { Ctr__Haskell__60 (rtkPosOf $1) $2 } |
          tok_TyCls_dummy_51 TyCls tok_TyCls_dummy_51 { Ctr__Haskell__61 (rtkPosOf $1) $2 } |
          tok_TyCon_dummy_50 TyCon tok_TyCon_dummy_50 { Ctr__Haskell__62 (rtkPosOf $1) $2 } |
          tok_TyVar_dummy_49 TyVar tok_TyVar_dummy_49 { Ctr__Haskell__63 (rtkPosOf $1) $2 } |
          tok_TyVars_dummy_48 TyVars tok_TyVars_dummy_48 { Ctr__Haskell__64 (rtkPosOf $1) (reverse $2) } |
          tok_Type_dummy_47 Type tok_Type_dummy_47 { Ctr__Haskell__65 (rtkPosOf $1) $2 } |
          tok_TypeList_dummy_46 TypeList tok_TypeList_dummy_46 { Ctr__Haskell__66 (rtkPosOf $1) $2 } |
          tok_Var_dummy_45 Var tok_Var_dummy_45 { Ctr__Haskell__67 (rtkPosOf $1) $2 } |
          tok_Vars_dummy_44 Vars tok_Vars_dummy_44 { Ctr__Haskell__68 (rtkPosOf $1) $2 }

Haskell : qq_Haskell { Anti_Haskell (tkVal_qq_Haskell $1) } |
          Module { Ctr__Haskell__69 (rtkPosOf $1) $1 }

AType : qq_AType { Anti_AType (tkVal_qq_AType $1) } |
        TyVar { Ctr__AType__0 (rtkPosOf $1) $1 } |
        GTyCon { Ctr__AType__1 (rtkPosOf $1) $1 } |
        tok__lparen__2 TypeList tok__rparen__4 { Ctr__AType__2 (rtkPosOf $1) $2 } |
        tok__sq_bkt_l__25 Rule_41 tok__sq_bkt_r__26 { Ctr__AType__3 (rtkPosOf $1) $2 }

ListElem_ATypeList40 : qq_ATypeList { Anti_AType (tkVal_qq_ATypeList $1) } |
                       AType { $1 }

ATypeList : {- empty -} { [] } |
            ATypeList ListElem_ATypeList40 { $2 : $1 }

BType : qq_BType { Anti_BType (tkVal_qq_BType $1) } |
        Rule_39 AType { Ctr__BType__0 (rtkPosOf $1) $1 $2 }

Body : qq_Body { Anti_Body (tkVal_qq_Body $1) } |
       tok__symbol__6 Rule_7 tok__symbol__8 { Ctr__Body__0 (rtkPosOf $1) $2 }

CName : qq_CName { Anti_CName (tkVal_qq_CName $1) } |
        Var { Ctr__CName__0 (rtkPosOf $1) $1 } |
        Con { Ctr__CName__1 (rtkPosOf $1) $1 }

CNameList : qq_CNameList { Anti_CNameList (tkVal_qq_CNameList $1) } |
            Rule_14 tok__coma__3 { Ctr__CNameList__0 (rtkPosOf (reverse $1)) (reverse $1) }

Class : qq_Class { Anti_Class (tkVal_qq_Class $1) } |
        QTyCls TyVar { Ctr__Class__0 (rtkPosOf $1) $1 $2 } |
        QTyCls tok__lparen__2 TyVar ATypeList tok__rparen__4 { Ctr__Class__1 (rtkPosOf $1) $1 $3 (reverse $4) }

ClassList : qq_ClassList { Anti_ClassList (tkVal_qq_ClassList $1) } |
            Rule_36 tok__coma__3 { Ctr__ClassList__0 (rtkPosOf (reverse $1)) (reverse $1) }

Con : qq_Con { Anti_Con (tkVal_qq_Con $1) } |
      conid { Ctr__Con__0 (rtkPosOf $1) (tkVal_conid $1) }

Constr : qq_Constr { Anti_Constr (tkVal_qq_Constr $1) } |
         Con tok__symbol__6 FieldDeclList tok__symbol__8 { Ctr__Constr__0 (rtkPosOf $1) $1 $3 }

Constrs : qq_Constrs { Anti_Constrs (tkVal_qq_Constrs $1) } |
          Rule_30 tok__pipe__21 { Ctr__Constrs__0 (rtkPosOf (reverse $1)) (reverse $1) }

Context : qq_Context { Anti_Context (tkVal_qq_Context $1) } |
          Class { Ctr__Context__0 (rtkPosOf $1) $1 } |
          tok__lparen__2 ClassList tok__rparen__4 { Ctr__Context__1 (rtkPosOf $1) $2 }

DClass : qq_DClass { Anti_DClass (tkVal_qq_DClass $1) } |
         QTyCls { Ctr__DClass__0 (rtkPosOf $1) $1 }

DClassList : qq_DClassList { Anti_DClassList (tkVal_qq_DClassList $1) } |
             Rule_35 tok__coma__3 { Ctr__DClassList__0 (rtkPosOf (reverse $1)) (reverse $1) }

Decl : qq_Decl { Anti_Decl (tkVal_qq_Decl $1) } |
       GenDecl { Ctr__Decl__0 (rtkPosOf $1) $1 } |
       Rule_22 Rhs { Ctr__Decl__1 (rtkPosOf $1) $1 $2 }

DeclList : qq_DeclList { Anti_DeclList (tkVal_qq_DeclList $1) } |
           Rule_26 tok__semi__7 { Ctr__DeclList__0 (rtkPosOf (reverse $1)) (reverse $1) }

Decls : qq_Decls { Anti_Decls (tkVal_qq_Decls $1) } |
        tok__symbol__6 DeclList tok__symbol__8 { Ctr__Decls__0 (rtkPosOf $1) $2 }

Deriving : qq_Deriving { Anti_Deriving (tkVal_qq_Deriving $1) } |
           tok_deriving_23 Rule_34 { Ctr__Deriving__0 (rtkPosOf $1) $2 }

Exp : qq_Exp { Anti_Exp (tkVal_qq_Exp $1) } |
      ExpI OptExpTypeSignature { Ctr__Exp__0 (rtkPosOf $1) $1 $2 }

ExpI : qq_ExpI { Anti_ExpI (tkVal_qq_ExpI $1) } |
       ExpI Rule_28 { Ctr__ExpI__0 (rtkPosOf $1) $1 (reverse $2) }

Export : qq_Export { Anti_Export (tkVal_qq_Export $1) } |
         tok_module_0 ModId { Ctr__Export__0 (rtkPosOf $1) $2 } |
         QVar { Ctr__Export__1 (rtkPosOf $1) $1 } |
         QTyCon Rule_3 { Ctr__Export__2 (rtkPosOf $1) $1 $2 } |
         QTyCls Rule_5 { Ctr__Export__3 (rtkPosOf $1) $1 $2 }

ExportsList : qq_ExportsList { Anti_ExportsList (tkVal_qq_ExportsList $1) } |
              Rule_2 tok__coma__3 { Ctr__ExportsList__0 (rtkPosOf (reverse $1)) (reverse $1) }

ExportsOpt : qq_ExportsOpt { Anti_ExportsOpt (tkVal_qq_ExportsOpt $1) } |
             { Ctr__ExportsOpt__0 rtkNoPos } |
             Rule_0 { Ctr__ExportsOpt__1 (rtkPosOf $1) $1 }

FieldDecl : qq_FieldDecl { Anti_FieldDecl (tkVal_qq_FieldDecl $1) } |
            Vars tok__colon__colon__17 Rule_32 { Ctr__FieldDecl__0 (rtkPosOf $1) $1 $3 }

FieldDeclList : qq_FieldDeclList { Anti_FieldDeclList (tkVal_qq_FieldDeclList $1) } |
                Rule_31 tok__coma__3 { Ctr__FieldDeclList__0 (rtkPosOf (reverse $1)) (reverse $1) }

Fixity : qq_Fixity { Anti_Fixity (tkVal_qq_Fixity $1) } |
         tok_infixl_18 { Ctr__Fixity__0 (rtkPosOf $1) } |
         tok_infixr_19 { Ctr__Fixity__1 (rtkPosOf $1) } |
         tok_infix_20 { Ctr__Fixity__2 (rtkPosOf $1) }

FunLhs : qq_FunLhs { Anti_FunLhs (tkVal_qq_FunLhs $1) } |
         Var { Ctr__FunLhs__0 (rtkPosOf $1) $1 }

GTyCon : qq_GTyCon { Anti_GTyCon (tkVal_qq_GTyCon $1) } |
         QTyCon { Ctr__GTyCon__0 (rtkPosOf $1) $1 } |
         tok__lparen__2 tok__minus__symbol__24 tok__rparen__4 { Ctr__GTyCon__1 (rtkPosOf $1) }

Gd : qq_Gd { Anti_Gd (tkVal_qq_Gd $1) } |
     { Ctr__Gd__0 rtkNoPos } |
     ExpI { Ctr__Gd__1 (rtkPosOf $1) $1 }

GdRhs : qq_GdRhs { Anti_GdRhs (tkVal_qq_GdRhs $1) } |
        Gd tok__eql__14 Exp OptGdRhs { Ctr__GdRhs__0 (rtkPosOf $1) $1 $3 $4 }

GenDecl : qq_GenDecl { Anti_GenDecl (tkVal_qq_GenDecl $1) } |
          Vars tok__colon__colon__17 OptContext Type { Ctr__GenDecl__0 (rtkPosOf $1) $1 $3 $4 } |
          Fixity OptInteger Ops { Ctr__GenDecl__1 (rtkPosOf $1) $1 $2 $3 }

ImpDecl : qq_ImpDecl { Anti_ImpDecl (tkVal_qq_ImpDecl $1) } |
          tok_import_12 OptQualified ModId OptQualifiedAs Rule_20 { Ctr__ImpDecl__0 (rtkPosOf $1) $2 $3 $4 $5 }

ImpDeclList : qq_ImpDeclList { Anti_ImpDeclList (tkVal_qq_ImpDeclList $1) } |
              Rule_10 tok__semi__7 { Ctr__ImpDeclList__0 (rtkPosOf (reverse $1)) (reverse $1) }

Import : qq_Import { Anti_Import (tkVal_qq_Import $1) } |
         Var { Ctr__Import__0 (rtkPosOf $1) $1 } |
         TyCon Rule_16 { Ctr__Import__1 (rtkPosOf $1) $1 $2 }

ImportList : qq_ImportList { Anti_ImportList (tkVal_qq_ImportList $1) } |
             Rule_11 tok__coma__3 { Ctr__ImportList__0 (rtkPosOf (reverse $1)) (reverse $1) }

ModId : qq_ModId { Anti_ModId (tkVal_qq_ModId $1) } |
        conid { Ctr__ModId__0 (rtkPosOf $1) (tkVal_conid $1) }

ModIdList : {- empty -} { [] } |
            ModIdList ListElem_ModIdList13 { $2 : $1 }

Module : qq_Module { Anti_Module (tkVal_qq_Module $1) } |
         tok_module_0 ModId ExportsOpt tok_where_1 Body { Ctr__Module__0 (rtkPosOf $1) $2 $3 $5 } |
         Body { Ctr__Module__1 (rtkPosOf $1) $1 }

Op : qq_Op { Anti_Op (tkVal_qq_Op $1) } |
     varid { Ctr__Op__0 (rtkPosOf $1) (tkVal_varid $1) } |
     conid { Ctr__Op__1 (rtkPosOf $1) (tkVal_conid $1) }

Ops : qq_Ops { Anti_Ops (tkVal_qq_Ops $1) } |
      Rule_24 tok__coma__3 { Ctr__Ops__0 (rtkPosOf (reverse $1)) (reverse $1) }

OptContext : qq_OptContext { Anti_OptContext (tkVal_qq_OptContext $1) } |
             { Ctr__OptContext__0 rtkNoPos } |
             Rule_23 { Ctr__OptContext__1 (rtkPosOf $1) $1 }

OptDeriving : qq_OptDeriving { Anti_OptDeriving (tkVal_qq_OptDeriving $1) } |
              { Ctr__OptDeriving__0 rtkNoPos } |
              Deriving { Ctr__OptDeriving__1 (rtkPosOf $1) $1 }

OptExpTypeSignature : qq_OptExpTypeSignature { Anti_OptExpTypeSignature (tkVal_qq_OptExpTypeSignature $1) } |
                      { Ctr__OptExpTypeSignature__0 rtkNoPos } |
                      Rule_27 { Ctr__OptExpTypeSignature__1 (rtkPosOf $1) $1 }

OptGdRhs : qq_OptGdRhs { Anti_OptGdRhs (tkVal_qq_OptGdRhs $1) } |
           { Ctr__OptGdRhs__0 rtkNoPos } |
           GdRhs { Ctr__OptGdRhs__1 (rtkPosOf $1) $1 }

OptImpSpec : qq_OptImpSpec { Anti_OptImpSpec (tkVal_qq_OptImpSpec $1) } |
             tok__lparen__2 ImportList Rule_19 tok__rparen__4 { Ctr__OptImpSpec__0 (rtkPosOf $1) $2 $3 }

OptInteger : qq_OptInteger { Anti_OptInteger (tkVal_qq_OptInteger $1) } |
             { Ctr__OptInteger__0 rtkNoPos } |
             integer { Ctr__OptInteger__1 (rtkPosOf $1) (tkVal_integer $1) }

OptQualified : qq_OptQualified { Anti_OptQualified (tkVal_qq_OptQualified $1) } |
               { Ctr__OptQualified__0 rtkNoPos } |
               tok_qualified_10 { Ctr__OptQualified__1 (rtkPosOf $1) }

OptQualifiedAs : qq_OptQualifiedAs { Anti_OptQualifiedAs (tkVal_qq_OptQualifiedAs $1) } |
                 { Ctr__OptQualifiedAs__0 rtkNoPos } |
                 Rule_18 { Ctr__OptQualifiedAs__1 (rtkPosOf $1) $1 }

OptWhere : qq_OptWhere { Anti_OptWhere (tkVal_qq_OptWhere $1) } |
           tok_where_1 Decls { Ctr__OptWhere__0 (rtkPosOf $1) $2 }

Pat : qq_Pat { Anti_Pat (tkVal_qq_Pat $1) } |
      Con Rule_25 { Ctr__Pat__0 (rtkPosOf $1) $1 (reverse $2) }

QOp : qq_QOp { Anti_QOp (tkVal_qq_QOp $1) } |
      ModIdList Op { Ctr__QOp__0 (rtkPosOf (reverse $1)) (reverse $1) $2 }

QTyCls : qq_QTyCls { Anti_QTyCls (tkVal_qq_QTyCls $1) } |
         ModIdList TyCls { Ctr__QTyCls__0 (rtkPosOf (reverse $1)) (reverse $1) $2 }

QTyCon : qq_QTyCon { Anti_QTyCon (tkVal_qq_QTyCon $1) } |
         ModIdList TyCon { Ctr__QTyCon__0 (rtkPosOf (reverse $1)) (reverse $1) $2 }

QVar : qq_QVar { Anti_QVar (tkVal_qq_QVar $1) } |
       QVarId { Ctr__QVar__0 (rtkPosOf $1) $1 }

QVarId : qq_QVarId { Anti_QVarId (tkVal_qq_QVarId $1) } |
         ModIdList varid { Ctr__QVarId__0 (rtkPosOf (reverse $1)) (reverse $1) (tkVal_varid $2) }

QVarList : qq_QVarList { Anti_QVarList (tkVal_qq_QVarList $1) } |
           Rule_15 tok__coma__3 { Ctr__QVarList__0 (rtkPosOf (reverse $1)) (reverse $1) }

Rhs : qq_Rhs { Anti_Rhs (tkVal_qq_Rhs $1) } |
      tok__eql__14 Exp OptWhere { Ctr__Rhs__0 (rtkPosOf $1) $2 $3 } |
      GdRhs OptWhere { Ctr__Rhs__1 (rtkPosOf $1) $1 $2 }

Rule_0 : tok__lparen__2 ExportsList Rule_1 tok__rparen__4 { Ctr__Rule_0__0 (rtkPosOf $1) $2 $3 }

Rule_1 : { Ctr__Rule_1__0 rtkNoPos } |
         tok__coma__3 { Ctr__Rule_1__1 (rtkPosOf $1) }

Rule_10 : ImpDecl { [$1] } |
          Rule_10 ImpDecl { $2 : $1 }

Rule_11 : {- empty -} { [] } |
          Rule_11 Import { $2 : $1 }

ListElem_ModIdList13 : qq_ModIdList { Anti_Rule_12 (tkVal_qq_ModIdList $1) } |
                       Rule_12 { $1 }

Rule_12 : ModId tok__dot__9 { Ctr__Rule_12__1 (rtkPosOf $1) $1 }

Rule_14 : {- empty -} { [] } |
          Rule_14 CName { $2 : $1 }

Rule_15 : {- empty -} { [] } |
          Rule_15 QVar { $2 : $1 }

Rule_16 : { Ctr__Rule_16__0 rtkNoPos } |
          Rule_17 { Ctr__Rule_16__1 (rtkPosOf $1) $1 }

Rule_17 : tok__lparen__2 tok__dot__dot__5 tok__rparen__4 { Ctr__Rule_17__0 (rtkPosOf $1) } |
          tok__lparen__2 CNameList tok__rparen__4 { Ctr__Rule_17__1 (rtkPosOf $1) $2 }

Rule_18 : tok_as_11 ModId { Ctr__Rule_18__0 (rtkPosOf $1) $2 }

Rule_19 : { Ctr__Rule_19__0 rtkNoPos } |
          tok__coma__3 { Ctr__Rule_19__1 (rtkPosOf $1) }

Rule_2 : {- empty -} { [] } |
         Rule_2 Export { $2 : $1 }

Rule_20 : { Ctr__Rule_20__0 rtkNoPos } |
          OptImpSpec { Ctr__Rule_20__1 (rtkPosOf $1) $1 }

Rule_21 : {- empty -} { [] } |
          Rule_21 TopDecl { $2 : $1 }

Rule_22 : FunLhs { Ctr__Rule_22__0 (rtkPosOf $1) $1 } |
          Pat { Ctr__Rule_22__1 (rtkPosOf $1) $1 }

Rule_23 : Context tok__eql__symbol__16 { Ctr__Rule_23__0 (rtkPosOf $1) $1 }

Rule_24 : {- empty -} { [] } |
          Rule_24 Op { $2 : $1 }

Rule_25 : {- empty -} { [] } |
          Rule_25 Var { $2 : $1 }

Rule_26 : {- empty -} { [] } |
          Rule_26 Decl { $2 : $1 }

Rule_27 : tok__colon__colon__17 OptContext Type { Ctr__Rule_27__0 (rtkPosOf $1) $2 $3 }

Rule_28 : {- empty -} { [] } |
          Rule_28 Rule_29 { $2 : $1 }

Rule_29 : QOp ExpI { Ctr__Rule_29__0 (rtkPosOf $1) $1 $2 }

Rule_3 : { Ctr__Rule_3__0 rtkNoPos } |
         Rule_4 { Ctr__Rule_3__1 (rtkPosOf $1) $1 }

Rule_30 : {- empty -} { [] } |
          Rule_30 Constr { $2 : $1 }

Rule_31 : {- empty -} { [] } |
          Rule_31 FieldDecl { $2 : $1 }

Rule_32 : Type { Ctr__Rule_32__0 (rtkPosOf $1) $1 } |
          tok__exclamation__22 AType { Ctr__Rule_32__1 (rtkPosOf $1) $2 }

Rule_33 : {- empty -} { [] } |
          Rule_33 Var { $2 : $1 }

Rule_34 : DClass { Ctr__Rule_34__0 (rtkPosOf $1) $1 } |
          tok__lparen__2 DClassList tok__rparen__4 { Ctr__Rule_34__1 (rtkPosOf $1) $2 }

Rule_35 : {- empty -} { [] } |
          Rule_35 DClass { $2 : $1 }

Rule_36 : {- empty -} { [] } |
          Rule_36 Class { $2 : $1 }

Rule_37 : { Ctr__Rule_37__0 rtkNoPos } |
          Rule_38 { Ctr__Rule_37__1 (rtkPosOf $1) $1 }

Rule_38 : tok__minus__symbol__24 Type { Ctr__Rule_38__0 (rtkPosOf $1) $2 }

Rule_39 : { Ctr__Rule_39__0 rtkNoPos } |
          BType { Ctr__Rule_39__1 (rtkPosOf $1) $1 }

Rule_4 : tok__lparen__2 tok__dot__dot__5 tok__rparen__4 { Ctr__Rule_4__0 (rtkPosOf $1) } |
         tok__lparen__2 CNameList tok__rparen__4 { Ctr__Rule_4__1 (rtkPosOf $1) $2 }

Rule_41 : { Ctr__Rule_41__0 rtkNoPos } |
          Type { Ctr__Rule_41__1 (rtkPosOf $1) $1 }

Rule_42 : {- empty -} { [] } |
          Rule_42 Type { $2 : $1 }

Rule_5 : { Ctr__Rule_5__0 rtkNoPos } |
         Rule_6 { Ctr__Rule_5__1 (rtkPosOf $1) $1 }

Rule_6 : tok__lparen__2 tok__dot__dot__5 tok__rparen__4 { Ctr__Rule_6__0 (rtkPosOf $1) } |
         tok__lparen__2 QVarList tok__rparen__4 { Ctr__Rule_6__1 (rtkPosOf $1) $2 }

Rule_7 : ImpDeclList Rule_8 { Ctr__Rule_7__0 (rtkPosOf $1) $1 $2 } |
         ImpDeclList { Ctr__Rule_7__1 (rtkPosOf $1) $1 }

Rule_8 : { Ctr__Rule_8__0 rtkNoPos } |
         Rule_9 { Ctr__Rule_8__1 (rtkPosOf $1) $1 }

Rule_9 : tok__semi__7 TopDecls { Ctr__Rule_9__0 (rtkPosOf $1) $2 }

SimpleType : qq_SimpleType { Anti_SimpleType (tkVal_qq_SimpleType $1) } |
             TyCon TyVars { Ctr__SimpleType__0 (rtkPosOf $1) $1 (reverse $2) }

TopDecl : qq_TopDecl { Anti_TopDecl (tkVal_qq_TopDecl $1) } |
          tok_type_13 SimpleType tok__eql__14 Type { Ctr__TopDecl__0 (rtkPosOf $1) $2 $4 } |
          tok_data_15 OptContext SimpleType tok__eql__14 Constrs OptDeriving { Ctr__TopDecl__1 (rtkPosOf $1) $2 $3 $5 $6 } |
          Decl { Ctr__TopDecl__2 (rtkPosOf $1) $1 }

TopDecls : qq_TopDecls { Anti_TopDecls (tkVal_qq_TopDecls $1) } |
           Rule_21 tok__semi__7 { Ctr__TopDecls__0 (rtkPosOf (reverse $1)) (reverse $1) }

TyCls : qq_TyCls { Anti_TyCls (tkVal_qq_TyCls $1) } |
        conid { Ctr__TyCls__0 (rtkPosOf $1) (tkVal_conid $1) }

TyCon : qq_TyCon { Anti_TyCon (tkVal_qq_TyCon $1) } |
        conid { Ctr__TyCon__0 (rtkPosOf $1) (tkVal_conid $1) }

TyVar : qq_TyVar { Anti_TyVar (tkVal_qq_TyVar $1) } |
        varid { Ctr__TyVar__0 (rtkPosOf $1) (tkVal_varid $1) }

ListElem_TyVars43 : qq_TyVars { Anti_TyVar (tkVal_qq_TyVars $1) } |
                    TyVar { $1 }

TyVars : {- empty -} { [] } |
         TyVars ListElem_TyVars43 { $2 : $1 }

Type : qq_Type { Anti_Type (tkVal_qq_Type $1) } |
       BType Rule_37 { Ctr__Type__0 (rtkPosOf $1) $1 $2 }

TypeList : qq_TypeList { Anti_TypeList (tkVal_qq_TypeList $1) } |
           Rule_42 tok__coma__3 { Ctr__TypeList__0 (rtkPosOf (reverse $1)) (reverse $1) }

Var : qq_Var { Anti_Var (tkVal_qq_Var $1) } |
      varid { Ctr__Var__0 (rtkPosOf $1) (tkVal_varid $1) }

Vars : qq_Vars { Anti_Vars (tkVal_qq_Vars $1) } |
       Rule_33 tok__coma__3 { Ctr__Vars__0 (rtkPosOf (reverse $1)) (reverse $1) }


{
parseError :: [L.PosToken] -> Either String a
parseError [] = Left "unexpected end of input"
parseError (L.PosToken (L.AlexPn _ line col) tok : _) =
    Left $ show line ++ ":" ++ show col ++ ":unexpected " ++ showRtkToken tok

-- Render a token the way it appears in the source, for error messages
showRtkToken :: L.Token -> String
showRtkToken L.EndOfFile = "end of input"
showRtkToken L.Tk__tok_AType_dummy_111 = "'tok_AType_dummy_111'"
showRtkToken L.Tk__tok_ATypeList_dummy_110 = "'tok_ATypeList_dummy_110'"
showRtkToken L.Tk__tok_BType_dummy_109 = "'tok_BType_dummy_109'"
showRtkToken L.Tk__tok_Body_dummy_108 = "'tok_Body_dummy_108'"
showRtkToken L.Tk__tok_CName_dummy_107 = "'tok_CName_dummy_107'"
showRtkToken L.Tk__tok_CNameList_dummy_106 = "'tok_CNameList_dummy_106'"
showRtkToken L.Tk__tok_Class_dummy_105 = "'tok_Class_dummy_105'"
showRtkToken L.Tk__tok_ClassList_dummy_104 = "'tok_ClassList_dummy_104'"
showRtkToken L.Tk__tok_Con_dummy_103 = "'tok_Con_dummy_103'"
showRtkToken L.Tk__tok_Constr_dummy_102 = "'tok_Constr_dummy_102'"
showRtkToken L.Tk__tok_Constrs_dummy_101 = "'tok_Constrs_dummy_101'"
showRtkToken L.Tk__tok_Context_dummy_100 = "'tok_Context_dummy_100'"
showRtkToken L.Tk__tok_DClass_dummy_99 = "'tok_DClass_dummy_99'"
showRtkToken L.Tk__tok_DClassList_dummy_98 = "'tok_DClassList_dummy_98'"
showRtkToken L.Tk__tok_Decl_dummy_97 = "'tok_Decl_dummy_97'"
showRtkToken L.Tk__tok_DeclList_dummy_96 = "'tok_DeclList_dummy_96'"
showRtkToken L.Tk__tok_Decls_dummy_95 = "'tok_Decls_dummy_95'"
showRtkToken L.Tk__tok_Deriving_dummy_94 = "'tok_Deriving_dummy_94'"
showRtkToken L.Tk__tok_Exp_dummy_93 = "'tok_Exp_dummy_93'"
showRtkToken L.Tk__tok_ExpI_dummy_92 = "'tok_ExpI_dummy_92'"
showRtkToken L.Tk__tok_Export_dummy_91 = "'tok_Export_dummy_91'"
showRtkToken L.Tk__tok_ExportsList_dummy_90 = "'tok_ExportsList_dummy_90'"
showRtkToken L.Tk__tok_ExportsOpt_dummy_89 = "'tok_ExportsOpt_dummy_89'"
showRtkToken L.Tk__tok_FieldDecl_dummy_88 = "'tok_FieldDecl_dummy_88'"
showRtkToken L.Tk__tok_FieldDeclList_dummy_87 = "'tok_FieldDeclList_dummy_87'"
showRtkToken L.Tk__tok_Fixity_dummy_86 = "'tok_Fixity_dummy_86'"
showRtkToken L.Tk__tok_FunLhs_dummy_85 = "'tok_FunLhs_dummy_85'"
showRtkToken L.Tk__tok_GTyCon_dummy_84 = "'tok_GTyCon_dummy_84'"
showRtkToken L.Tk__tok_Gd_dummy_83 = "'tok_Gd_dummy_83'"
showRtkToken L.Tk__tok_GdRhs_dummy_82 = "'tok_GdRhs_dummy_82'"
showRtkToken L.Tk__tok_GenDecl_dummy_81 = "'tok_GenDecl_dummy_81'"
showRtkToken L.Tk__tok_Haskell_dummy_112 = "'tok_Haskell_dummy_112'"
showRtkToken L.Tk__tok_ImpDecl_dummy_80 = "'tok_ImpDecl_dummy_80'"
showRtkToken L.Tk__tok_ImpDeclList_dummy_79 = "'tok_ImpDeclList_dummy_79'"
showRtkToken L.Tk__tok_Import_dummy_78 = "'tok_Import_dummy_78'"
showRtkToken L.Tk__tok_ImportList_dummy_77 = "'tok_ImportList_dummy_77'"
showRtkToken L.Tk__tok_ModId_dummy_76 = "'tok_ModId_dummy_76'"
showRtkToken L.Tk__tok_ModIdList_dummy_75 = "'tok_ModIdList_dummy_75'"
showRtkToken L.Tk__tok_Module_dummy_74 = "'tok_Module_dummy_74'"
showRtkToken L.Tk__tok_Op_dummy_73 = "'tok_Op_dummy_73'"
showRtkToken L.Tk__tok_Ops_dummy_72 = "'tok_Ops_dummy_72'"
showRtkToken L.Tk__tok_OptContext_dummy_71 = "'tok_OptContext_dummy_71'"
showRtkToken L.Tk__tok_OptDeriving_dummy_70 = "'tok_OptDeriving_dummy_70'"
showRtkToken L.Tk__tok_OptExpTypeSignature_dummy_69 = "'tok_OptExpTypeSignature_dummy_69'"
showRtkToken L.Tk__tok_OptGdRhs_dummy_68 = "'tok_OptGdRhs_dummy_68'"
showRtkToken L.Tk__tok_OptImpSpec_dummy_67 = "'tok_OptImpSpec_dummy_67'"
showRtkToken L.Tk__tok_OptInteger_dummy_66 = "'tok_OptInteger_dummy_66'"
showRtkToken L.Tk__tok_OptQualified_dummy_65 = "'tok_OptQualified_dummy_65'"
showRtkToken L.Tk__tok_OptQualifiedAs_dummy_64 = "'tok_OptQualifiedAs_dummy_64'"
showRtkToken L.Tk__tok_OptWhere_dummy_63 = "'tok_OptWhere_dummy_63'"
showRtkToken L.Tk__tok_Pat_dummy_62 = "'tok_Pat_dummy_62'"
showRtkToken L.Tk__tok_QOp_dummy_61 = "'tok_QOp_dummy_61'"
showRtkToken L.Tk__tok_QTyCls_dummy_60 = "'tok_QTyCls_dummy_60'"
showRtkToken L.Tk__tok_QTyCon_dummy_59 = "'tok_QTyCon_dummy_59'"
showRtkToken L.Tk__tok_QVar_dummy_58 = "'tok_QVar_dummy_58'"
showRtkToken L.Tk__tok_QVarId_dummy_57 = "'tok_QVarId_dummy_57'"
showRtkToken L.Tk__tok_QVarList_dummy_56 = "'tok_QVarList_dummy_56'"
showRtkToken L.Tk__tok_Rhs_dummy_55 = "'tok_Rhs_dummy_55'"
showRtkToken L.Tk__tok_SimpleType_dummy_54 = "'tok_SimpleType_dummy_54'"
showRtkToken L.Tk__tok_TopDecl_dummy_53 = "'tok_TopDecl_dummy_53'"
showRtkToken L.Tk__tok_TopDecls_dummy_52 = "'tok_TopDecls_dummy_52'"
showRtkToken L.Tk__tok_TyCls_dummy_51 = "'tok_TyCls_dummy_51'"
showRtkToken L.Tk__tok_TyCon_dummy_50 = "'tok_TyCon_dummy_50'"
showRtkToken L.Tk__tok_TyVar_dummy_49 = "'tok_TyVar_dummy_49'"
showRtkToken L.Tk__tok_TyVars_dummy_48 = "'tok_TyVars_dummy_48'"
showRtkToken L.Tk__tok_Type_dummy_47 = "'tok_Type_dummy_47'"
showRtkToken L.Tk__tok_TypeList_dummy_46 = "'tok_TypeList_dummy_46'"
showRtkToken L.Tk__tok_Var_dummy_45 = "'tok_Var_dummy_45'"
showRtkToken L.Tk__tok_Vars_dummy_44 = "'tok_Vars_dummy_44'"
showRtkToken L.Tk__tok__symbol__8 = "'}'"
showRtkToken L.Tk__tok__pipe__21 = "'|'"
showRtkToken L.Tk__tok__symbol__6 = "'{'"
showRtkToken L.Tk__tok_where_1 = "'where'"
showRtkToken L.Tk__tok_type_13 = "'type'"
showRtkToken L.Tk__tok_qualified_10 = "'qualified'"
showRtkToken L.Tk__tok_module_0 = "'module'"
showRtkToken L.Tk__tok_infixr_19 = "'infixr'"
showRtkToken L.Tk__tok_infixl_18 = "'infixl'"
showRtkToken L.Tk__tok_infix_20 = "'infix'"
showRtkToken L.Tk__tok_import_12 = "'import'"
showRtkToken L.Tk__tok_deriving_23 = "'deriving'"
showRtkToken L.Tk__tok_data_15 = "'data'"
showRtkToken L.Tk__tok_as_11 = "'as'"
showRtkToken L.Tk__tok__sq_bkt_r__26 = "']'"
showRtkToken L.Tk__tok__sq_bkt_l__25 = "'['"
showRtkToken L.Tk__tok__eql__symbol__16 = "'=>'"
showRtkToken L.Tk__tok__eql__14 = "'='"
showRtkToken L.Tk__tok__semi__7 = "';'"
showRtkToken L.Tk__tok__colon__colon__17 = "'::'"
showRtkToken L.Tk__tok__dot__dot__5 = "'..'"
showRtkToken L.Tk__tok__dot__9 = "'.'"
showRtkToken L.Tk__tok__minus__symbol__24 = "'->'"
showRtkToken L.Tk__tok__coma__3 = "','"
showRtkToken L.Tk__tok__rparen__4 = "')'"
showRtkToken L.Tk__tok__lparen__2 = "'('"
showRtkToken L.Tk__tok__exclamation__22 = "'!'"
showRtkToken (L.Tk__th v) = "th " ++ show v
showRtkToken (L.Tk__ncomment v) = "ncomment " ++ show v
showRtkToken (L.Tk__whitespace v) = "whitespace " ++ show v
showRtkToken (L.Tk__integer v) = "integer " ++ show v
showRtkToken (L.Tk__hexadecimal v) = "hexadecimal " ++ show v
showRtkToken (L.Tk__octal v) = "octal " ++ show v
showRtkToken (L.Tk__decimal v) = "decimal " ++ show v
showRtkToken (L.Tk__qq_QOp v) = "qq_QOp " ++ show v
showRtkToken (L.Tk__qq_Op v) = "qq_Op " ++ show v
showRtkToken (L.Tk__qq_TyCls v) = "qq_TyCls " ++ show v
showRtkToken (L.Tk__qq_ModId v) = "qq_ModId " ++ show v
showRtkToken (L.Tk__qq_TyCon v) = "qq_TyCon " ++ show v
showRtkToken (L.Tk__qq_TyVar v) = "qq_TyVar " ++ show v
showRtkToken (L.Tk__varid v) = "varid " ++ show v
showRtkToken (L.Tk__conid v) = "conid " ++ show v
showRtkToken (L.Tk__qq_TyVars v) = "qq_TyVars " ++ show v
showRtkToken (L.Tk__qq_SimpleType v) = "qq_SimpleType " ++ show v
showRtkToken (L.Tk__qq_TypeList v) = "qq_TypeList " ++ show v
showRtkToken (L.Tk__qq_GTyCon v) = "qq_GTyCon " ++ show v
showRtkToken (L.Tk__qq_AType v) = "qq_AType " ++ show v
showRtkToken (L.Tk__qq_ATypeList v) = "qq_ATypeList " ++ show v
showRtkToken (L.Tk__qq_BType v) = "qq_BType " ++ show v
showRtkToken (L.Tk__qq_Type v) = "qq_Type " ++ show v
showRtkToken (L.Tk__qq_Class v) = "qq_Class " ++ show v
showRtkToken (L.Tk__qq_ClassList v) = "qq_ClassList " ++ show v
showRtkToken (L.Tk__qq_Context v) = "qq_Context " ++ show v
showRtkToken (L.Tk__qq_DClass v) = "qq_DClass " ++ show v
showRtkToken (L.Tk__qq_DClassList v) = "qq_DClassList " ++ show v
showRtkToken (L.Tk__qq_Deriving v) = "qq_Deriving " ++ show v
showRtkToken (L.Tk__qq_OptDeriving v) = "qq_OptDeriving " ++ show v
showRtkToken (L.Tk__qq_Vars v) = "qq_Vars " ++ show v
showRtkToken (L.Tk__qq_FieldDecl v) = "qq_FieldDecl " ++ show v
showRtkToken (L.Tk__qq_FieldDeclList v) = "qq_FieldDeclList " ++ show v
showRtkToken (L.Tk__qq_Constr v) = "qq_Constr " ++ show v
showRtkToken (L.Tk__qq_Constrs v) = "qq_Constrs " ++ show v
showRtkToken (L.Tk__qq_GdRhs v) = "qq_GdRhs " ++ show v
showRtkToken (L.Tk__qq_ExpI v) = "qq_ExpI " ++ show v
showRtkToken (L.Tk__qq_Exp v) = "qq_Exp " ++ show v
showRtkToken (L.Tk__qq_OptExpTypeSignature v) = "qq_OptExpTypeSignature " ++ show v
showRtkToken (L.Tk__qq_Gd v) = "qq_Gd " ++ show v
showRtkToken (L.Tk__qq_OptGdRhs v) = "qq_OptGdRhs " ++ show v
showRtkToken (L.Tk__qq_Rhs v) = "qq_Rhs " ++ show v
showRtkToken (L.Tk__qq_Decls v) = "qq_Decls " ++ show v
showRtkToken (L.Tk__qq_DeclList v) = "qq_DeclList " ++ show v
showRtkToken (L.Tk__qq_OptWhere v) = "qq_OptWhere " ++ show v
showRtkToken (L.Tk__qq_Pat v) = "qq_Pat " ++ show v
showRtkToken (L.Tk__qq_FunLhs v) = "qq_FunLhs " ++ show v
showRtkToken (L.Tk__qq_Fixity v) = "qq_Fixity " ++ show v
showRtkToken (L.Tk__qq_Ops v) = "qq_Ops " ++ show v
showRtkToken (L.Tk__qq_OptInteger v) = "qq_OptInteger " ++ show v
showRtkToken (L.Tk__qq_GenDecl v) = "qq_GenDecl " ++ show v
showRtkToken (L.Tk__qq_OptContext v) = "qq_OptContext " ++ show v
showRtkToken (L.Tk__qq_Decl v) = "qq_Decl " ++ show v
showRtkToken (L.Tk__qq_TopDecl v) = "qq_TopDecl " ++ show v
showRtkToken (L.Tk__qq_TopDecls v) = "qq_TopDecls " ++ show v
showRtkToken (L.Tk__qq_ImpDecl v) = "qq_ImpDecl " ++ show v
showRtkToken (L.Tk__qq_OptImpSpec v) = "qq_OptImpSpec " ++ show v
showRtkToken (L.Tk__qq_OptQualifiedAs v) = "qq_OptQualifiedAs " ++ show v
showRtkToken (L.Tk__qq_OptQualified v) = "qq_OptQualified " ++ show v
showRtkToken (L.Tk__qq_Import v) = "qq_Import " ++ show v
showRtkToken (L.Tk__qq_QVarList v) = "qq_QVarList " ++ show v
showRtkToken (L.Tk__qq_CNameList v) = "qq_CNameList " ++ show v
showRtkToken (L.Tk__qq_CName v) = "qq_CName " ++ show v
showRtkToken (L.Tk__qq_QTyCon v) = "qq_QTyCon " ++ show v
showRtkToken (L.Tk__qq_QTyCls v) = "qq_QTyCls " ++ show v
showRtkToken (L.Tk__qq_QVar v) = "qq_QVar " ++ show v
showRtkToken (L.Tk__qq_QVarId v) = "qq_QVarId " ++ show v
showRtkToken (L.Tk__qq_ModIdList v) = "qq_ModIdList " ++ show v
showRtkToken (L.Tk__qq_Con v) = "qq_Con " ++ show v
showRtkToken (L.Tk__qq_Var v) = "qq_Var " ++ show v
showRtkToken (L.Tk__qq_ImportList v) = "qq_ImportList " ++ show v
showRtkToken (L.Tk__qq_ImpDeclList v) = "qq_ImpDeclList " ++ show v
showRtkToken (L.Tk__qq_Body v) = "qq_Body " ++ show v
showRtkToken (L.Tk__qq_Export v) = "qq_Export " ++ show v
showRtkToken (L.Tk__qq_ExportsList v) = "qq_ExportsList " ++ show v
showRtkToken (L.Tk__qq_ExportsOpt v) = "qq_ExportsOpt " ++ show v
showRtkToken (L.Tk__qq_Module v) = "qq_Module " ++ show v
showRtkToken (L.Tk__qq_Haskell v) = "qq_Haskell " ++ show v

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

-- Recover a token's payload from the whole positioned token: %token
-- bindings keep the L.PosToken so semantic actions can read its position
tkVal_th :: L.PosToken -> String
tkVal_th (L.PosToken _ (L.Tk__th v)) = v
tkVal_th t = error ("rtk internal error: token th expected, got " ++ showRtkToken (L.ptToken t))
tkVal_ncomment :: L.PosToken -> String
tkVal_ncomment (L.PosToken _ (L.Tk__ncomment v)) = v
tkVal_ncomment t = error ("rtk internal error: token ncomment expected, got " ++ showRtkToken (L.ptToken t))
tkVal_whitespace :: L.PosToken -> String
tkVal_whitespace (L.PosToken _ (L.Tk__whitespace v)) = v
tkVal_whitespace t = error ("rtk internal error: token whitespace expected, got " ++ showRtkToken (L.ptToken t))
tkVal_integer :: L.PosToken -> String
tkVal_integer (L.PosToken _ (L.Tk__integer v)) = v
tkVal_integer t = error ("rtk internal error: token integer expected, got " ++ showRtkToken (L.ptToken t))
tkVal_hexadecimal :: L.PosToken -> String
tkVal_hexadecimal (L.PosToken _ (L.Tk__hexadecimal v)) = v
tkVal_hexadecimal t = error ("rtk internal error: token hexadecimal expected, got " ++ showRtkToken (L.ptToken t))
tkVal_octal :: L.PosToken -> String
tkVal_octal (L.PosToken _ (L.Tk__octal v)) = v
tkVal_octal t = error ("rtk internal error: token octal expected, got " ++ showRtkToken (L.ptToken t))
tkVal_decimal :: L.PosToken -> String
tkVal_decimal (L.PosToken _ (L.Tk__decimal v)) = v
tkVal_decimal t = error ("rtk internal error: token decimal expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_QOp :: L.PosToken -> String
tkVal_qq_QOp (L.PosToken _ (L.Tk__qq_QOp v)) = v
tkVal_qq_QOp t = error ("rtk internal error: token qq_QOp expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Op :: L.PosToken -> String
tkVal_qq_Op (L.PosToken _ (L.Tk__qq_Op v)) = v
tkVal_qq_Op t = error ("rtk internal error: token qq_Op expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TyCls :: L.PosToken -> String
tkVal_qq_TyCls (L.PosToken _ (L.Tk__qq_TyCls v)) = v
tkVal_qq_TyCls t = error ("rtk internal error: token qq_TyCls expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ModId :: L.PosToken -> String
tkVal_qq_ModId (L.PosToken _ (L.Tk__qq_ModId v)) = v
tkVal_qq_ModId t = error ("rtk internal error: token qq_ModId expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TyCon :: L.PosToken -> String
tkVal_qq_TyCon (L.PosToken _ (L.Tk__qq_TyCon v)) = v
tkVal_qq_TyCon t = error ("rtk internal error: token qq_TyCon expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TyVar :: L.PosToken -> String
tkVal_qq_TyVar (L.PosToken _ (L.Tk__qq_TyVar v)) = v
tkVal_qq_TyVar t = error ("rtk internal error: token qq_TyVar expected, got " ++ showRtkToken (L.ptToken t))
tkVal_varid :: L.PosToken -> String
tkVal_varid (L.PosToken _ (L.Tk__varid v)) = v
tkVal_varid t = error ("rtk internal error: token varid expected, got " ++ showRtkToken (L.ptToken t))
tkVal_conid :: L.PosToken -> String
tkVal_conid (L.PosToken _ (L.Tk__conid v)) = v
tkVal_conid t = error ("rtk internal error: token conid expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TyVars :: L.PosToken -> String
tkVal_qq_TyVars (L.PosToken _ (L.Tk__qq_TyVars v)) = v
tkVal_qq_TyVars t = error ("rtk internal error: token qq_TyVars expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_SimpleType :: L.PosToken -> String
tkVal_qq_SimpleType (L.PosToken _ (L.Tk__qq_SimpleType v)) = v
tkVal_qq_SimpleType t = error ("rtk internal error: token qq_SimpleType expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TypeList :: L.PosToken -> String
tkVal_qq_TypeList (L.PosToken _ (L.Tk__qq_TypeList v)) = v
tkVal_qq_TypeList t = error ("rtk internal error: token qq_TypeList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_GTyCon :: L.PosToken -> String
tkVal_qq_GTyCon (L.PosToken _ (L.Tk__qq_GTyCon v)) = v
tkVal_qq_GTyCon t = error ("rtk internal error: token qq_GTyCon expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_AType :: L.PosToken -> String
tkVal_qq_AType (L.PosToken _ (L.Tk__qq_AType v)) = v
tkVal_qq_AType t = error ("rtk internal error: token qq_AType expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ATypeList :: L.PosToken -> String
tkVal_qq_ATypeList (L.PosToken _ (L.Tk__qq_ATypeList v)) = v
tkVal_qq_ATypeList t = error ("rtk internal error: token qq_ATypeList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_BType :: L.PosToken -> String
tkVal_qq_BType (L.PosToken _ (L.Tk__qq_BType v)) = v
tkVal_qq_BType t = error ("rtk internal error: token qq_BType expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Type :: L.PosToken -> String
tkVal_qq_Type (L.PosToken _ (L.Tk__qq_Type v)) = v
tkVal_qq_Type t = error ("rtk internal error: token qq_Type expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Class :: L.PosToken -> String
tkVal_qq_Class (L.PosToken _ (L.Tk__qq_Class v)) = v
tkVal_qq_Class t = error ("rtk internal error: token qq_Class expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ClassList :: L.PosToken -> String
tkVal_qq_ClassList (L.PosToken _ (L.Tk__qq_ClassList v)) = v
tkVal_qq_ClassList t = error ("rtk internal error: token qq_ClassList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Context :: L.PosToken -> String
tkVal_qq_Context (L.PosToken _ (L.Tk__qq_Context v)) = v
tkVal_qq_Context t = error ("rtk internal error: token qq_Context expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_DClass :: L.PosToken -> String
tkVal_qq_DClass (L.PosToken _ (L.Tk__qq_DClass v)) = v
tkVal_qq_DClass t = error ("rtk internal error: token qq_DClass expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_DClassList :: L.PosToken -> String
tkVal_qq_DClassList (L.PosToken _ (L.Tk__qq_DClassList v)) = v
tkVal_qq_DClassList t = error ("rtk internal error: token qq_DClassList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Deriving :: L.PosToken -> String
tkVal_qq_Deriving (L.PosToken _ (L.Tk__qq_Deriving v)) = v
tkVal_qq_Deriving t = error ("rtk internal error: token qq_Deriving expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptDeriving :: L.PosToken -> String
tkVal_qq_OptDeriving (L.PosToken _ (L.Tk__qq_OptDeriving v)) = v
tkVal_qq_OptDeriving t = error ("rtk internal error: token qq_OptDeriving expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Vars :: L.PosToken -> String
tkVal_qq_Vars (L.PosToken _ (L.Tk__qq_Vars v)) = v
tkVal_qq_Vars t = error ("rtk internal error: token qq_Vars expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_FieldDecl :: L.PosToken -> String
tkVal_qq_FieldDecl (L.PosToken _ (L.Tk__qq_FieldDecl v)) = v
tkVal_qq_FieldDecl t = error ("rtk internal error: token qq_FieldDecl expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_FieldDeclList :: L.PosToken -> String
tkVal_qq_FieldDeclList (L.PosToken _ (L.Tk__qq_FieldDeclList v)) = v
tkVal_qq_FieldDeclList t = error ("rtk internal error: token qq_FieldDeclList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Constr :: L.PosToken -> String
tkVal_qq_Constr (L.PosToken _ (L.Tk__qq_Constr v)) = v
tkVal_qq_Constr t = error ("rtk internal error: token qq_Constr expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Constrs :: L.PosToken -> String
tkVal_qq_Constrs (L.PosToken _ (L.Tk__qq_Constrs v)) = v
tkVal_qq_Constrs t = error ("rtk internal error: token qq_Constrs expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_GdRhs :: L.PosToken -> String
tkVal_qq_GdRhs (L.PosToken _ (L.Tk__qq_GdRhs v)) = v
tkVal_qq_GdRhs t = error ("rtk internal error: token qq_GdRhs expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ExpI :: L.PosToken -> String
tkVal_qq_ExpI (L.PosToken _ (L.Tk__qq_ExpI v)) = v
tkVal_qq_ExpI t = error ("rtk internal error: token qq_ExpI expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Exp :: L.PosToken -> String
tkVal_qq_Exp (L.PosToken _ (L.Tk__qq_Exp v)) = v
tkVal_qq_Exp t = error ("rtk internal error: token qq_Exp expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptExpTypeSignature :: L.PosToken -> String
tkVal_qq_OptExpTypeSignature (L.PosToken _ (L.Tk__qq_OptExpTypeSignature v)) = v
tkVal_qq_OptExpTypeSignature t = error ("rtk internal error: token qq_OptExpTypeSignature expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Gd :: L.PosToken -> String
tkVal_qq_Gd (L.PosToken _ (L.Tk__qq_Gd v)) = v
tkVal_qq_Gd t = error ("rtk internal error: token qq_Gd expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptGdRhs :: L.PosToken -> String
tkVal_qq_OptGdRhs (L.PosToken _ (L.Tk__qq_OptGdRhs v)) = v
tkVal_qq_OptGdRhs t = error ("rtk internal error: token qq_OptGdRhs expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Rhs :: L.PosToken -> String
tkVal_qq_Rhs (L.PosToken _ (L.Tk__qq_Rhs v)) = v
tkVal_qq_Rhs t = error ("rtk internal error: token qq_Rhs expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Decls :: L.PosToken -> String
tkVal_qq_Decls (L.PosToken _ (L.Tk__qq_Decls v)) = v
tkVal_qq_Decls t = error ("rtk internal error: token qq_Decls expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_DeclList :: L.PosToken -> String
tkVal_qq_DeclList (L.PosToken _ (L.Tk__qq_DeclList v)) = v
tkVal_qq_DeclList t = error ("rtk internal error: token qq_DeclList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptWhere :: L.PosToken -> String
tkVal_qq_OptWhere (L.PosToken _ (L.Tk__qq_OptWhere v)) = v
tkVal_qq_OptWhere t = error ("rtk internal error: token qq_OptWhere expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Pat :: L.PosToken -> String
tkVal_qq_Pat (L.PosToken _ (L.Tk__qq_Pat v)) = v
tkVal_qq_Pat t = error ("rtk internal error: token qq_Pat expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_FunLhs :: L.PosToken -> String
tkVal_qq_FunLhs (L.PosToken _ (L.Tk__qq_FunLhs v)) = v
tkVal_qq_FunLhs t = error ("rtk internal error: token qq_FunLhs expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Fixity :: L.PosToken -> String
tkVal_qq_Fixity (L.PosToken _ (L.Tk__qq_Fixity v)) = v
tkVal_qq_Fixity t = error ("rtk internal error: token qq_Fixity expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Ops :: L.PosToken -> String
tkVal_qq_Ops (L.PosToken _ (L.Tk__qq_Ops v)) = v
tkVal_qq_Ops t = error ("rtk internal error: token qq_Ops expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptInteger :: L.PosToken -> String
tkVal_qq_OptInteger (L.PosToken _ (L.Tk__qq_OptInteger v)) = v
tkVal_qq_OptInteger t = error ("rtk internal error: token qq_OptInteger expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_GenDecl :: L.PosToken -> String
tkVal_qq_GenDecl (L.PosToken _ (L.Tk__qq_GenDecl v)) = v
tkVal_qq_GenDecl t = error ("rtk internal error: token qq_GenDecl expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptContext :: L.PosToken -> String
tkVal_qq_OptContext (L.PosToken _ (L.Tk__qq_OptContext v)) = v
tkVal_qq_OptContext t = error ("rtk internal error: token qq_OptContext expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Decl :: L.PosToken -> String
tkVal_qq_Decl (L.PosToken _ (L.Tk__qq_Decl v)) = v
tkVal_qq_Decl t = error ("rtk internal error: token qq_Decl expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TopDecl :: L.PosToken -> String
tkVal_qq_TopDecl (L.PosToken _ (L.Tk__qq_TopDecl v)) = v
tkVal_qq_TopDecl t = error ("rtk internal error: token qq_TopDecl expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TopDecls :: L.PosToken -> String
tkVal_qq_TopDecls (L.PosToken _ (L.Tk__qq_TopDecls v)) = v
tkVal_qq_TopDecls t = error ("rtk internal error: token qq_TopDecls expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ImpDecl :: L.PosToken -> String
tkVal_qq_ImpDecl (L.PosToken _ (L.Tk__qq_ImpDecl v)) = v
tkVal_qq_ImpDecl t = error ("rtk internal error: token qq_ImpDecl expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptImpSpec :: L.PosToken -> String
tkVal_qq_OptImpSpec (L.PosToken _ (L.Tk__qq_OptImpSpec v)) = v
tkVal_qq_OptImpSpec t = error ("rtk internal error: token qq_OptImpSpec expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptQualifiedAs :: L.PosToken -> String
tkVal_qq_OptQualifiedAs (L.PosToken _ (L.Tk__qq_OptQualifiedAs v)) = v
tkVal_qq_OptQualifiedAs t = error ("rtk internal error: token qq_OptQualifiedAs expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptQualified :: L.PosToken -> String
tkVal_qq_OptQualified (L.PosToken _ (L.Tk__qq_OptQualified v)) = v
tkVal_qq_OptQualified t = error ("rtk internal error: token qq_OptQualified expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Import :: L.PosToken -> String
tkVal_qq_Import (L.PosToken _ (L.Tk__qq_Import v)) = v
tkVal_qq_Import t = error ("rtk internal error: token qq_Import expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_QVarList :: L.PosToken -> String
tkVal_qq_QVarList (L.PosToken _ (L.Tk__qq_QVarList v)) = v
tkVal_qq_QVarList t = error ("rtk internal error: token qq_QVarList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_CNameList :: L.PosToken -> String
tkVal_qq_CNameList (L.PosToken _ (L.Tk__qq_CNameList v)) = v
tkVal_qq_CNameList t = error ("rtk internal error: token qq_CNameList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_CName :: L.PosToken -> String
tkVal_qq_CName (L.PosToken _ (L.Tk__qq_CName v)) = v
tkVal_qq_CName t = error ("rtk internal error: token qq_CName expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_QTyCon :: L.PosToken -> String
tkVal_qq_QTyCon (L.PosToken _ (L.Tk__qq_QTyCon v)) = v
tkVal_qq_QTyCon t = error ("rtk internal error: token qq_QTyCon expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_QTyCls :: L.PosToken -> String
tkVal_qq_QTyCls (L.PosToken _ (L.Tk__qq_QTyCls v)) = v
tkVal_qq_QTyCls t = error ("rtk internal error: token qq_QTyCls expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_QVar :: L.PosToken -> String
tkVal_qq_QVar (L.PosToken _ (L.Tk__qq_QVar v)) = v
tkVal_qq_QVar t = error ("rtk internal error: token qq_QVar expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_QVarId :: L.PosToken -> String
tkVal_qq_QVarId (L.PosToken _ (L.Tk__qq_QVarId v)) = v
tkVal_qq_QVarId t = error ("rtk internal error: token qq_QVarId expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ModIdList :: L.PosToken -> String
tkVal_qq_ModIdList (L.PosToken _ (L.Tk__qq_ModIdList v)) = v
tkVal_qq_ModIdList t = error ("rtk internal error: token qq_ModIdList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Con :: L.PosToken -> String
tkVal_qq_Con (L.PosToken _ (L.Tk__qq_Con v)) = v
tkVal_qq_Con t = error ("rtk internal error: token qq_Con expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Var :: L.PosToken -> String
tkVal_qq_Var (L.PosToken _ (L.Tk__qq_Var v)) = v
tkVal_qq_Var t = error ("rtk internal error: token qq_Var expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ImportList :: L.PosToken -> String
tkVal_qq_ImportList (L.PosToken _ (L.Tk__qq_ImportList v)) = v
tkVal_qq_ImportList t = error ("rtk internal error: token qq_ImportList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ImpDeclList :: L.PosToken -> String
tkVal_qq_ImpDeclList (L.PosToken _ (L.Tk__qq_ImpDeclList v)) = v
tkVal_qq_ImpDeclList t = error ("rtk internal error: token qq_ImpDeclList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Body :: L.PosToken -> String
tkVal_qq_Body (L.PosToken _ (L.Tk__qq_Body v)) = v
tkVal_qq_Body t = error ("rtk internal error: token qq_Body expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Export :: L.PosToken -> String
tkVal_qq_Export (L.PosToken _ (L.Tk__qq_Export v)) = v
tkVal_qq_Export t = error ("rtk internal error: token qq_Export expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ExportsList :: L.PosToken -> String
tkVal_qq_ExportsList (L.PosToken _ (L.Tk__qq_ExportsList v)) = v
tkVal_qq_ExportsList t = error ("rtk internal error: token qq_ExportsList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ExportsOpt :: L.PosToken -> String
tkVal_qq_ExportsOpt (L.PosToken _ (L.Tk__qq_ExportsOpt v)) = v
tkVal_qq_ExportsOpt t = error ("rtk internal error: token qq_ExportsOpt expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Module :: L.PosToken -> String
tkVal_qq_Module (L.PosToken _ (L.Tk__qq_Module v)) = v
tkVal_qq_Module t = error ("rtk internal error: token qq_Module expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Haskell :: L.PosToken -> String
tkVal_qq_Haskell (L.PosToken _ (L.Tk__qq_Haskell v)) = v
tkVal_qq_Haskell t = error ("rtk internal error: token qq_Haskell expected, got " ++ showRtkToken (L.ptToken t))

data Haskell = Ctr__Haskell__0 RtkPos Haskell |
               Ctr__Haskell__1 RtkPos AType |
               Ctr__Haskell__2 RtkPos ATypeList |
               Ctr__Haskell__3 RtkPos BType |
               Ctr__Haskell__4 RtkPos Body |
               Ctr__Haskell__5 RtkPos CName |
               Ctr__Haskell__6 RtkPos CNameList |
               Ctr__Haskell__7 RtkPos Class |
               Ctr__Haskell__8 RtkPos ClassList |
               Ctr__Haskell__9 RtkPos Con |
               Ctr__Haskell__10 RtkPos Constr |
               Ctr__Haskell__11 RtkPos Constrs |
               Ctr__Haskell__12 RtkPos Context |
               Ctr__Haskell__13 RtkPos DClass |
               Ctr__Haskell__14 RtkPos DClassList |
               Ctr__Haskell__15 RtkPos Decl |
               Ctr__Haskell__16 RtkPos DeclList |
               Ctr__Haskell__17 RtkPos Decls |
               Ctr__Haskell__18 RtkPos Deriving |
               Ctr__Haskell__19 RtkPos Exp |
               Ctr__Haskell__20 RtkPos ExpI |
               Ctr__Haskell__21 RtkPos Export |
               Ctr__Haskell__22 RtkPos ExportsList |
               Ctr__Haskell__23 RtkPos ExportsOpt |
               Ctr__Haskell__24 RtkPos FieldDecl |
               Ctr__Haskell__25 RtkPos FieldDeclList |
               Ctr__Haskell__26 RtkPos Fixity |
               Ctr__Haskell__27 RtkPos FunLhs |
               Ctr__Haskell__28 RtkPos GTyCon |
               Ctr__Haskell__29 RtkPos Gd |
               Ctr__Haskell__30 RtkPos GdRhs |
               Ctr__Haskell__31 RtkPos GenDecl |
               Ctr__Haskell__32 RtkPos ImpDecl |
               Ctr__Haskell__33 RtkPos ImpDeclList |
               Ctr__Haskell__34 RtkPos Import |
               Ctr__Haskell__35 RtkPos ImportList |
               Ctr__Haskell__36 RtkPos ModId |
               Ctr__Haskell__37 RtkPos ModIdList |
               Ctr__Haskell__38 RtkPos Module |
               Ctr__Haskell__39 RtkPos Op |
               Ctr__Haskell__40 RtkPos Ops |
               Ctr__Haskell__41 RtkPos OptContext |
               Ctr__Haskell__42 RtkPos OptDeriving |
               Ctr__Haskell__43 RtkPos OptExpTypeSignature |
               Ctr__Haskell__44 RtkPos OptGdRhs |
               Ctr__Haskell__45 RtkPos OptImpSpec |
               Ctr__Haskell__46 RtkPos OptInteger |
               Ctr__Haskell__47 RtkPos OptQualified |
               Ctr__Haskell__48 RtkPos OptQualifiedAs |
               Ctr__Haskell__49 RtkPos OptWhere |
               Ctr__Haskell__50 RtkPos Pat |
               Ctr__Haskell__51 RtkPos QOp |
               Ctr__Haskell__52 RtkPos QTyCls |
               Ctr__Haskell__53 RtkPos QTyCon |
               Ctr__Haskell__54 RtkPos QVar |
               Ctr__Haskell__55 RtkPos QVarId |
               Ctr__Haskell__56 RtkPos QVarList |
               Ctr__Haskell__57 RtkPos Rhs |
               Ctr__Haskell__58 RtkPos SimpleType |
               Ctr__Haskell__59 RtkPos TopDecl |
               Ctr__Haskell__60 RtkPos TopDecls |
               Ctr__Haskell__61 RtkPos TyCls |
               Ctr__Haskell__62 RtkPos TyCon |
               Ctr__Haskell__63 RtkPos TyVar |
               Ctr__Haskell__64 RtkPos TyVars |
               Ctr__Haskell__65 RtkPos Type |
               Ctr__Haskell__66 RtkPos TypeList |
               Ctr__Haskell__67 RtkPos Var |
               Ctr__Haskell__68 RtkPos Vars |
               Anti_Haskell String |
               Ctr__Haskell__69 RtkPos Module
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Haskell where
    rtkPosOf (Ctr__Haskell__0 p _) = p
    rtkPosOf (Ctr__Haskell__1 p _) = p
    rtkPosOf (Ctr__Haskell__2 p _) = p
    rtkPosOf (Ctr__Haskell__3 p _) = p
    rtkPosOf (Ctr__Haskell__4 p _) = p
    rtkPosOf (Ctr__Haskell__5 p _) = p
    rtkPosOf (Ctr__Haskell__6 p _) = p
    rtkPosOf (Ctr__Haskell__7 p _) = p
    rtkPosOf (Ctr__Haskell__8 p _) = p
    rtkPosOf (Ctr__Haskell__9 p _) = p
    rtkPosOf (Ctr__Haskell__10 p _) = p
    rtkPosOf (Ctr__Haskell__11 p _) = p
    rtkPosOf (Ctr__Haskell__12 p _) = p
    rtkPosOf (Ctr__Haskell__13 p _) = p
    rtkPosOf (Ctr__Haskell__14 p _) = p
    rtkPosOf (Ctr__Haskell__15 p _) = p
    rtkPosOf (Ctr__Haskell__16 p _) = p
    rtkPosOf (Ctr__Haskell__17 p _) = p
    rtkPosOf (Ctr__Haskell__18 p _) = p
    rtkPosOf (Ctr__Haskell__19 p _) = p
    rtkPosOf (Ctr__Haskell__20 p _) = p
    rtkPosOf (Ctr__Haskell__21 p _) = p
    rtkPosOf (Ctr__Haskell__22 p _) = p
    rtkPosOf (Ctr__Haskell__23 p _) = p
    rtkPosOf (Ctr__Haskell__24 p _) = p
    rtkPosOf (Ctr__Haskell__25 p _) = p
    rtkPosOf (Ctr__Haskell__26 p _) = p
    rtkPosOf (Ctr__Haskell__27 p _) = p
    rtkPosOf (Ctr__Haskell__28 p _) = p
    rtkPosOf (Ctr__Haskell__29 p _) = p
    rtkPosOf (Ctr__Haskell__30 p _) = p
    rtkPosOf (Ctr__Haskell__31 p _) = p
    rtkPosOf (Ctr__Haskell__32 p _) = p
    rtkPosOf (Ctr__Haskell__33 p _) = p
    rtkPosOf (Ctr__Haskell__34 p _) = p
    rtkPosOf (Ctr__Haskell__35 p _) = p
    rtkPosOf (Ctr__Haskell__36 p _) = p
    rtkPosOf (Ctr__Haskell__37 p _) = p
    rtkPosOf (Ctr__Haskell__38 p _) = p
    rtkPosOf (Ctr__Haskell__39 p _) = p
    rtkPosOf (Ctr__Haskell__40 p _) = p
    rtkPosOf (Ctr__Haskell__41 p _) = p
    rtkPosOf (Ctr__Haskell__42 p _) = p
    rtkPosOf (Ctr__Haskell__43 p _) = p
    rtkPosOf (Ctr__Haskell__44 p _) = p
    rtkPosOf (Ctr__Haskell__45 p _) = p
    rtkPosOf (Ctr__Haskell__46 p _) = p
    rtkPosOf (Ctr__Haskell__47 p _) = p
    rtkPosOf (Ctr__Haskell__48 p _) = p
    rtkPosOf (Ctr__Haskell__49 p _) = p
    rtkPosOf (Ctr__Haskell__50 p _) = p
    rtkPosOf (Ctr__Haskell__51 p _) = p
    rtkPosOf (Ctr__Haskell__52 p _) = p
    rtkPosOf (Ctr__Haskell__53 p _) = p
    rtkPosOf (Ctr__Haskell__54 p _) = p
    rtkPosOf (Ctr__Haskell__55 p _) = p
    rtkPosOf (Ctr__Haskell__56 p _) = p
    rtkPosOf (Ctr__Haskell__57 p _) = p
    rtkPosOf (Ctr__Haskell__58 p _) = p
    rtkPosOf (Ctr__Haskell__59 p _) = p
    rtkPosOf (Ctr__Haskell__60 p _) = p
    rtkPosOf (Ctr__Haskell__61 p _) = p
    rtkPosOf (Ctr__Haskell__62 p _) = p
    rtkPosOf (Ctr__Haskell__63 p _) = p
    rtkPosOf (Ctr__Haskell__64 p _) = p
    rtkPosOf (Ctr__Haskell__65 p _) = p
    rtkPosOf (Ctr__Haskell__66 p _) = p
    rtkPosOf (Ctr__Haskell__67 p _) = p
    rtkPosOf (Ctr__Haskell__68 p _) = p
    rtkPosOf (Anti_Haskell _) = rtkNoPos
    rtkPosOf (Ctr__Haskell__69 p _) = p
data AType = Anti_AType String |
             Ctr__AType__0 RtkPos TyVar |
             Ctr__AType__1 RtkPos GTyCon |
             Ctr__AType__2 RtkPos TypeList |
             Ctr__AType__3 RtkPos Rule_41
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf AType where
    rtkPosOf (Anti_AType _) = rtkNoPos
    rtkPosOf (Ctr__AType__0 p _) = p
    rtkPosOf (Ctr__AType__1 p _) = p
    rtkPosOf (Ctr__AType__2 p _) = p
    rtkPosOf (Ctr__AType__3 p _) = p
type ATypeList = [AType]
data BType = Anti_BType String |
             Ctr__BType__0 RtkPos Rule_39 AType
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf BType where
    rtkPosOf (Anti_BType _) = rtkNoPos
    rtkPosOf (Ctr__BType__0 p _ _) = p
data Body = Anti_Body String |
            Ctr__Body__0 RtkPos Rule_7
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Body where
    rtkPosOf (Anti_Body _) = rtkNoPos
    rtkPosOf (Ctr__Body__0 p _) = p
data CName = Anti_CName String |
             Ctr__CName__0 RtkPos Var |
             Ctr__CName__1 RtkPos Con
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf CName where
    rtkPosOf (Anti_CName _) = rtkNoPos
    rtkPosOf (Ctr__CName__0 p _) = p
    rtkPosOf (Ctr__CName__1 p _) = p
data CNameList = Anti_CNameList String |
                 Ctr__CNameList__0 RtkPos Rule_14
                 deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf CNameList where
    rtkPosOf (Anti_CNameList _) = rtkNoPos
    rtkPosOf (Ctr__CNameList__0 p _) = p
data Class = Anti_Class String |
             Ctr__Class__0 RtkPos QTyCls TyVar |
             Ctr__Class__1 RtkPos QTyCls TyVar ATypeList
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Class where
    rtkPosOf (Anti_Class _) = rtkNoPos
    rtkPosOf (Ctr__Class__0 p _ _) = p
    rtkPosOf (Ctr__Class__1 p _ _ _) = p
data ClassList = Anti_ClassList String |
                 Ctr__ClassList__0 RtkPos Rule_36
                 deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ClassList where
    rtkPosOf (Anti_ClassList _) = rtkNoPos
    rtkPosOf (Ctr__ClassList__0 p _) = p
data Con = Anti_Con String |
           Ctr__Con__0 RtkPos String
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Con where
    rtkPosOf (Anti_Con _) = rtkNoPos
    rtkPosOf (Ctr__Con__0 p _) = p
data Constr = Anti_Constr String |
              Ctr__Constr__0 RtkPos Con FieldDeclList
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Constr where
    rtkPosOf (Anti_Constr _) = rtkNoPos
    rtkPosOf (Ctr__Constr__0 p _ _) = p
data Constrs = Anti_Constrs String |
               Ctr__Constrs__0 RtkPos Rule_30
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Constrs where
    rtkPosOf (Anti_Constrs _) = rtkNoPos
    rtkPosOf (Ctr__Constrs__0 p _) = p
data Context = Anti_Context String |
               Ctr__Context__0 RtkPos Class |
               Ctr__Context__1 RtkPos ClassList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Context where
    rtkPosOf (Anti_Context _) = rtkNoPos
    rtkPosOf (Ctr__Context__0 p _) = p
    rtkPosOf (Ctr__Context__1 p _) = p
data DClass = Anti_DClass String |
              Ctr__DClass__0 RtkPos QTyCls
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf DClass where
    rtkPosOf (Anti_DClass _) = rtkNoPos
    rtkPosOf (Ctr__DClass__0 p _) = p
data DClassList = Anti_DClassList String |
                  Ctr__DClassList__0 RtkPos Rule_35
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf DClassList where
    rtkPosOf (Anti_DClassList _) = rtkNoPos
    rtkPosOf (Ctr__DClassList__0 p _) = p
data Decl = Anti_Decl String |
            Ctr__Decl__0 RtkPos GenDecl |
            Ctr__Decl__1 RtkPos Rule_22 Rhs
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Decl where
    rtkPosOf (Anti_Decl _) = rtkNoPos
    rtkPosOf (Ctr__Decl__0 p _) = p
    rtkPosOf (Ctr__Decl__1 p _ _) = p
data DeclList = Anti_DeclList String |
                Ctr__DeclList__0 RtkPos Rule_26
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf DeclList where
    rtkPosOf (Anti_DeclList _) = rtkNoPos
    rtkPosOf (Ctr__DeclList__0 p _) = p
data Decls = Anti_Decls String |
             Ctr__Decls__0 RtkPos DeclList
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Decls where
    rtkPosOf (Anti_Decls _) = rtkNoPos
    rtkPosOf (Ctr__Decls__0 p _) = p
data Deriving = Anti_Deriving String |
                Ctr__Deriving__0 RtkPos Rule_34
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Deriving where
    rtkPosOf (Anti_Deriving _) = rtkNoPos
    rtkPosOf (Ctr__Deriving__0 p _) = p
data Exp = Anti_Exp String |
           Ctr__Exp__0 RtkPos ExpI OptExpTypeSignature
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Exp where
    rtkPosOf (Anti_Exp _) = rtkNoPos
    rtkPosOf (Ctr__Exp__0 p _ _) = p
data ExpI = Anti_ExpI String |
            Ctr__ExpI__0 RtkPos ExpI Rule_28
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ExpI where
    rtkPosOf (Anti_ExpI _) = rtkNoPos
    rtkPosOf (Ctr__ExpI__0 p _ _) = p
data Export = Anti_Export String |
              Ctr__Export__0 RtkPos ModId |
              Ctr__Export__1 RtkPos QVar |
              Ctr__Export__2 RtkPos QTyCon Rule_3 |
              Ctr__Export__3 RtkPos QTyCls Rule_5
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Export where
    rtkPosOf (Anti_Export _) = rtkNoPos
    rtkPosOf (Ctr__Export__0 p _) = p
    rtkPosOf (Ctr__Export__1 p _) = p
    rtkPosOf (Ctr__Export__2 p _ _) = p
    rtkPosOf (Ctr__Export__3 p _ _) = p
data ExportsList = Anti_ExportsList String |
                   Ctr__ExportsList__0 RtkPos Rule_2
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ExportsList where
    rtkPosOf (Anti_ExportsList _) = rtkNoPos
    rtkPosOf (Ctr__ExportsList__0 p _) = p
data ExportsOpt = Anti_ExportsOpt String |
                  Ctr__ExportsOpt__0 RtkPos |
                  Ctr__ExportsOpt__1 RtkPos Rule_0
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ExportsOpt where
    rtkPosOf (Anti_ExportsOpt _) = rtkNoPos
    rtkPosOf (Ctr__ExportsOpt__0 p) = p
    rtkPosOf (Ctr__ExportsOpt__1 p _) = p
data FieldDecl = Anti_FieldDecl String |
                 Ctr__FieldDecl__0 RtkPos Vars Rule_32
                 deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf FieldDecl where
    rtkPosOf (Anti_FieldDecl _) = rtkNoPos
    rtkPosOf (Ctr__FieldDecl__0 p _ _) = p
data FieldDeclList = Anti_FieldDeclList String |
                     Ctr__FieldDeclList__0 RtkPos Rule_31
                     deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf FieldDeclList where
    rtkPosOf (Anti_FieldDeclList _) = rtkNoPos
    rtkPosOf (Ctr__FieldDeclList__0 p _) = p
data Fixity = Anti_Fixity String |
              Ctr__Fixity__0 RtkPos |
              Ctr__Fixity__1 RtkPos |
              Ctr__Fixity__2 RtkPos
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Fixity where
    rtkPosOf (Anti_Fixity _) = rtkNoPos
    rtkPosOf (Ctr__Fixity__0 p) = p
    rtkPosOf (Ctr__Fixity__1 p) = p
    rtkPosOf (Ctr__Fixity__2 p) = p
data FunLhs = Anti_FunLhs String |
              Ctr__FunLhs__0 RtkPos Var
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf FunLhs where
    rtkPosOf (Anti_FunLhs _) = rtkNoPos
    rtkPosOf (Ctr__FunLhs__0 p _) = p
data GTyCon = Anti_GTyCon String |
              Ctr__GTyCon__0 RtkPos QTyCon |
              Ctr__GTyCon__1 RtkPos
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf GTyCon where
    rtkPosOf (Anti_GTyCon _) = rtkNoPos
    rtkPosOf (Ctr__GTyCon__0 p _) = p
    rtkPosOf (Ctr__GTyCon__1 p) = p
data Gd = Anti_Gd String |
          Ctr__Gd__0 RtkPos |
          Ctr__Gd__1 RtkPos ExpI
          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Gd where
    rtkPosOf (Anti_Gd _) = rtkNoPos
    rtkPosOf (Ctr__Gd__0 p) = p
    rtkPosOf (Ctr__Gd__1 p _) = p
data GdRhs = Anti_GdRhs String |
             Ctr__GdRhs__0 RtkPos Gd Exp OptGdRhs
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf GdRhs where
    rtkPosOf (Anti_GdRhs _) = rtkNoPos
    rtkPosOf (Ctr__GdRhs__0 p _ _ _) = p
data GenDecl = Anti_GenDecl String |
               Ctr__GenDecl__0 RtkPos Vars OptContext Type |
               Ctr__GenDecl__1 RtkPos Fixity OptInteger Ops
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf GenDecl where
    rtkPosOf (Anti_GenDecl _) = rtkNoPos
    rtkPosOf (Ctr__GenDecl__0 p _ _ _) = p
    rtkPosOf (Ctr__GenDecl__1 p _ _ _) = p
data ImpDecl = Anti_ImpDecl String |
               Ctr__ImpDecl__0 RtkPos OptQualified ModId OptQualifiedAs Rule_20
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ImpDecl where
    rtkPosOf (Anti_ImpDecl _) = rtkNoPos
    rtkPosOf (Ctr__ImpDecl__0 p _ _ _ _) = p
data ImpDeclList = Anti_ImpDeclList String |
                   Ctr__ImpDeclList__0 RtkPos Rule_10
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ImpDeclList where
    rtkPosOf (Anti_ImpDeclList _) = rtkNoPos
    rtkPosOf (Ctr__ImpDeclList__0 p _) = p
data Import = Anti_Import String |
              Ctr__Import__0 RtkPos Var |
              Ctr__Import__1 RtkPos TyCon Rule_16
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Import where
    rtkPosOf (Anti_Import _) = rtkNoPos
    rtkPosOf (Ctr__Import__0 p _) = p
    rtkPosOf (Ctr__Import__1 p _ _) = p
data ImportList = Anti_ImportList String |
                  Ctr__ImportList__0 RtkPos Rule_11
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ImportList where
    rtkPosOf (Anti_ImportList _) = rtkNoPos
    rtkPosOf (Ctr__ImportList__0 p _) = p
data ModId = Anti_ModId String |
             Ctr__ModId__0 RtkPos String
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ModId where
    rtkPosOf (Anti_ModId _) = rtkNoPos
    rtkPosOf (Ctr__ModId__0 p _) = p
type ModIdList = [Rule_12]
data Module = Anti_Module String |
              Ctr__Module__0 RtkPos ModId ExportsOpt Body |
              Ctr__Module__1 RtkPos Body
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Module where
    rtkPosOf (Anti_Module _) = rtkNoPos
    rtkPosOf (Ctr__Module__0 p _ _ _) = p
    rtkPosOf (Ctr__Module__1 p _) = p
data Op = Anti_Op String |
          Ctr__Op__0 RtkPos String |
          Ctr__Op__1 RtkPos String
          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Op where
    rtkPosOf (Anti_Op _) = rtkNoPos
    rtkPosOf (Ctr__Op__0 p _) = p
    rtkPosOf (Ctr__Op__1 p _) = p
data Ops = Anti_Ops String |
           Ctr__Ops__0 RtkPos Rule_24
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Ops where
    rtkPosOf (Anti_Ops _) = rtkNoPos
    rtkPosOf (Ctr__Ops__0 p _) = p
data OptContext = Anti_OptContext String |
                  Ctr__OptContext__0 RtkPos |
                  Ctr__OptContext__1 RtkPos Rule_23
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptContext where
    rtkPosOf (Anti_OptContext _) = rtkNoPos
    rtkPosOf (Ctr__OptContext__0 p) = p
    rtkPosOf (Ctr__OptContext__1 p _) = p
data OptDeriving = Anti_OptDeriving String |
                   Ctr__OptDeriving__0 RtkPos |
                   Ctr__OptDeriving__1 RtkPos Deriving
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptDeriving where
    rtkPosOf (Anti_OptDeriving _) = rtkNoPos
    rtkPosOf (Ctr__OptDeriving__0 p) = p
    rtkPosOf (Ctr__OptDeriving__1 p _) = p
data OptExpTypeSignature = Anti_OptExpTypeSignature String |
                           Ctr__OptExpTypeSignature__0 RtkPos |
                           Ctr__OptExpTypeSignature__1 RtkPos Rule_27
                           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptExpTypeSignature where
    rtkPosOf (Anti_OptExpTypeSignature _) = rtkNoPos
    rtkPosOf (Ctr__OptExpTypeSignature__0 p) = p
    rtkPosOf (Ctr__OptExpTypeSignature__1 p _) = p
data OptGdRhs = Anti_OptGdRhs String |
                Ctr__OptGdRhs__0 RtkPos |
                Ctr__OptGdRhs__1 RtkPos GdRhs
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptGdRhs where
    rtkPosOf (Anti_OptGdRhs _) = rtkNoPos
    rtkPosOf (Ctr__OptGdRhs__0 p) = p
    rtkPosOf (Ctr__OptGdRhs__1 p _) = p
data OptImpSpec = Anti_OptImpSpec String |
                  Ctr__OptImpSpec__0 RtkPos ImportList Rule_19
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptImpSpec where
    rtkPosOf (Anti_OptImpSpec _) = rtkNoPos
    rtkPosOf (Ctr__OptImpSpec__0 p _ _) = p
data OptInteger = Anti_OptInteger String |
                  Ctr__OptInteger__0 RtkPos |
                  Ctr__OptInteger__1 RtkPos String
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptInteger where
    rtkPosOf (Anti_OptInteger _) = rtkNoPos
    rtkPosOf (Ctr__OptInteger__0 p) = p
    rtkPosOf (Ctr__OptInteger__1 p _) = p
data OptQualified = Anti_OptQualified String |
                    Ctr__OptQualified__0 RtkPos |
                    Ctr__OptQualified__1 RtkPos
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptQualified where
    rtkPosOf (Anti_OptQualified _) = rtkNoPos
    rtkPosOf (Ctr__OptQualified__0 p) = p
    rtkPosOf (Ctr__OptQualified__1 p) = p
data OptQualifiedAs = Anti_OptQualifiedAs String |
                      Ctr__OptQualifiedAs__0 RtkPos |
                      Ctr__OptQualifiedAs__1 RtkPos Rule_18
                      deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptQualifiedAs where
    rtkPosOf (Anti_OptQualifiedAs _) = rtkNoPos
    rtkPosOf (Ctr__OptQualifiedAs__0 p) = p
    rtkPosOf (Ctr__OptQualifiedAs__1 p _) = p
data OptWhere = Anti_OptWhere String |
                Ctr__OptWhere__0 RtkPos Decls
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptWhere where
    rtkPosOf (Anti_OptWhere _) = rtkNoPos
    rtkPosOf (Ctr__OptWhere__0 p _) = p
data Pat = Anti_Pat String |
           Ctr__Pat__0 RtkPos Con Rule_25
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Pat where
    rtkPosOf (Anti_Pat _) = rtkNoPos
    rtkPosOf (Ctr__Pat__0 p _ _) = p
data QOp = Anti_QOp String |
           Ctr__QOp__0 RtkPos ModIdList Op
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf QOp where
    rtkPosOf (Anti_QOp _) = rtkNoPos
    rtkPosOf (Ctr__QOp__0 p _ _) = p
data QTyCls = Anti_QTyCls String |
              Ctr__QTyCls__0 RtkPos ModIdList TyCls
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf QTyCls where
    rtkPosOf (Anti_QTyCls _) = rtkNoPos
    rtkPosOf (Ctr__QTyCls__0 p _ _) = p
data QTyCon = Anti_QTyCon String |
              Ctr__QTyCon__0 RtkPos ModIdList TyCon
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf QTyCon where
    rtkPosOf (Anti_QTyCon _) = rtkNoPos
    rtkPosOf (Ctr__QTyCon__0 p _ _) = p
data QVar = Anti_QVar String |
            Ctr__QVar__0 RtkPos QVarId
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf QVar where
    rtkPosOf (Anti_QVar _) = rtkNoPos
    rtkPosOf (Ctr__QVar__0 p _) = p
data QVarId = Anti_QVarId String |
              Ctr__QVarId__0 RtkPos ModIdList String
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf QVarId where
    rtkPosOf (Anti_QVarId _) = rtkNoPos
    rtkPosOf (Ctr__QVarId__0 p _ _) = p
data QVarList = Anti_QVarList String |
                Ctr__QVarList__0 RtkPos Rule_15
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf QVarList where
    rtkPosOf (Anti_QVarList _) = rtkNoPos
    rtkPosOf (Ctr__QVarList__0 p _) = p
data Rhs = Anti_Rhs String |
           Ctr__Rhs__0 RtkPos Exp OptWhere |
           Ctr__Rhs__1 RtkPos GdRhs OptWhere
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rhs where
    rtkPosOf (Anti_Rhs _) = rtkNoPos
    rtkPosOf (Ctr__Rhs__0 p _ _) = p
    rtkPosOf (Ctr__Rhs__1 p _ _) = p
data Rule_0 = Ctr__Rule_0__0 RtkPos ExportsList Rule_1
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_0 where
    rtkPosOf (Ctr__Rule_0__0 p _ _) = p
data Rule_1 = Ctr__Rule_1__0 RtkPos |
              Ctr__Rule_1__1 RtkPos
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_1 where
    rtkPosOf (Ctr__Rule_1__0 p) = p
    rtkPosOf (Ctr__Rule_1__1 p) = p
type Rule_10 = [ImpDecl]
type Rule_11 = [Import]
data Rule_12 = Anti_Rule_12 String |
               Ctr__Rule_12__1 RtkPos ModId
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_12 where
    rtkPosOf (Anti_Rule_12 _) = rtkNoPos
    rtkPosOf (Ctr__Rule_12__1 p _) = p
type Rule_14 = [CName]
type Rule_15 = [QVar]
data Rule_16 = Ctr__Rule_16__0 RtkPos |
               Ctr__Rule_16__1 RtkPos Rule_17
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_16 where
    rtkPosOf (Ctr__Rule_16__0 p) = p
    rtkPosOf (Ctr__Rule_16__1 p _) = p
data Rule_17 = Ctr__Rule_17__0 RtkPos |
               Ctr__Rule_17__1 RtkPos CNameList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_17 where
    rtkPosOf (Ctr__Rule_17__0 p) = p
    rtkPosOf (Ctr__Rule_17__1 p _) = p
data Rule_18 = Ctr__Rule_18__0 RtkPos ModId
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_18 where
    rtkPosOf (Ctr__Rule_18__0 p _) = p
data Rule_19 = Ctr__Rule_19__0 RtkPos |
               Ctr__Rule_19__1 RtkPos
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_19 where
    rtkPosOf (Ctr__Rule_19__0 p) = p
    rtkPosOf (Ctr__Rule_19__1 p) = p
type Rule_2 = [Export]
data Rule_20 = Ctr__Rule_20__0 RtkPos |
               Ctr__Rule_20__1 RtkPos OptImpSpec
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_20 where
    rtkPosOf (Ctr__Rule_20__0 p) = p
    rtkPosOf (Ctr__Rule_20__1 p _) = p
type Rule_21 = [TopDecl]
data Rule_22 = Ctr__Rule_22__0 RtkPos FunLhs |
               Ctr__Rule_22__1 RtkPos Pat
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_22 where
    rtkPosOf (Ctr__Rule_22__0 p _) = p
    rtkPosOf (Ctr__Rule_22__1 p _) = p
data Rule_23 = Ctr__Rule_23__0 RtkPos Context
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_23 where
    rtkPosOf (Ctr__Rule_23__0 p _) = p
type Rule_24 = [Op]
type Rule_25 = [Var]
type Rule_26 = [Decl]
data Rule_27 = Ctr__Rule_27__0 RtkPos OptContext Type
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_27 where
    rtkPosOf (Ctr__Rule_27__0 p _ _) = p
type Rule_28 = [Rule_29]
data Rule_29 = Ctr__Rule_29__0 RtkPos QOp ExpI
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_29 where
    rtkPosOf (Ctr__Rule_29__0 p _ _) = p
data Rule_3 = Ctr__Rule_3__0 RtkPos |
              Ctr__Rule_3__1 RtkPos Rule_4
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_3 where
    rtkPosOf (Ctr__Rule_3__0 p) = p
    rtkPosOf (Ctr__Rule_3__1 p _) = p
type Rule_30 = [Constr]
type Rule_31 = [FieldDecl]
data Rule_32 = Ctr__Rule_32__0 RtkPos Type |
               Ctr__Rule_32__1 RtkPos AType
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_32 where
    rtkPosOf (Ctr__Rule_32__0 p _) = p
    rtkPosOf (Ctr__Rule_32__1 p _) = p
type Rule_33 = [Var]
data Rule_34 = Ctr__Rule_34__0 RtkPos DClass |
               Ctr__Rule_34__1 RtkPos DClassList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_34 where
    rtkPosOf (Ctr__Rule_34__0 p _) = p
    rtkPosOf (Ctr__Rule_34__1 p _) = p
type Rule_35 = [DClass]
type Rule_36 = [Class]
data Rule_37 = Ctr__Rule_37__0 RtkPos |
               Ctr__Rule_37__1 RtkPos Rule_38
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_37 where
    rtkPosOf (Ctr__Rule_37__0 p) = p
    rtkPosOf (Ctr__Rule_37__1 p _) = p
data Rule_38 = Ctr__Rule_38__0 RtkPos Type
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_38 where
    rtkPosOf (Ctr__Rule_38__0 p _) = p
data Rule_39 = Ctr__Rule_39__0 RtkPos |
               Ctr__Rule_39__1 RtkPos BType
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_39 where
    rtkPosOf (Ctr__Rule_39__0 p) = p
    rtkPosOf (Ctr__Rule_39__1 p _) = p
data Rule_4 = Ctr__Rule_4__0 RtkPos |
              Ctr__Rule_4__1 RtkPos CNameList
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_4 where
    rtkPosOf (Ctr__Rule_4__0 p) = p
    rtkPosOf (Ctr__Rule_4__1 p _) = p
data Rule_41 = Ctr__Rule_41__0 RtkPos |
               Ctr__Rule_41__1 RtkPos Type
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_41 where
    rtkPosOf (Ctr__Rule_41__0 p) = p
    rtkPosOf (Ctr__Rule_41__1 p _) = p
type Rule_42 = [Type]
data Rule_5 = Ctr__Rule_5__0 RtkPos |
              Ctr__Rule_5__1 RtkPos Rule_6
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_5 where
    rtkPosOf (Ctr__Rule_5__0 p) = p
    rtkPosOf (Ctr__Rule_5__1 p _) = p
data Rule_6 = Ctr__Rule_6__0 RtkPos |
              Ctr__Rule_6__1 RtkPos QVarList
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_6 where
    rtkPosOf (Ctr__Rule_6__0 p) = p
    rtkPosOf (Ctr__Rule_6__1 p _) = p
data Rule_7 = Ctr__Rule_7__0 RtkPos ImpDeclList Rule_8 |
              Ctr__Rule_7__1 RtkPos ImpDeclList
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_7 where
    rtkPosOf (Ctr__Rule_7__0 p _ _) = p
    rtkPosOf (Ctr__Rule_7__1 p _) = p
data Rule_8 = Ctr__Rule_8__0 RtkPos |
              Ctr__Rule_8__1 RtkPos Rule_9
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_8 where
    rtkPosOf (Ctr__Rule_8__0 p) = p
    rtkPosOf (Ctr__Rule_8__1 p _) = p
data Rule_9 = Ctr__Rule_9__0 RtkPos TopDecls
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_9 where
    rtkPosOf (Ctr__Rule_9__0 p _) = p
data SimpleType = Anti_SimpleType String |
                  Ctr__SimpleType__0 RtkPos TyCon TyVars
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf SimpleType where
    rtkPosOf (Anti_SimpleType _) = rtkNoPos
    rtkPosOf (Ctr__SimpleType__0 p _ _) = p
data TopDecl = Anti_TopDecl String |
               Ctr__TopDecl__0 RtkPos SimpleType Type |
               Ctr__TopDecl__1 RtkPos OptContext SimpleType Constrs OptDeriving |
               Ctr__TopDecl__2 RtkPos Decl
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TopDecl where
    rtkPosOf (Anti_TopDecl _) = rtkNoPos
    rtkPosOf (Ctr__TopDecl__0 p _ _) = p
    rtkPosOf (Ctr__TopDecl__1 p _ _ _ _) = p
    rtkPosOf (Ctr__TopDecl__2 p _) = p
data TopDecls = Anti_TopDecls String |
                Ctr__TopDecls__0 RtkPos Rule_21
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TopDecls where
    rtkPosOf (Anti_TopDecls _) = rtkNoPos
    rtkPosOf (Ctr__TopDecls__0 p _) = p
data TyCls = Anti_TyCls String |
             Ctr__TyCls__0 RtkPos String
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TyCls where
    rtkPosOf (Anti_TyCls _) = rtkNoPos
    rtkPosOf (Ctr__TyCls__0 p _) = p
data TyCon = Anti_TyCon String |
             Ctr__TyCon__0 RtkPos String
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TyCon where
    rtkPosOf (Anti_TyCon _) = rtkNoPos
    rtkPosOf (Ctr__TyCon__0 p _) = p
data TyVar = Anti_TyVar String |
             Ctr__TyVar__0 RtkPos String
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TyVar where
    rtkPosOf (Anti_TyVar _) = rtkNoPos
    rtkPosOf (Ctr__TyVar__0 p _) = p
type TyVars = [TyVar]
data Type = Anti_Type String |
            Ctr__Type__0 RtkPos BType Rule_37
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Type where
    rtkPosOf (Anti_Type _) = rtkNoPos
    rtkPosOf (Ctr__Type__0 p _ _) = p
data TypeList = Anti_TypeList String |
                Ctr__TypeList__0 RtkPos Rule_42
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TypeList where
    rtkPosOf (Anti_TypeList _) = rtkNoPos
    rtkPosOf (Ctr__TypeList__0 p _) = p
data Var = Anti_Var String |
           Ctr__Var__0 RtkPos String
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Var where
    rtkPosOf (Anti_Var _) = rtkNoPos
    rtkPosOf (Ctr__Var__0 p _) = p
data Vars = Anti_Vars String |
            Ctr__Vars__0 RtkPos Rule_33
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Vars where
    rtkPosOf (Anti_Vars _) = rtkNoPos
    rtkPosOf (Ctr__Vars__0 p _) = p
}