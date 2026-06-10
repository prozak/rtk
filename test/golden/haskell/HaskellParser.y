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
tok_AType_dummy_121 { L.PosToken _ L.Tk__tok_AType_dummy_121 }
tok_ATypeList_dummy_120 { L.PosToken _ L.Tk__tok_ATypeList_dummy_120 }
tok_BType_dummy_119 { L.PosToken _ L.Tk__tok_BType_dummy_119 }
tok_Body_dummy_118 { L.PosToken _ L.Tk__tok_Body_dummy_118 }
tok_CName_dummy_117 { L.PosToken _ L.Tk__tok_CName_dummy_117 }
tok_CNameList_dummy_116 { L.PosToken _ L.Tk__tok_CNameList_dummy_116 }
tok_Class_dummy_115 { L.PosToken _ L.Tk__tok_Class_dummy_115 }
tok_ClassList_dummy_114 { L.PosToken _ L.Tk__tok_ClassList_dummy_114 }
tok_Con_dummy_113 { L.PosToken _ L.Tk__tok_Con_dummy_113 }
tok_Constr_dummy_112 { L.PosToken _ L.Tk__tok_Constr_dummy_112 }
tok_Constrs_dummy_111 { L.PosToken _ L.Tk__tok_Constrs_dummy_111 }
tok_Context_dummy_110 { L.PosToken _ L.Tk__tok_Context_dummy_110 }
tok_DClass_dummy_109 { L.PosToken _ L.Tk__tok_DClass_dummy_109 }
tok_DClassList_dummy_108 { L.PosToken _ L.Tk__tok_DClassList_dummy_108 }
tok_Decl_dummy_107 { L.PosToken _ L.Tk__tok_Decl_dummy_107 }
tok_DeclList_dummy_106 { L.PosToken _ L.Tk__tok_DeclList_dummy_106 }
tok_Decls_dummy_105 { L.PosToken _ L.Tk__tok_Decls_dummy_105 }
tok_Deriving_dummy_104 { L.PosToken _ L.Tk__tok_Deriving_dummy_104 }
tok_Exp_dummy_103 { L.PosToken _ L.Tk__tok_Exp_dummy_103 }
tok_ExpI_dummy_102 { L.PosToken _ L.Tk__tok_ExpI_dummy_102 }
tok_Export_dummy_101 { L.PosToken _ L.Tk__tok_Export_dummy_101 }
tok_ExportsList_dummy_100 { L.PosToken _ L.Tk__tok_ExportsList_dummy_100 }
tok_ExportsOpt_dummy_99 { L.PosToken _ L.Tk__tok_ExportsOpt_dummy_99 }
tok_FieldDecl_dummy_98 { L.PosToken _ L.Tk__tok_FieldDecl_dummy_98 }
tok_FieldDeclList_dummy_97 { L.PosToken _ L.Tk__tok_FieldDeclList_dummy_97 }
tok_Fixity_dummy_96 { L.PosToken _ L.Tk__tok_Fixity_dummy_96 }
tok_FunLhs_dummy_95 { L.PosToken _ L.Tk__tok_FunLhs_dummy_95 }
tok_GTyCon_dummy_94 { L.PosToken _ L.Tk__tok_GTyCon_dummy_94 }
tok_Gd_dummy_93 { L.PosToken _ L.Tk__tok_Gd_dummy_93 }
tok_GdRhs_dummy_92 { L.PosToken _ L.Tk__tok_GdRhs_dummy_92 }
tok_GenDecl_dummy_91 { L.PosToken _ L.Tk__tok_GenDecl_dummy_91 }
tok_Haskell_dummy_122 { L.PosToken _ L.Tk__tok_Haskell_dummy_122 }
tok_ImpDecl_dummy_90 { L.PosToken _ L.Tk__tok_ImpDecl_dummy_90 }
tok_ImpDeclList_dummy_89 { L.PosToken _ L.Tk__tok_ImpDeclList_dummy_89 }
tok_Import_dummy_88 { L.PosToken _ L.Tk__tok_Import_dummy_88 }
tok_ImportList_dummy_87 { L.PosToken _ L.Tk__tok_ImportList_dummy_87 }
tok_ModId_dummy_86 { L.PosToken _ L.Tk__tok_ModId_dummy_86 }
tok_ModIdList_dummy_85 { L.PosToken _ L.Tk__tok_ModIdList_dummy_85 }
tok_Module_dummy_84 { L.PosToken _ L.Tk__tok_Module_dummy_84 }
tok_Op_dummy_83 { L.PosToken _ L.Tk__tok_Op_dummy_83 }
tok_Ops_dummy_82 { L.PosToken _ L.Tk__tok_Ops_dummy_82 }
tok_OptContext_dummy_81 { L.PosToken _ L.Tk__tok_OptContext_dummy_81 }
tok_OptDeriving_dummy_80 { L.PosToken _ L.Tk__tok_OptDeriving_dummy_80 }
tok_OptExpTypeSignature_dummy_79 { L.PosToken _ L.Tk__tok_OptExpTypeSignature_dummy_79 }
tok_OptGdRhs_dummy_78 { L.PosToken _ L.Tk__tok_OptGdRhs_dummy_78 }
tok_OptImpSpec_dummy_77 { L.PosToken _ L.Tk__tok_OptImpSpec_dummy_77 }
tok_OptInteger_dummy_76 { L.PosToken _ L.Tk__tok_OptInteger_dummy_76 }
tok_OptQualified_dummy_75 { L.PosToken _ L.Tk__tok_OptQualified_dummy_75 }
tok_OptQualifiedAs_dummy_74 { L.PosToken _ L.Tk__tok_OptQualifiedAs_dummy_74 }
tok_OptWhere_dummy_73 { L.PosToken _ L.Tk__tok_OptWhere_dummy_73 }
tok_Pat_dummy_72 { L.PosToken _ L.Tk__tok_Pat_dummy_72 }
tok_QOp_dummy_71 { L.PosToken _ L.Tk__tok_QOp_dummy_71 }
tok_QTyCls_dummy_70 { L.PosToken _ L.Tk__tok_QTyCls_dummy_70 }
tok_QTyCon_dummy_69 { L.PosToken _ L.Tk__tok_QTyCon_dummy_69 }
tok_QVar_dummy_68 { L.PosToken _ L.Tk__tok_QVar_dummy_68 }
tok_QVarId_dummy_67 { L.PosToken _ L.Tk__tok_QVarId_dummy_67 }
tok_QVarList_dummy_66 { L.PosToken _ L.Tk__tok_QVarList_dummy_66 }
tok_Rhs_dummy_65 { L.PosToken _ L.Tk__tok_Rhs_dummy_65 }
tok_SimpleType_dummy_64 { L.PosToken _ L.Tk__tok_SimpleType_dummy_64 }
tok_TopDecl_dummy_63 { L.PosToken _ L.Tk__tok_TopDecl_dummy_63 }
tok_TopDecls_dummy_62 { L.PosToken _ L.Tk__tok_TopDecls_dummy_62 }
tok_TyCls_dummy_61 { L.PosToken _ L.Tk__tok_TyCls_dummy_61 }
tok_TyCon_dummy_60 { L.PosToken _ L.Tk__tok_TyCon_dummy_60 }
tok_TyVar_dummy_59 { L.PosToken _ L.Tk__tok_TyVar_dummy_59 }
tok_TyVars_dummy_58 { L.PosToken _ L.Tk__tok_TyVars_dummy_58 }
tok_Type_dummy_57 { L.PosToken _ L.Tk__tok_Type_dummy_57 }
tok_TypeList_dummy_56 { L.PosToken _ L.Tk__tok_TypeList_dummy_56 }
tok_Var_dummy_55 { L.PosToken _ L.Tk__tok_Var_dummy_55 }
tok_Vars_dummy_54 { L.PosToken _ L.Tk__tok_Vars_dummy_54 }
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
th { L.PosToken _ (L.Tk__th $$) }
ncomment { L.PosToken _ (L.Tk__ncomment $$) }
whitespace { L.PosToken _ (L.Tk__whitespace $$) }
integer { L.PosToken _ (L.Tk__integer $$) }
hexadecimal { L.PosToken _ (L.Tk__hexadecimal $$) }
octal { L.PosToken _ (L.Tk__octal $$) }
decimal { L.PosToken _ (L.Tk__decimal $$) }
qq_QOp { L.PosToken _ (L.Tk__qq_QOp $$) }
qq_Op { L.PosToken _ (L.Tk__qq_Op $$) }
qq_TyCls { L.PosToken _ (L.Tk__qq_TyCls $$) }
qq_ModId { L.PosToken _ (L.Tk__qq_ModId $$) }
qq_TyCon { L.PosToken _ (L.Tk__qq_TyCon $$) }
qq_TyVar { L.PosToken _ (L.Tk__qq_TyVar $$) }
varid { L.PosToken _ (L.Tk__varid $$) }
conid { L.PosToken _ (L.Tk__conid $$) }
qq_TyVars { L.PosToken _ (L.Tk__qq_TyVars $$) }
qq_SimpleType { L.PosToken _ (L.Tk__qq_SimpleType $$) }
qq_TypeList { L.PosToken _ (L.Tk__qq_TypeList $$) }
qq_GTyCon { L.PosToken _ (L.Tk__qq_GTyCon $$) }
qq_AType { L.PosToken _ (L.Tk__qq_AType $$) }
qq_ATypeList { L.PosToken _ (L.Tk__qq_ATypeList $$) }
qq_BType { L.PosToken _ (L.Tk__qq_BType $$) }
qq_Type { L.PosToken _ (L.Tk__qq_Type $$) }
qq_Class { L.PosToken _ (L.Tk__qq_Class $$) }
qq_ClassList { L.PosToken _ (L.Tk__qq_ClassList $$) }
qq_Context { L.PosToken _ (L.Tk__qq_Context $$) }
qq_DClass { L.PosToken _ (L.Tk__qq_DClass $$) }
qq_DClassList { L.PosToken _ (L.Tk__qq_DClassList $$) }
qq_Deriving { L.PosToken _ (L.Tk__qq_Deriving $$) }
qq_OptDeriving { L.PosToken _ (L.Tk__qq_OptDeriving $$) }
qq_Vars { L.PosToken _ (L.Tk__qq_Vars $$) }
qq_FieldDecl { L.PosToken _ (L.Tk__qq_FieldDecl $$) }
qq_FieldDeclList { L.PosToken _ (L.Tk__qq_FieldDeclList $$) }
qq_Constr { L.PosToken _ (L.Tk__qq_Constr $$) }
qq_Constrs { L.PosToken _ (L.Tk__qq_Constrs $$) }
qq_GdRhs { L.PosToken _ (L.Tk__qq_GdRhs $$) }
qq_ExpI { L.PosToken _ (L.Tk__qq_ExpI $$) }
qq_Exp { L.PosToken _ (L.Tk__qq_Exp $$) }
qq_OptExpTypeSignature { L.PosToken _ (L.Tk__qq_OptExpTypeSignature $$) }
qq_Gd { L.PosToken _ (L.Tk__qq_Gd $$) }
qq_OptGdRhs { L.PosToken _ (L.Tk__qq_OptGdRhs $$) }
qq_Rhs { L.PosToken _ (L.Tk__qq_Rhs $$) }
qq_Decls { L.PosToken _ (L.Tk__qq_Decls $$) }
qq_DeclList { L.PosToken _ (L.Tk__qq_DeclList $$) }
qq_OptWhere { L.PosToken _ (L.Tk__qq_OptWhere $$) }
qq_Pat { L.PosToken _ (L.Tk__qq_Pat $$) }
qq_FunLhs { L.PosToken _ (L.Tk__qq_FunLhs $$) }
qq_Fixity { L.PosToken _ (L.Tk__qq_Fixity $$) }
qq_Ops { L.PosToken _ (L.Tk__qq_Ops $$) }
qq_OptInteger { L.PosToken _ (L.Tk__qq_OptInteger $$) }
qq_GenDecl { L.PosToken _ (L.Tk__qq_GenDecl $$) }
qq_OptContext { L.PosToken _ (L.Tk__qq_OptContext $$) }
qq_Decl { L.PosToken _ (L.Tk__qq_Decl $$) }
qq_TopDecl { L.PosToken _ (L.Tk__qq_TopDecl $$) }
qq_TopDecls { L.PosToken _ (L.Tk__qq_TopDecls $$) }
qq_ImpDecl { L.PosToken _ (L.Tk__qq_ImpDecl $$) }
qq_OptImpSpec { L.PosToken _ (L.Tk__qq_OptImpSpec $$) }
qq_OptQualifiedAs { L.PosToken _ (L.Tk__qq_OptQualifiedAs $$) }
qq_OptQualified { L.PosToken _ (L.Tk__qq_OptQualified $$) }
qq_Import { L.PosToken _ (L.Tk__qq_Import $$) }
qq_QVarList { L.PosToken _ (L.Tk__qq_QVarList $$) }
qq_CNameList { L.PosToken _ (L.Tk__qq_CNameList $$) }
qq_CName { L.PosToken _ (L.Tk__qq_CName $$) }
qq_QTyCon { L.PosToken _ (L.Tk__qq_QTyCon $$) }
qq_QTyCls { L.PosToken _ (L.Tk__qq_QTyCls $$) }
qq_QVar { L.PosToken _ (L.Tk__qq_QVar $$) }
qq_QVarId { L.PosToken _ (L.Tk__qq_QVarId $$) }
qq_ModIdList { L.PosToken _ (L.Tk__qq_ModIdList $$) }
qq_Con { L.PosToken _ (L.Tk__qq_Con $$) }
qq_Var { L.PosToken _ (L.Tk__qq_Var $$) }
qq_ImportList { L.PosToken _ (L.Tk__qq_ImportList $$) }
qq_ImpDeclList { L.PosToken _ (L.Tk__qq_ImpDeclList $$) }
qq_Body { L.PosToken _ (L.Tk__qq_Body $$) }
qq_Export { L.PosToken _ (L.Tk__qq_Export $$) }
qq_ExportsList { L.PosToken _ (L.Tk__qq_ExportsList $$) }
qq_ExportsOpt { L.PosToken _ (L.Tk__qq_ExportsOpt $$) }
qq_Module { L.PosToken _ (L.Tk__qq_Module $$) }
qq_Haskell { L.PosToken _ (L.Tk__qq_Haskell $$) }

%%

Haskell__top : Haskell rtk__eof { $1 }

Haskell : tok_Haskell_dummy_122 Haskell tok_Haskell_dummy_122 { Ctr__Haskell__0 $2 } |
          tok_AType_dummy_121 AType tok_AType_dummy_121 { Ctr__Haskell__1 $2 } |
          tok_ATypeList_dummy_120 ATypeList tok_ATypeList_dummy_120 { Ctr__Haskell__2 (reverse $2) } |
          tok_BType_dummy_119 BType tok_BType_dummy_119 { Ctr__Haskell__3 $2 } |
          tok_Body_dummy_118 Body tok_Body_dummy_118 { Ctr__Haskell__4 $2 } |
          tok_CName_dummy_117 CName tok_CName_dummy_117 { Ctr__Haskell__5 $2 } |
          tok_CNameList_dummy_116 CNameList tok_CNameList_dummy_116 { Ctr__Haskell__6 $2 } |
          tok_Class_dummy_115 Class tok_Class_dummy_115 { Ctr__Haskell__7 $2 } |
          tok_ClassList_dummy_114 ClassList tok_ClassList_dummy_114 { Ctr__Haskell__8 $2 } |
          tok_Con_dummy_113 Con tok_Con_dummy_113 { Ctr__Haskell__9 $2 } |
          tok_Constr_dummy_112 Constr tok_Constr_dummy_112 { Ctr__Haskell__10 $2 } |
          tok_Constrs_dummy_111 Constrs tok_Constrs_dummy_111 { Ctr__Haskell__11 $2 } |
          tok_Context_dummy_110 Context tok_Context_dummy_110 { Ctr__Haskell__12 $2 } |
          tok_DClass_dummy_109 DClass tok_DClass_dummy_109 { Ctr__Haskell__13 $2 } |
          tok_DClassList_dummy_108 DClassList tok_DClassList_dummy_108 { Ctr__Haskell__14 $2 } |
          tok_Decl_dummy_107 Decl tok_Decl_dummy_107 { Ctr__Haskell__15 $2 } |
          tok_DeclList_dummy_106 DeclList tok_DeclList_dummy_106 { Ctr__Haskell__16 $2 } |
          tok_Decls_dummy_105 Decls tok_Decls_dummy_105 { Ctr__Haskell__17 $2 } |
          tok_Deriving_dummy_104 Deriving tok_Deriving_dummy_104 { Ctr__Haskell__18 $2 } |
          tok_Exp_dummy_103 Exp tok_Exp_dummy_103 { Ctr__Haskell__19 $2 } |
          tok_ExpI_dummy_102 ExpI tok_ExpI_dummy_102 { Ctr__Haskell__20 $2 } |
          tok_Export_dummy_101 Export tok_Export_dummy_101 { Ctr__Haskell__21 $2 } |
          tok_ExportsList_dummy_100 ExportsList tok_ExportsList_dummy_100 { Ctr__Haskell__22 $2 } |
          tok_ExportsOpt_dummy_99 ExportsOpt tok_ExportsOpt_dummy_99 { Ctr__Haskell__23 $2 } |
          tok_FieldDecl_dummy_98 FieldDecl tok_FieldDecl_dummy_98 { Ctr__Haskell__24 $2 } |
          tok_FieldDeclList_dummy_97 FieldDeclList tok_FieldDeclList_dummy_97 { Ctr__Haskell__25 $2 } |
          tok_Fixity_dummy_96 Fixity tok_Fixity_dummy_96 { Ctr__Haskell__26 $2 } |
          tok_FunLhs_dummy_95 FunLhs tok_FunLhs_dummy_95 { Ctr__Haskell__27 $2 } |
          tok_GTyCon_dummy_94 GTyCon tok_GTyCon_dummy_94 { Ctr__Haskell__28 $2 } |
          tok_Gd_dummy_93 Gd tok_Gd_dummy_93 { Ctr__Haskell__29 $2 } |
          tok_GdRhs_dummy_92 GdRhs tok_GdRhs_dummy_92 { Ctr__Haskell__30 $2 } |
          tok_GenDecl_dummy_91 GenDecl tok_GenDecl_dummy_91 { Ctr__Haskell__31 $2 } |
          tok_ImpDecl_dummy_90 ImpDecl tok_ImpDecl_dummy_90 { Ctr__Haskell__32 $2 } |
          tok_ImpDeclList_dummy_89 ImpDeclList tok_ImpDeclList_dummy_89 { Ctr__Haskell__33 $2 } |
          tok_Import_dummy_88 Import tok_Import_dummy_88 { Ctr__Haskell__34 $2 } |
          tok_ImportList_dummy_87 ImportList tok_ImportList_dummy_87 { Ctr__Haskell__35 $2 } |
          tok_ModId_dummy_86 ModId tok_ModId_dummy_86 { Ctr__Haskell__36 $2 } |
          tok_ModIdList_dummy_85 ModIdList tok_ModIdList_dummy_85 { Ctr__Haskell__37 (reverse $2) } |
          tok_Module_dummy_84 Module tok_Module_dummy_84 { Ctr__Haskell__38 $2 } |
          tok_Op_dummy_83 Op tok_Op_dummy_83 { Ctr__Haskell__39 $2 } |
          tok_Ops_dummy_82 Ops tok_Ops_dummy_82 { Ctr__Haskell__40 $2 } |
          tok_OptContext_dummy_81 OptContext tok_OptContext_dummy_81 { Ctr__Haskell__41 $2 } |
          tok_OptDeriving_dummy_80 OptDeriving tok_OptDeriving_dummy_80 { Ctr__Haskell__42 $2 } |
          tok_OptExpTypeSignature_dummy_79 OptExpTypeSignature tok_OptExpTypeSignature_dummy_79 { Ctr__Haskell__43 $2 } |
          tok_OptGdRhs_dummy_78 OptGdRhs tok_OptGdRhs_dummy_78 { Ctr__Haskell__44 $2 } |
          tok_OptImpSpec_dummy_77 OptImpSpec tok_OptImpSpec_dummy_77 { Ctr__Haskell__45 $2 } |
          tok_OptInteger_dummy_76 OptInteger tok_OptInteger_dummy_76 { Ctr__Haskell__46 $2 } |
          tok_OptQualified_dummy_75 OptQualified tok_OptQualified_dummy_75 { Ctr__Haskell__47 $2 } |
          tok_OptQualifiedAs_dummy_74 OptQualifiedAs tok_OptQualifiedAs_dummy_74 { Ctr__Haskell__48 $2 } |
          tok_OptWhere_dummy_73 OptWhere tok_OptWhere_dummy_73 { Ctr__Haskell__49 $2 } |
          tok_Pat_dummy_72 Pat tok_Pat_dummy_72 { Ctr__Haskell__50 $2 } |
          tok_QOp_dummy_71 QOp tok_QOp_dummy_71 { Ctr__Haskell__51 $2 } |
          tok_QTyCls_dummy_70 QTyCls tok_QTyCls_dummy_70 { Ctr__Haskell__52 $2 } |
          tok_QTyCon_dummy_69 QTyCon tok_QTyCon_dummy_69 { Ctr__Haskell__53 $2 } |
          tok_QVar_dummy_68 QVar tok_QVar_dummy_68 { Ctr__Haskell__54 $2 } |
          tok_QVarId_dummy_67 QVarId tok_QVarId_dummy_67 { Ctr__Haskell__55 $2 } |
          tok_QVarList_dummy_66 QVarList tok_QVarList_dummy_66 { Ctr__Haskell__56 $2 } |
          tok_Rhs_dummy_65 Rhs tok_Rhs_dummy_65 { Ctr__Haskell__57 $2 } |
          tok_SimpleType_dummy_64 SimpleType tok_SimpleType_dummy_64 { Ctr__Haskell__58 $2 } |
          tok_TopDecl_dummy_63 TopDecl tok_TopDecl_dummy_63 { Ctr__Haskell__59 $2 } |
          tok_TopDecls_dummy_62 TopDecls tok_TopDecls_dummy_62 { Ctr__Haskell__60 $2 } |
          tok_TyCls_dummy_61 TyCls tok_TyCls_dummy_61 { Ctr__Haskell__61 $2 } |
          tok_TyCon_dummy_60 TyCon tok_TyCon_dummy_60 { Ctr__Haskell__62 $2 } |
          tok_TyVar_dummy_59 TyVar tok_TyVar_dummy_59 { Ctr__Haskell__63 $2 } |
          tok_TyVars_dummy_58 TyVars tok_TyVars_dummy_58 { Ctr__Haskell__64 (reverse $2) } |
          tok_Type_dummy_57 Type tok_Type_dummy_57 { Ctr__Haskell__65 $2 } |
          tok_TypeList_dummy_56 TypeList tok_TypeList_dummy_56 { Ctr__Haskell__66 $2 } |
          tok_Var_dummy_55 Var tok_Var_dummy_55 { Ctr__Haskell__67 $2 } |
          tok_Vars_dummy_54 Vars tok_Vars_dummy_54 { Ctr__Haskell__68 $2 }

Haskell : qq_Haskell { Anti_Haskell $1 } |
          Module { Ctr__Haskell__69 $1 }

AType : qq_AType { Anti_AType $1 } |
        TyVar { Ctr__AType__0 $1 } |
        GTyCon { Ctr__AType__1 $1 } |
        tok__lparen__2 Rule_49 tok__rparen__4 { Ctr__AType__2 $2 } |
        tok__sq_bkt_l__25 Rule_50 tok__sq_bkt_r__26 { Ctr__AType__3 $2 }

ListElem_ATypeList48 : qq_ATypeList { Anti_AType $1 } |
                       AType { $1 }

ATypeList : {- empty -} { [] } |
            ATypeList ListElem_ATypeList48 { $2 : $1 }

BType : qq_BType { Anti_BType $1 } |
        Rule_46 AType { Ctr__BType__0 $1 $2 }

Body : qq_Body { Anti_Body $1 } |
       tok__symbol__6 Rule_8 tok__symbol__8 { Ctr__Body__0 $2 }

CName : qq_CName { Anti_CName $1 } |
        Var { Ctr__CName__0 $1 } |
        Con { Ctr__CName__1 $1 }

CNameList : qq_CNameList { Anti_CNameList $1 } |
            Rule_15 tok__coma__3 { Ctr__CNameList__0 (reverse $1) }

Class : qq_Class { Anti_Class $1 } |
        QTyCls TyVar { Ctr__Class__0 $1 $2 } |
        QTyCls tok__lparen__2 TyVar ATypeList tok__rparen__4 { Ctr__Class__1 $1 $3 (reverse $4) }

ClassList : qq_ClassList { Anti_ClassList $1 } |
            Rule_43 tok__coma__3 { Ctr__ClassList__0 (reverse $1) }

Con : qq_Con { Anti_Con $1 } |
      conid { Ctr__Con__0 $1 }

Constr : qq_Constr { Anti_Constr $1 } |
         Con tok__symbol__6 FieldDeclList tok__symbol__8 { Ctr__Constr__0 $1 $3 }

Constrs : qq_Constrs { Anti_Constrs $1 } |
          Rule_36 tok__pipe__21 { Ctr__Constrs__0 (reverse $1) }

Context : qq_Context { Anti_Context $1 } |
          Class { Ctr__Context__0 $1 } |
          tok__lparen__2 ClassList tok__rparen__4 { Ctr__Context__1 $2 }

DClass : qq_DClass { Anti_DClass $1 } |
         QTyCls { Ctr__DClass__0 $1 }

DClassList : qq_DClassList { Anti_DClassList $1 } |
             Rule_42 tok__coma__3 { Ctr__DClassList__0 (reverse $1) }

Decl : qq_Decl { Anti_Decl $1 } |
       GenDecl { Ctr__Decl__0 $1 } |
       Rule_25 Rhs { Ctr__Decl__1 $1 $2 }

DeclList : qq_DeclList { Anti_DeclList $1 } |
           Rule_31 tok__semi__7 { Ctr__DeclList__0 (reverse $1) }

Decls : qq_Decls { Anti_Decls $1 } |
        tok__symbol__6 DeclList tok__symbol__8 { Ctr__Decls__0 $2 }

Deriving : qq_Deriving { Anti_Deriving $1 } |
           tok_deriving_23 Rule_41 { Ctr__Deriving__0 $2 }

Exp : qq_Exp { Anti_Exp $1 } |
      ExpI OptExpTypeSignature { Ctr__Exp__0 $1 $2 }

ExpI : qq_ExpI { Anti_ExpI $1 } |
       ExpI Rule_34 { Ctr__ExpI__0 $1 (reverse $2) }

Export : qq_Export { Anti_Export $1 } |
         tok_module_0 ModId { Ctr__Export__0 $2 } |
         QVar { Ctr__Export__1 $1 } |
         QTyCon Rule_4 { Ctr__Export__2 $1 $2 } |
         QTyCls Rule_6 { Ctr__Export__3 $1 $2 }

ExportsList : qq_ExportsList { Anti_ExportsList $1 } |
              Rule_3 tok__coma__3 { Ctr__ExportsList__0 (reverse $1) }

ExportsOpt : qq_ExportsOpt { Anti_ExportsOpt $1 } |
             { Ctr__ExportsOpt__0 } |
             Rule_0 { Ctr__ExportsOpt__1 $1 }

FieldDecl : qq_FieldDecl { Anti_FieldDecl $1 } |
            Vars tok__colon__colon__17 Rule_38 { Ctr__FieldDecl__0 $1 $3 }

FieldDeclList : qq_FieldDeclList { Anti_FieldDeclList $1 } |
                Rule_37 tok__coma__3 { Ctr__FieldDeclList__0 (reverse $1) }

Fixity : qq_Fixity { Anti_Fixity $1 } |
         tok_infixl_18 { Ctr__Fixity__0 } |
         tok_infixr_19 { Ctr__Fixity__1 } |
         tok_infix_20 { Ctr__Fixity__2 }

FunLhs : qq_FunLhs { Anti_FunLhs $1 } |
         Var { Ctr__FunLhs__0 $1 }

GTyCon : qq_GTyCon { Anti_GTyCon $1 } |
         QTyCon { Ctr__GTyCon__0 $1 } |
         tok__lparen__2 tok__minus__symbol__24 tok__rparen__4 { Ctr__GTyCon__1 }

Gd : qq_Gd { Anti_Gd $1 } |
     { Ctr__Gd__0 } |
     ExpI { Ctr__Gd__1 $1 }

GdRhs : qq_GdRhs { Anti_GdRhs $1 } |
        Gd tok__eql__14 Exp OptGdRhs { Ctr__GdRhs__0 $1 $3 $4 }

GenDecl : qq_GenDecl { Anti_GenDecl $1 } |
          Vars tok__colon__colon__17 OptContext Type { Ctr__GenDecl__0 $1 $3 $4 } |
          Fixity OptInteger Ops { Ctr__GenDecl__1 $1 $2 $3 }

ImpDecl : qq_ImpDecl { Anti_ImpDecl $1 } |
          tok_import_12 OptQualified ModId OptQualifiedAs Rule_23 { Ctr__ImpDecl__0 $2 $3 $4 $5 }

ImpDeclList : qq_ImpDeclList { Anti_ImpDeclList $1 } |
              Rule_11 tok__semi__7 { Ctr__ImpDeclList__0 (reverse $1) }

Import : qq_Import { Anti_Import $1 } |
         Var { Ctr__Import__0 $1 } |
         TyCon Rule_17 { Ctr__Import__1 $1 $2 }

ImportList : qq_ImportList { Anti_ImportList $1 } |
             Rule_12 tok__coma__3 { Ctr__ImportList__0 (reverse $1) }

ModId : qq_ModId { Anti_ModId $1 } |
        conid { Ctr__ModId__0 $1 }

ModIdList : {- empty -} { [] } |
            ModIdList ListElem_ModIdList14 { $2 : $1 }

Module : qq_Module { Anti_Module $1 } |
         tok_module_0 ModId ExportsOpt tok_where_1 Body { Ctr__Module__0 $2 $3 $5 } |
         Body { Ctr__Module__1 $1 }

Op : qq_Op { Anti_Op $1 } |
     varid { Ctr__Op__0 $1 } |
     conid { Ctr__Op__1 $1 }

Ops : qq_Ops { Anti_Ops $1 } |
      Rule_28 tok__coma__3 { Ctr__Ops__0 (reverse $1) }

OptContext : qq_OptContext { Anti_OptContext $1 } |
             { Ctr__OptContext__0 } |
             Rule_26 { Ctr__OptContext__1 $1 }

OptDeriving : qq_OptDeriving { Anti_OptDeriving $1 } |
              { Ctr__OptDeriving__0 } |
              Rule_40 { Ctr__OptDeriving__1 $1 }

OptExpTypeSignature : qq_OptExpTypeSignature { Anti_OptExpTypeSignature $1 } |
                      { Ctr__OptExpTypeSignature__0 } |
                      Rule_33 { Ctr__OptExpTypeSignature__1 $1 }

OptGdRhs : qq_OptGdRhs { Anti_OptGdRhs $1 } |
           { Ctr__OptGdRhs__0 } |
           Rule_32 { Ctr__OptGdRhs__1 $1 }

OptImpSpec : qq_OptImpSpec { Anti_OptImpSpec $1 } |
             tok__lparen__2 ImportList Rule_21 tok__rparen__4 { Ctr__OptImpSpec__0 $2 $3 }

OptInteger : qq_OptInteger { Anti_OptInteger $1 } |
             { Ctr__OptInteger__0 } |
             Rule_27 { Ctr__OptInteger__1 $1 }

OptQualified : qq_OptQualified { Anti_OptQualified $1 } |
               { Ctr__OptQualified__0 } |
               Rule_19 { Ctr__OptQualified__1 $1 }

OptQualifiedAs : qq_OptQualifiedAs { Anti_OptQualifiedAs $1 } |
                 { Ctr__OptQualifiedAs__0 } |
                 Rule_20 { Ctr__OptQualifiedAs__1 $1 }

OptWhere : qq_OptWhere { Anti_OptWhere $1 } |
           tok_where_1 Decls { Ctr__OptWhere__0 $2 }

Pat : qq_Pat { Anti_Pat $1 } |
      Con Rule_29 { Ctr__Pat__0 $1 (reverse $2) }

QOp : qq_QOp { Anti_QOp $1 } |
      ModIdList Op { Ctr__QOp__0 (reverse $1) $2 }

QTyCls : qq_QTyCls { Anti_QTyCls $1 } |
         ModIdList TyCls { Ctr__QTyCls__0 (reverse $1) $2 }

QTyCon : qq_QTyCon { Anti_QTyCon $1 } |
         ModIdList TyCon { Ctr__QTyCon__0 (reverse $1) $2 }

QVar : qq_QVar { Anti_QVar $1 } |
       QVarId { Ctr__QVar__0 $1 }

QVarId : qq_QVarId { Anti_QVarId $1 } |
         ModIdList varid { Ctr__QVarId__0 (reverse $1) $2 }

QVarList : qq_QVarList { Anti_QVarList $1 } |
           Rule_16 tok__coma__3 { Ctr__QVarList__0 (reverse $1) }

Rhs : qq_Rhs { Anti_Rhs $1 } |
      tok__eql__14 Exp OptWhere { Ctr__Rhs__0 $2 $3 } |
      GdRhs OptWhere { Ctr__Rhs__1 $1 $2 }

Rule_0 : tok__lparen__2 ExportsList Rule_1 tok__rparen__4 { Ctr__Rule_0__0 $2 $3 }

Rule_1 : { Ctr__Rule_1__0 } |
         Rule_2 { Ctr__Rule_1__1 $1 }

Rule_10 : tok__semi__7 TopDecls { Ctr__Rule_10__0 $2 }

Rule_11 : ImpDecl { [$1] } |
          Rule_11 ImpDecl { $2 : $1 }

Rule_12 : {- empty -} { [] } |
          Rule_12 Import { $2 : $1 }

ListElem_ModIdList14 : qq_ModIdList { Anti_Rule_13 $1 } |
                       Rule_13 { $1 }

Rule_13 : ModId tok__dot__9 { Ctr__Rule_13__1 $1 }

Rule_15 : {- empty -} { [] } |
          Rule_15 CName { $2 : $1 }

Rule_16 : {- empty -} { [] } |
          Rule_16 QVar { $2 : $1 }

Rule_17 : { Ctr__Rule_17__0 } |
          Rule_18 { Ctr__Rule_17__1 $1 }

Rule_18 : tok__lparen__2 tok__dot__dot__5 tok__rparen__4 { Ctr__Rule_18__0 } |
          tok__lparen__2 CNameList tok__rparen__4 { Ctr__Rule_18__1 $2 }

Rule_19 : tok_qualified_10 { Ctr__Rule_19__0 }

Rule_2 : tok__coma__3 { Ctr__Rule_2__0 }

Rule_20 : tok_as_11 ModId { Ctr__Rule_20__0 $2 }

Rule_21 : { Ctr__Rule_21__0 } |
          Rule_22 { Ctr__Rule_21__1 $1 }

Rule_22 : tok__coma__3 { Ctr__Rule_22__0 }

Rule_23 : { Ctr__Rule_23__0 } |
          OptImpSpec { Ctr__Rule_23__1 $1 }

Rule_24 : {- empty -} { [] } |
          Rule_24 TopDecl { $2 : $1 }

Rule_25 : FunLhs { Ctr__Rule_25__0 $1 } |
          Pat { Ctr__Rule_25__1 $1 }

Rule_26 : Context tok__eql__symbol__16 { Ctr__Rule_26__0 $1 }

Rule_27 : integer { Ctr__Rule_27__0 $1 }

Rule_28 : {- empty -} { [] } |
          Rule_28 Op { $2 : $1 }

Rule_29 : {- empty -} { [] } |
          Rule_29 Rule_30 { $2 : $1 }

Rule_3 : {- empty -} { [] } |
         Rule_3 Export { $2 : $1 }

Rule_30 : Var { Ctr__Rule_30__0 $1 }

Rule_31 : {- empty -} { [] } |
          Rule_31 Decl { $2 : $1 }

Rule_32 : GdRhs { Ctr__Rule_32__0 $1 }

Rule_33 : tok__colon__colon__17 OptContext Type { Ctr__Rule_33__0 $2 $3 }

Rule_34 : {- empty -} { [] } |
          Rule_34 Rule_35 { $2 : $1 }

Rule_35 : QOp ExpI { Ctr__Rule_35__0 $1 $2 }

Rule_36 : {- empty -} { [] } |
          Rule_36 Constr { $2 : $1 }

Rule_37 : {- empty -} { [] } |
          Rule_37 FieldDecl { $2 : $1 }

Rule_38 : Type { Ctr__Rule_38__0 $1 } |
          tok__exclamation__22 AType { Ctr__Rule_38__1 $2 }

Rule_39 : {- empty -} { [] } |
          Rule_39 Var { $2 : $1 }

Rule_4 : { Ctr__Rule_4__0 } |
         Rule_5 { Ctr__Rule_4__1 $1 }

Rule_40 : Deriving { Ctr__Rule_40__0 $1 }

Rule_41 : DClass { Ctr__Rule_41__0 $1 } |
          tok__lparen__2 DClassList tok__rparen__4 { Ctr__Rule_41__1 $2 }

Rule_42 : {- empty -} { [] } |
          Rule_42 DClass { $2 : $1 }

Rule_43 : {- empty -} { [] } |
          Rule_43 Class { $2 : $1 }

Rule_44 : { Ctr__Rule_44__0 } |
          Rule_45 { Ctr__Rule_44__1 $1 }

Rule_45 : tok__minus__symbol__24 Type { Ctr__Rule_45__0 $2 }

Rule_46 : { Ctr__Rule_46__0 } |
          Rule_47 { Ctr__Rule_46__1 $1 }

Rule_47 : BType { Ctr__Rule_47__0 $1 }

Rule_49 : TypeList { Ctr__Rule_49__0 $1 }

Rule_5 : tok__lparen__2 tok__dot__dot__5 tok__rparen__4 { Ctr__Rule_5__0 } |
         tok__lparen__2 CNameList tok__rparen__4 { Ctr__Rule_5__1 $2 }

Rule_50 : { Ctr__Rule_50__0 } |
          Rule_51 { Ctr__Rule_50__1 $1 }

Rule_51 : Type { Ctr__Rule_51__0 $1 }

Rule_52 : {- empty -} { [] } |
          Rule_52 Type { $2 : $1 }

Rule_6 : { Ctr__Rule_6__0 } |
         Rule_7 { Ctr__Rule_6__1 $1 }

Rule_7 : tok__lparen__2 tok__dot__dot__5 tok__rparen__4 { Ctr__Rule_7__0 } |
         tok__lparen__2 QVarList tok__rparen__4 { Ctr__Rule_7__1 $2 }

Rule_8 : ImpDeclList Rule_9 { Ctr__Rule_8__0 $1 $2 } |
         ImpDeclList { Ctr__Rule_8__1 $1 }

Rule_9 : { Ctr__Rule_9__0 } |
         Rule_10 { Ctr__Rule_9__1 $1 }

SimpleType : qq_SimpleType { Anti_SimpleType $1 } |
             TyCon TyVars { Ctr__SimpleType__0 $1 (reverse $2) }

TopDecl : qq_TopDecl { Anti_TopDecl $1 } |
          tok_type_13 SimpleType tok__eql__14 Type { Ctr__TopDecl__0 $2 $4 } |
          tok_data_15 OptContext SimpleType tok__eql__14 Constrs OptDeriving { Ctr__TopDecl__1 $2 $3 $5 $6 } |
          Decl { Ctr__TopDecl__2 $1 }

TopDecls : qq_TopDecls { Anti_TopDecls $1 } |
           Rule_24 tok__semi__7 { Ctr__TopDecls__0 (reverse $1) }

TyCls : qq_TyCls { Anti_TyCls $1 } |
        conid { Ctr__TyCls__0 $1 }

TyCon : qq_TyCon { Anti_TyCon $1 } |
        conid { Ctr__TyCon__0 $1 }

TyVar : qq_TyVar { Anti_TyVar $1 } |
        varid { Ctr__TyVar__0 $1 }

ListElem_TyVars53 : qq_TyVars { Anti_TyVar $1 } |
                    TyVar { $1 }

TyVars : {- empty -} { [] } |
         TyVars ListElem_TyVars53 { $2 : $1 }

Type : qq_Type { Anti_Type $1 } |
       BType Rule_44 { Ctr__Type__0 $1 $2 }

TypeList : qq_TypeList { Anti_TypeList $1 } |
           Rule_52 tok__coma__3 { Ctr__TypeList__0 (reverse $1) }

Var : qq_Var { Anti_Var $1 } |
      varid { Ctr__Var__0 $1 }

Vars : qq_Vars { Anti_Vars $1 } |
       Rule_39 tok__coma__3 { Ctr__Vars__0 (reverse $1) }


{
parseError :: [L.PosToken] -> Either String a
parseError [] = Left "Parse error: unexpected end of input"
parseError (L.PosToken (L.AlexPn _ line col) tok : _) =
    Left $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ ": unexpected " ++ showRtkToken tok

-- Render a token the way it appears in the source, for error messages
showRtkToken :: L.Token -> String
showRtkToken L.EndOfFile = "end of input"
showRtkToken L.Tk__tok_AType_dummy_121 = "'tok_AType_dummy_121'"
showRtkToken L.Tk__tok_ATypeList_dummy_120 = "'tok_ATypeList_dummy_120'"
showRtkToken L.Tk__tok_BType_dummy_119 = "'tok_BType_dummy_119'"
showRtkToken L.Tk__tok_Body_dummy_118 = "'tok_Body_dummy_118'"
showRtkToken L.Tk__tok_CName_dummy_117 = "'tok_CName_dummy_117'"
showRtkToken L.Tk__tok_CNameList_dummy_116 = "'tok_CNameList_dummy_116'"
showRtkToken L.Tk__tok_Class_dummy_115 = "'tok_Class_dummy_115'"
showRtkToken L.Tk__tok_ClassList_dummy_114 = "'tok_ClassList_dummy_114'"
showRtkToken L.Tk__tok_Con_dummy_113 = "'tok_Con_dummy_113'"
showRtkToken L.Tk__tok_Constr_dummy_112 = "'tok_Constr_dummy_112'"
showRtkToken L.Tk__tok_Constrs_dummy_111 = "'tok_Constrs_dummy_111'"
showRtkToken L.Tk__tok_Context_dummy_110 = "'tok_Context_dummy_110'"
showRtkToken L.Tk__tok_DClass_dummy_109 = "'tok_DClass_dummy_109'"
showRtkToken L.Tk__tok_DClassList_dummy_108 = "'tok_DClassList_dummy_108'"
showRtkToken L.Tk__tok_Decl_dummy_107 = "'tok_Decl_dummy_107'"
showRtkToken L.Tk__tok_DeclList_dummy_106 = "'tok_DeclList_dummy_106'"
showRtkToken L.Tk__tok_Decls_dummy_105 = "'tok_Decls_dummy_105'"
showRtkToken L.Tk__tok_Deriving_dummy_104 = "'tok_Deriving_dummy_104'"
showRtkToken L.Tk__tok_Exp_dummy_103 = "'tok_Exp_dummy_103'"
showRtkToken L.Tk__tok_ExpI_dummy_102 = "'tok_ExpI_dummy_102'"
showRtkToken L.Tk__tok_Export_dummy_101 = "'tok_Export_dummy_101'"
showRtkToken L.Tk__tok_ExportsList_dummy_100 = "'tok_ExportsList_dummy_100'"
showRtkToken L.Tk__tok_ExportsOpt_dummy_99 = "'tok_ExportsOpt_dummy_99'"
showRtkToken L.Tk__tok_FieldDecl_dummy_98 = "'tok_FieldDecl_dummy_98'"
showRtkToken L.Tk__tok_FieldDeclList_dummy_97 = "'tok_FieldDeclList_dummy_97'"
showRtkToken L.Tk__tok_Fixity_dummy_96 = "'tok_Fixity_dummy_96'"
showRtkToken L.Tk__tok_FunLhs_dummy_95 = "'tok_FunLhs_dummy_95'"
showRtkToken L.Tk__tok_GTyCon_dummy_94 = "'tok_GTyCon_dummy_94'"
showRtkToken L.Tk__tok_Gd_dummy_93 = "'tok_Gd_dummy_93'"
showRtkToken L.Tk__tok_GdRhs_dummy_92 = "'tok_GdRhs_dummy_92'"
showRtkToken L.Tk__tok_GenDecl_dummy_91 = "'tok_GenDecl_dummy_91'"
showRtkToken L.Tk__tok_Haskell_dummy_122 = "'tok_Haskell_dummy_122'"
showRtkToken L.Tk__tok_ImpDecl_dummy_90 = "'tok_ImpDecl_dummy_90'"
showRtkToken L.Tk__tok_ImpDeclList_dummy_89 = "'tok_ImpDeclList_dummy_89'"
showRtkToken L.Tk__tok_Import_dummy_88 = "'tok_Import_dummy_88'"
showRtkToken L.Tk__tok_ImportList_dummy_87 = "'tok_ImportList_dummy_87'"
showRtkToken L.Tk__tok_ModId_dummy_86 = "'tok_ModId_dummy_86'"
showRtkToken L.Tk__tok_ModIdList_dummy_85 = "'tok_ModIdList_dummy_85'"
showRtkToken L.Tk__tok_Module_dummy_84 = "'tok_Module_dummy_84'"
showRtkToken L.Tk__tok_Op_dummy_83 = "'tok_Op_dummy_83'"
showRtkToken L.Tk__tok_Ops_dummy_82 = "'tok_Ops_dummy_82'"
showRtkToken L.Tk__tok_OptContext_dummy_81 = "'tok_OptContext_dummy_81'"
showRtkToken L.Tk__tok_OptDeriving_dummy_80 = "'tok_OptDeriving_dummy_80'"
showRtkToken L.Tk__tok_OptExpTypeSignature_dummy_79 = "'tok_OptExpTypeSignature_dummy_79'"
showRtkToken L.Tk__tok_OptGdRhs_dummy_78 = "'tok_OptGdRhs_dummy_78'"
showRtkToken L.Tk__tok_OptImpSpec_dummy_77 = "'tok_OptImpSpec_dummy_77'"
showRtkToken L.Tk__tok_OptInteger_dummy_76 = "'tok_OptInteger_dummy_76'"
showRtkToken L.Tk__tok_OptQualified_dummy_75 = "'tok_OptQualified_dummy_75'"
showRtkToken L.Tk__tok_OptQualifiedAs_dummy_74 = "'tok_OptQualifiedAs_dummy_74'"
showRtkToken L.Tk__tok_OptWhere_dummy_73 = "'tok_OptWhere_dummy_73'"
showRtkToken L.Tk__tok_Pat_dummy_72 = "'tok_Pat_dummy_72'"
showRtkToken L.Tk__tok_QOp_dummy_71 = "'tok_QOp_dummy_71'"
showRtkToken L.Tk__tok_QTyCls_dummy_70 = "'tok_QTyCls_dummy_70'"
showRtkToken L.Tk__tok_QTyCon_dummy_69 = "'tok_QTyCon_dummy_69'"
showRtkToken L.Tk__tok_QVar_dummy_68 = "'tok_QVar_dummy_68'"
showRtkToken L.Tk__tok_QVarId_dummy_67 = "'tok_QVarId_dummy_67'"
showRtkToken L.Tk__tok_QVarList_dummy_66 = "'tok_QVarList_dummy_66'"
showRtkToken L.Tk__tok_Rhs_dummy_65 = "'tok_Rhs_dummy_65'"
showRtkToken L.Tk__tok_SimpleType_dummy_64 = "'tok_SimpleType_dummy_64'"
showRtkToken L.Tk__tok_TopDecl_dummy_63 = "'tok_TopDecl_dummy_63'"
showRtkToken L.Tk__tok_TopDecls_dummy_62 = "'tok_TopDecls_dummy_62'"
showRtkToken L.Tk__tok_TyCls_dummy_61 = "'tok_TyCls_dummy_61'"
showRtkToken L.Tk__tok_TyCon_dummy_60 = "'tok_TyCon_dummy_60'"
showRtkToken L.Tk__tok_TyVar_dummy_59 = "'tok_TyVar_dummy_59'"
showRtkToken L.Tk__tok_TyVars_dummy_58 = "'tok_TyVars_dummy_58'"
showRtkToken L.Tk__tok_Type_dummy_57 = "'tok_Type_dummy_57'"
showRtkToken L.Tk__tok_TypeList_dummy_56 = "'tok_TypeList_dummy_56'"
showRtkToken L.Tk__tok_Var_dummy_55 = "'tok_Var_dummy_55'"
showRtkToken L.Tk__tok_Vars_dummy_54 = "'tok_Vars_dummy_54'"
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

data Haskell = Ctr__Haskell__0 Haskell |
               Ctr__Haskell__1 AType |
               Ctr__Haskell__2 ATypeList |
               Ctr__Haskell__3 BType |
               Ctr__Haskell__4 Body |
               Ctr__Haskell__5 CName |
               Ctr__Haskell__6 CNameList |
               Ctr__Haskell__7 Class |
               Ctr__Haskell__8 ClassList |
               Ctr__Haskell__9 Con |
               Ctr__Haskell__10 Constr |
               Ctr__Haskell__11 Constrs |
               Ctr__Haskell__12 Context |
               Ctr__Haskell__13 DClass |
               Ctr__Haskell__14 DClassList |
               Ctr__Haskell__15 Decl |
               Ctr__Haskell__16 DeclList |
               Ctr__Haskell__17 Decls |
               Ctr__Haskell__18 Deriving |
               Ctr__Haskell__19 Exp |
               Ctr__Haskell__20 ExpI |
               Ctr__Haskell__21 Export |
               Ctr__Haskell__22 ExportsList |
               Ctr__Haskell__23 ExportsOpt |
               Ctr__Haskell__24 FieldDecl |
               Ctr__Haskell__25 FieldDeclList |
               Ctr__Haskell__26 Fixity |
               Ctr__Haskell__27 FunLhs |
               Ctr__Haskell__28 GTyCon |
               Ctr__Haskell__29 Gd |
               Ctr__Haskell__30 GdRhs |
               Ctr__Haskell__31 GenDecl |
               Ctr__Haskell__32 ImpDecl |
               Ctr__Haskell__33 ImpDeclList |
               Ctr__Haskell__34 Import |
               Ctr__Haskell__35 ImportList |
               Ctr__Haskell__36 ModId |
               Ctr__Haskell__37 ModIdList |
               Ctr__Haskell__38 Module |
               Ctr__Haskell__39 Op |
               Ctr__Haskell__40 Ops |
               Ctr__Haskell__41 OptContext |
               Ctr__Haskell__42 OptDeriving |
               Ctr__Haskell__43 OptExpTypeSignature |
               Ctr__Haskell__44 OptGdRhs |
               Ctr__Haskell__45 OptImpSpec |
               Ctr__Haskell__46 OptInteger |
               Ctr__Haskell__47 OptQualified |
               Ctr__Haskell__48 OptQualifiedAs |
               Ctr__Haskell__49 OptWhere |
               Ctr__Haskell__50 Pat |
               Ctr__Haskell__51 QOp |
               Ctr__Haskell__52 QTyCls |
               Ctr__Haskell__53 QTyCon |
               Ctr__Haskell__54 QVar |
               Ctr__Haskell__55 QVarId |
               Ctr__Haskell__56 QVarList |
               Ctr__Haskell__57 Rhs |
               Ctr__Haskell__58 SimpleType |
               Ctr__Haskell__59 TopDecl |
               Ctr__Haskell__60 TopDecls |
               Ctr__Haskell__61 TyCls |
               Ctr__Haskell__62 TyCon |
               Ctr__Haskell__63 TyVar |
               Ctr__Haskell__64 TyVars |
               Ctr__Haskell__65 Type |
               Ctr__Haskell__66 TypeList |
               Ctr__Haskell__67 Var |
               Ctr__Haskell__68 Vars |
               Anti_Haskell String |
               Ctr__Haskell__69 Module
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data AType = Anti_AType String |
             Ctr__AType__0 TyVar |
             Ctr__AType__1 GTyCon |
             Ctr__AType__2 Rule_49 |
             Ctr__AType__3 Rule_50
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type ATypeList = [AType]
data BType = Anti_BType String |
             Ctr__BType__0 Rule_46 AType
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Body = Anti_Body String |
            Ctr__Body__0 Rule_8
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data CName = Anti_CName String |
             Ctr__CName__0 Var |
             Ctr__CName__1 Con
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data CNameList = Anti_CNameList String |
                 Ctr__CNameList__0 Rule_15
                 deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Class = Anti_Class String |
             Ctr__Class__0 QTyCls TyVar |
             Ctr__Class__1 QTyCls TyVar ATypeList
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data ClassList = Anti_ClassList String |
                 Ctr__ClassList__0 Rule_43
                 deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Con = Anti_Con String |
           Ctr__Con__0 String
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Constr = Anti_Constr String |
              Ctr__Constr__0 Con FieldDeclList
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Constrs = Anti_Constrs String |
               Ctr__Constrs__0 Rule_36
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Context = Anti_Context String |
               Ctr__Context__0 Class |
               Ctr__Context__1 ClassList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data DClass = Anti_DClass String |
              Ctr__DClass__0 QTyCls
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data DClassList = Anti_DClassList String |
                  Ctr__DClassList__0 Rule_42
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Decl = Anti_Decl String |
            Ctr__Decl__0 GenDecl |
            Ctr__Decl__1 Rule_25 Rhs
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data DeclList = Anti_DeclList String |
                Ctr__DeclList__0 Rule_31
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Decls = Anti_Decls String |
             Ctr__Decls__0 DeclList
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Deriving = Anti_Deriving String |
                Ctr__Deriving__0 Rule_41
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Exp = Anti_Exp String |
           Ctr__Exp__0 ExpI OptExpTypeSignature
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data ExpI = Anti_ExpI String |
            Ctr__ExpI__0 ExpI Rule_34
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Export = Anti_Export String |
              Ctr__Export__0 ModId |
              Ctr__Export__1 QVar |
              Ctr__Export__2 QTyCon Rule_4 |
              Ctr__Export__3 QTyCls Rule_6
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data ExportsList = Anti_ExportsList String |
                   Ctr__ExportsList__0 Rule_3
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data ExportsOpt = Anti_ExportsOpt String |
                  Ctr__ExportsOpt__0 |
                  Ctr__ExportsOpt__1 Rule_0
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data FieldDecl = Anti_FieldDecl String |
                 Ctr__FieldDecl__0 Vars Rule_38
                 deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data FieldDeclList = Anti_FieldDeclList String |
                     Ctr__FieldDeclList__0 Rule_37
                     deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Fixity = Anti_Fixity String |
              Ctr__Fixity__0 |
              Ctr__Fixity__1 |
              Ctr__Fixity__2
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data FunLhs = Anti_FunLhs String |
              Ctr__FunLhs__0 Var
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data GTyCon = Anti_GTyCon String |
              Ctr__GTyCon__0 QTyCon |
              Ctr__GTyCon__1
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Gd = Anti_Gd String |
          Ctr__Gd__0 |
          Ctr__Gd__1 ExpI
          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data GdRhs = Anti_GdRhs String |
             Ctr__GdRhs__0 Gd Exp OptGdRhs
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data GenDecl = Anti_GenDecl String |
               Ctr__GenDecl__0 Vars OptContext Type |
               Ctr__GenDecl__1 Fixity OptInteger Ops
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data ImpDecl = Anti_ImpDecl String |
               Ctr__ImpDecl__0 OptQualified ModId OptQualifiedAs Rule_23
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data ImpDeclList = Anti_ImpDeclList String |
                   Ctr__ImpDeclList__0 Rule_11
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Import = Anti_Import String |
              Ctr__Import__0 Var |
              Ctr__Import__1 TyCon Rule_17
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data ImportList = Anti_ImportList String |
                  Ctr__ImportList__0 Rule_12
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data ModId = Anti_ModId String |
             Ctr__ModId__0 String
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type ModIdList = [Rule_13]
data Module = Anti_Module String |
              Ctr__Module__0 ModId ExportsOpt Body |
              Ctr__Module__1 Body
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Op = Anti_Op String |
          Ctr__Op__0 String |
          Ctr__Op__1 String
          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Ops = Anti_Ops String |
           Ctr__Ops__0 Rule_28
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptContext = Anti_OptContext String |
                  Ctr__OptContext__0 |
                  Ctr__OptContext__1 Rule_26
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptDeriving = Anti_OptDeriving String |
                   Ctr__OptDeriving__0 |
                   Ctr__OptDeriving__1 Rule_40
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptExpTypeSignature = Anti_OptExpTypeSignature String |
                           Ctr__OptExpTypeSignature__0 |
                           Ctr__OptExpTypeSignature__1 Rule_33
                           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptGdRhs = Anti_OptGdRhs String |
                Ctr__OptGdRhs__0 |
                Ctr__OptGdRhs__1 Rule_32
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptImpSpec = Anti_OptImpSpec String |
                  Ctr__OptImpSpec__0 ImportList Rule_21
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptInteger = Anti_OptInteger String |
                  Ctr__OptInteger__0 |
                  Ctr__OptInteger__1 Rule_27
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptQualified = Anti_OptQualified String |
                    Ctr__OptQualified__0 |
                    Ctr__OptQualified__1 Rule_19
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptQualifiedAs = Anti_OptQualifiedAs String |
                      Ctr__OptQualifiedAs__0 |
                      Ctr__OptQualifiedAs__1 Rule_20
                      deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptWhere = Anti_OptWhere String |
                Ctr__OptWhere__0 Decls
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Pat = Anti_Pat String |
           Ctr__Pat__0 Con Rule_29
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data QOp = Anti_QOp String |
           Ctr__QOp__0 ModIdList Op
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data QTyCls = Anti_QTyCls String |
              Ctr__QTyCls__0 ModIdList TyCls
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data QTyCon = Anti_QTyCon String |
              Ctr__QTyCon__0 ModIdList TyCon
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data QVar = Anti_QVar String |
            Ctr__QVar__0 QVarId
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data QVarId = Anti_QVarId String |
              Ctr__QVarId__0 ModIdList String
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data QVarList = Anti_QVarList String |
                Ctr__QVarList__0 Rule_16
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rhs = Anti_Rhs String |
           Ctr__Rhs__0 Exp OptWhere |
           Ctr__Rhs__1 GdRhs OptWhere
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_0 = Ctr__Rule_0__0 ExportsList Rule_1
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_1 = Ctr__Rule_1__0 |
              Ctr__Rule_1__1 Rule_2
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_10 = Ctr__Rule_10__0 TopDecls
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_11 = [ImpDecl]
type Rule_12 = [Import]
data Rule_13 = Anti_Rule_13 String |
               Ctr__Rule_13__1 ModId
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_15 = [CName]
type Rule_16 = [QVar]
data Rule_17 = Ctr__Rule_17__0 |
               Ctr__Rule_17__1 Rule_18
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_18 = Ctr__Rule_18__0 |
               Ctr__Rule_18__1 CNameList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_19 = Ctr__Rule_19__0
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_2 = Ctr__Rule_2__0
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_20 = Ctr__Rule_20__0 ModId
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_21 = Ctr__Rule_21__0 |
               Ctr__Rule_21__1 Rule_22
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_22 = Ctr__Rule_22__0
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_23 = Ctr__Rule_23__0 |
               Ctr__Rule_23__1 OptImpSpec
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_24 = [TopDecl]
data Rule_25 = Ctr__Rule_25__0 FunLhs |
               Ctr__Rule_25__1 Pat
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_26 = Ctr__Rule_26__0 Context
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_27 = Ctr__Rule_27__0 String
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_28 = [Op]
type Rule_29 = [Rule_30]
type Rule_3 = [Export]
data Rule_30 = Ctr__Rule_30__0 Var
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_31 = [Decl]
data Rule_32 = Ctr__Rule_32__0 GdRhs
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_33 = Ctr__Rule_33__0 OptContext Type
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_34 = [Rule_35]
data Rule_35 = Ctr__Rule_35__0 QOp ExpI
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_36 = [Constr]
type Rule_37 = [FieldDecl]
data Rule_38 = Ctr__Rule_38__0 Type |
               Ctr__Rule_38__1 AType
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_39 = [Var]
data Rule_4 = Ctr__Rule_4__0 |
              Ctr__Rule_4__1 Rule_5
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_40 = Ctr__Rule_40__0 Deriving
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_41 = Ctr__Rule_41__0 DClass |
               Ctr__Rule_41__1 DClassList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_42 = [DClass]
type Rule_43 = [Class]
data Rule_44 = Ctr__Rule_44__0 |
               Ctr__Rule_44__1 Rule_45
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_45 = Ctr__Rule_45__0 Type
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_46 = Ctr__Rule_46__0 |
               Ctr__Rule_46__1 Rule_47
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_47 = Ctr__Rule_47__0 BType
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_49 = Ctr__Rule_49__0 TypeList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_5 = Ctr__Rule_5__0 |
              Ctr__Rule_5__1 CNameList
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_50 = Ctr__Rule_50__0 |
               Ctr__Rule_50__1 Rule_51
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_51 = Ctr__Rule_51__0 Type
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_52 = [Type]
data Rule_6 = Ctr__Rule_6__0 |
              Ctr__Rule_6__1 Rule_7
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_7 = Ctr__Rule_7__0 |
              Ctr__Rule_7__1 QVarList
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_8 = Ctr__Rule_8__0 ImpDeclList Rule_9 |
              Ctr__Rule_8__1 ImpDeclList
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_9 = Ctr__Rule_9__0 |
              Ctr__Rule_9__1 Rule_10
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data SimpleType = Anti_SimpleType String |
                  Ctr__SimpleType__0 TyCon TyVars
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data TopDecl = Anti_TopDecl String |
               Ctr__TopDecl__0 SimpleType Type |
               Ctr__TopDecl__1 OptContext SimpleType Constrs OptDeriving |
               Ctr__TopDecl__2 Decl
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data TopDecls = Anti_TopDecls String |
                Ctr__TopDecls__0 Rule_24
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data TyCls = Anti_TyCls String |
             Ctr__TyCls__0 String
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data TyCon = Anti_TyCon String |
             Ctr__TyCon__0 String
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data TyVar = Anti_TyVar String |
             Ctr__TyVar__0 String
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type TyVars = [TyVar]
data Type = Anti_Type String |
            Ctr__Type__0 BType Rule_44
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data TypeList = Anti_TypeList String |
                Ctr__TypeList__0 Rule_52
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Var = Anti_Var String |
           Ctr__Var__0 String
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Vars = Anti_Vars String |
            Ctr__Vars__0 Rule_39
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
}