{
module HaskellLexer(alexScanTokens, Token(..), PosToken(..), AlexPosn(..))
where

 }
%wrapper "monad"

@decimal = ([0-9]+)
@octal = ([0-7]+)
@hexadecimal = ([0-9A-Fa-f]+)

tokens :- "tok_AType_dummy_121" { simple Tk__tok_AType_dummy_121 }
          "tok_ATypeList_dummy_120" { simple Tk__tok_ATypeList_dummy_120 }
          "tok_BType_dummy_119" { simple Tk__tok_BType_dummy_119 }
          "tok_Body_dummy_118" { simple Tk__tok_Body_dummy_118 }
          "tok_CName_dummy_117" { simple Tk__tok_CName_dummy_117 }
          "tok_CNameList_dummy_116" { simple Tk__tok_CNameList_dummy_116 }
          "tok_Class_dummy_115" { simple Tk__tok_Class_dummy_115 }
          "tok_ClassList_dummy_114" { simple Tk__tok_ClassList_dummy_114 }
          "tok_Con_dummy_113" { simple Tk__tok_Con_dummy_113 }
          "tok_Constr_dummy_112" { simple Tk__tok_Constr_dummy_112 }
          "tok_Constrs_dummy_111" { simple Tk__tok_Constrs_dummy_111 }
          "tok_Context_dummy_110" { simple Tk__tok_Context_dummy_110 }
          "tok_DClass_dummy_109" { simple Tk__tok_DClass_dummy_109 }
          "tok_DClassList_dummy_108" { simple Tk__tok_DClassList_dummy_108 }
          "tok_Decl_dummy_107" { simple Tk__tok_Decl_dummy_107 }
          "tok_DeclList_dummy_106" { simple Tk__tok_DeclList_dummy_106 }
          "tok_Decls_dummy_105" { simple Tk__tok_Decls_dummy_105 }
          "tok_Deriving_dummy_104" { simple Tk__tok_Deriving_dummy_104 }
          "tok_Exp_dummy_103" { simple Tk__tok_Exp_dummy_103 }
          "tok_ExpI_dummy_102" { simple Tk__tok_ExpI_dummy_102 }
          "tok_Export_dummy_101" { simple Tk__tok_Export_dummy_101 }
          "tok_ExportsList_dummy_100" { simple Tk__tok_ExportsList_dummy_100 }
          "tok_ExportsOpt_dummy_99" { simple Tk__tok_ExportsOpt_dummy_99 }
          "tok_FieldDecl_dummy_98" { simple Tk__tok_FieldDecl_dummy_98 }
          "tok_FieldDeclList_dummy_97" { simple Tk__tok_FieldDeclList_dummy_97 }
          "tok_Fixity_dummy_96" { simple Tk__tok_Fixity_dummy_96 }
          "tok_FunLhs_dummy_95" { simple Tk__tok_FunLhs_dummy_95 }
          "tok_GTyCon_dummy_94" { simple Tk__tok_GTyCon_dummy_94 }
          "tok_Gd_dummy_93" { simple Tk__tok_Gd_dummy_93 }
          "tok_GdRhs_dummy_92" { simple Tk__tok_GdRhs_dummy_92 }
          "tok_GenDecl_dummy_91" { simple Tk__tok_GenDecl_dummy_91 }
          "tok_Haskell_dummy_122" { simple Tk__tok_Haskell_dummy_122 }
          "tok_ImpDecl_dummy_90" { simple Tk__tok_ImpDecl_dummy_90 }
          "tok_ImpDeclList_dummy_89" { simple Tk__tok_ImpDeclList_dummy_89 }
          "tok_Import_dummy_88" { simple Tk__tok_Import_dummy_88 }
          "tok_ImportList_dummy_87" { simple Tk__tok_ImportList_dummy_87 }
          "tok_ModId_dummy_86" { simple Tk__tok_ModId_dummy_86 }
          "tok_ModIdList_dummy_85" { simple Tk__tok_ModIdList_dummy_85 }
          "tok_Module_dummy_84" { simple Tk__tok_Module_dummy_84 }
          "tok_Op_dummy_83" { simple Tk__tok_Op_dummy_83 }
          "tok_Ops_dummy_82" { simple Tk__tok_Ops_dummy_82 }
          "tok_OptContext_dummy_81" { simple Tk__tok_OptContext_dummy_81 }
          "tok_OptDeriving_dummy_80" { simple Tk__tok_OptDeriving_dummy_80 }
          "tok_OptExpTypeSignature_dummy_79" { simple Tk__tok_OptExpTypeSignature_dummy_79 }
          "tok_OptGdRhs_dummy_78" { simple Tk__tok_OptGdRhs_dummy_78 }
          "tok_OptImpSpec_dummy_77" { simple Tk__tok_OptImpSpec_dummy_77 }
          "tok_OptInteger_dummy_76" { simple Tk__tok_OptInteger_dummy_76 }
          "tok_OptQualified_dummy_75" { simple Tk__tok_OptQualified_dummy_75 }
          "tok_OptQualifiedAs_dummy_74" { simple Tk__tok_OptQualifiedAs_dummy_74 }
          "tok_OptWhere_dummy_73" { simple Tk__tok_OptWhere_dummy_73 }
          "tok_Pat_dummy_72" { simple Tk__tok_Pat_dummy_72 }
          "tok_QOp_dummy_71" { simple Tk__tok_QOp_dummy_71 }
          "tok_QTyCls_dummy_70" { simple Tk__tok_QTyCls_dummy_70 }
          "tok_QTyCon_dummy_69" { simple Tk__tok_QTyCon_dummy_69 }
          "tok_QVar_dummy_68" { simple Tk__tok_QVar_dummy_68 }
          "tok_QVarId_dummy_67" { simple Tk__tok_QVarId_dummy_67 }
          "tok_QVarList_dummy_66" { simple Tk__tok_QVarList_dummy_66 }
          "tok_Rhs_dummy_65" { simple Tk__tok_Rhs_dummy_65 }
          "tok_SimpleType_dummy_64" { simple Tk__tok_SimpleType_dummy_64 }
          "tok_TopDecl_dummy_63" { simple Tk__tok_TopDecl_dummy_63 }
          "tok_TopDecls_dummy_62" { simple Tk__tok_TopDecls_dummy_62 }
          "tok_TyCls_dummy_61" { simple Tk__tok_TyCls_dummy_61 }
          "tok_TyCon_dummy_60" { simple Tk__tok_TyCon_dummy_60 }
          "tok_TyVar_dummy_59" { simple Tk__tok_TyVar_dummy_59 }
          "tok_TyVars_dummy_58" { simple Tk__tok_TyVars_dummy_58 }
          "tok_Type_dummy_57" { simple Tk__tok_Type_dummy_57 }
          "tok_TypeList_dummy_56" { simple Tk__tok_TypeList_dummy_56 }
          "tok_Var_dummy_55" { simple Tk__tok_Var_dummy_55 }
          "tok_Vars_dummy_54" { simple Tk__tok_Vars_dummy_54 }
          "}" { simple Tk__tok__symbol__8 }
          "|" { simple Tk__tok__pipe__21 }
          "{" { simple Tk__tok__symbol__6 }
          "where" { simple Tk__tok_where_1 }
          "type" { simple Tk__tok_type_13 }
          "qualified" { simple Tk__tok_qualified_10 }
          "module" { simple Tk__tok_module_0 }
          "infixr" { simple Tk__tok_infixr_19 }
          "infixl" { simple Tk__tok_infixl_18 }
          "infix" { simple Tk__tok_infix_20 }
          "import" { simple Tk__tok_import_12 }
          "deriving" { simple Tk__tok_deriving_23 }
          "data" { simple Tk__tok_data_15 }
          "as" { simple Tk__tok_as_11 }
          "]" { simple Tk__tok__sq_bkt_r__26 }
          "[" { simple Tk__tok__sq_bkt_l__25 }
          "=>" { simple Tk__tok__eql__symbol__16 }
          "=" { simple Tk__tok__eql__14 }
          ";" { simple Tk__tok__semi__7 }
          "::" { simple Tk__tok__colon__colon__17 }
          ".." { simple Tk__tok__dot__dot__5 }
          "." { simple Tk__tok__dot__9 }
          "->" { simple Tk__tok__minus__symbol__24 }
          "," { simple Tk__tok__coma__3 }
          ")" { simple Tk__tok__rparen__4 }
          "(" { simple Tk__tok__lparen__2 }
          "!" { simple Tk__tok__exclamation__22 }
          ("$("  ([^\)]| [\n])*  ")") { simple1 $  Tk__th . (id) }
          ("{-"  (.| [\n])*  "-}") { simple1 $  Tk__ncomment . (id) }
          ("--"  .*  [\n]) ;
          ([\ \t\n]) { simple1 $  Tk__whitespace . (id) }
          (@decimal| ("0o"| "0O")  @octal| ("0x"| "0X")  @hexadecimal) { simple1 $  Tk__integer . (id) }
          ([0-9A-Fa-f]+) { simple1 $  Tk__hexadecimal . (id) }
          ([0-7]+) { simple1 $  Tk__octal . (id) }
          ([0-9]+) { simple1 $  Tk__decimal . (id) }
          ("$"  "QOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_QOp . ((tail . dropWhile (/= ':'))) }
          ("$"  "Op"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Op . ((tail . dropWhile (/= ':'))) }
          ("$"  "TyCls"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TyCls . ((tail . dropWhile (/= ':'))) }
          ("$"  "ModId"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ModId . ((tail . dropWhile (/= ':'))) }
          ("$"  "TyCon"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TyCon . ((tail . dropWhile (/= ':'))) }
          ("$"  "TyVar"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TyVar . ((tail . dropWhile (/= ':'))) }
          ([a-zA-Z_]  [a-zA-Z_0-9]*) { simple1 $  Tk__varid . (id) }
          ([A-Z]  [a-zA-Z_0-9]*) { simple1 $  Tk__conid . (id) }
          ("$"  "TyVars"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TyVars . ((tail . dropWhile (/= ':'))) }
          ("$"  "SimpleType"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_SimpleType . ((tail . dropWhile (/= ':'))) }
          ("$"  "TypeList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TypeList . ((tail . dropWhile (/= ':'))) }
          ("$"  "GTyCon"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_GTyCon . ((tail . dropWhile (/= ':'))) }
          ("$"  "AType"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AType . ((tail . dropWhile (/= ':'))) }
          ("$"  "ATypeList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ATypeList . ((tail . dropWhile (/= ':'))) }
          ("$"  "BType"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_BType . ((tail . dropWhile (/= ':'))) }
          ("$"  "Type"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Type . ((tail . dropWhile (/= ':'))) }
          ("$"  "Class"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Class . ((tail . dropWhile (/= ':'))) }
          ("$"  "ClassList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ClassList . ((tail . dropWhile (/= ':'))) }
          ("$"  "Context"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Context . ((tail . dropWhile (/= ':'))) }
          ("$"  "DClass"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_DClass . ((tail . dropWhile (/= ':'))) }
          ("$"  "DClassList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_DClassList . ((tail . dropWhile (/= ':'))) }
          ("$"  "Deriving"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Deriving . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptDeriving"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptDeriving . ((tail . dropWhile (/= ':'))) }
          ("$"  "Vars"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Vars . ((tail . dropWhile (/= ':'))) }
          ("$"  "FieldDecl"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_FieldDecl . ((tail . dropWhile (/= ':'))) }
          ("$"  "FieldDeclList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_FieldDeclList . ((tail . dropWhile (/= ':'))) }
          ("$"  "Constr"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Constr . ((tail . dropWhile (/= ':'))) }
          ("$"  "Constrs"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Constrs . ((tail . dropWhile (/= ':'))) }
          ("$"  "GdRhs"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_GdRhs . ((tail . dropWhile (/= ':'))) }
          ("$"  "ExpI"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ExpI . ((tail . dropWhile (/= ':'))) }
          ("$"  "Exp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Exp . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptExpTypeSignature"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptExpTypeSignature . ((tail . dropWhile (/= ':'))) }
          ("$"  "Gd"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Gd . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptGdRhs"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptGdRhs . ((tail . dropWhile (/= ':'))) }
          ("$"  "Rhs"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Rhs . ((tail . dropWhile (/= ':'))) }
          ("$"  "Decls"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Decls . ((tail . dropWhile (/= ':'))) }
          ("$"  "DeclList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_DeclList . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptWhere"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptWhere . ((tail . dropWhile (/= ':'))) }
          ("$"  "Pat"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Pat . ((tail . dropWhile (/= ':'))) }
          ("$"  "FunLhs"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_FunLhs . ((tail . dropWhile (/= ':'))) }
          ("$"  "Fixity"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Fixity . ((tail . dropWhile (/= ':'))) }
          ("$"  "Ops"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Ops . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptInteger"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptInteger . ((tail . dropWhile (/= ':'))) }
          ("$"  "GenDecl"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_GenDecl . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptContext"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptContext . ((tail . dropWhile (/= ':'))) }
          ("$"  "Decl"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Decl . ((tail . dropWhile (/= ':'))) }
          ("$"  "TopDecl"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TopDecl . ((tail . dropWhile (/= ':'))) }
          ("$"  "TopDecls"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TopDecls . ((tail . dropWhile (/= ':'))) }
          ("$"  "ImpDecl"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ImpDecl . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptImpSpec"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptImpSpec . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptQualifiedAs"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptQualifiedAs . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptQualified"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptQualified . ((tail . dropWhile (/= ':'))) }
          ("$"  "Import"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Import . ((tail . dropWhile (/= ':'))) }
          ("$"  "QVarList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_QVarList . ((tail . dropWhile (/= ':'))) }
          ("$"  "CNameList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_CNameList . ((tail . dropWhile (/= ':'))) }
          ("$"  "CName"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_CName . ((tail . dropWhile (/= ':'))) }
          ("$"  "QTyCon"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_QTyCon . ((tail . dropWhile (/= ':'))) }
          ("$"  "QTyCls"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_QTyCls . ((tail . dropWhile (/= ':'))) }
          ("$"  "QVar"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_QVar . ((tail . dropWhile (/= ':'))) }
          ("$"  "QVarId"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_QVarId . ((tail . dropWhile (/= ':'))) }
          ("$"  "ModIdList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ModIdList . ((tail . dropWhile (/= ':'))) }
          ("$"  "Con"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Con . ((tail . dropWhile (/= ':'))) }
          ("$"  "Var"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Var . ((tail . dropWhile (/= ':'))) }
          ("$"  "ImportList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ImportList . ((tail . dropWhile (/= ':'))) }
          ("$"  "ImpDeclList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ImpDeclList . ((tail . dropWhile (/= ':'))) }
          ("$"  "Body"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Body . ((tail . dropWhile (/= ':'))) }
          ("$"  "Export"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Export . ((tail . dropWhile (/= ':'))) }
          ("$"  "ExportsList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ExportsList . ((tail . dropWhile (/= ':'))) }
          ("$"  "ExportsOpt"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ExportsOpt . ((tail . dropWhile (/= ':'))) }
          ("$"  "Module"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Module . ((tail . dropWhile (/= ':'))) }
          ("$"  "Haskell"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Haskell . ((tail . dropWhile (/= ':'))) }
          . { rtkError }

{
data Token = EndOfFile |
             Tk__tok_AType_dummy_121 |
             Tk__tok_ATypeList_dummy_120 |
             Tk__tok_BType_dummy_119 |
             Tk__tok_Body_dummy_118 |
             Tk__tok_CName_dummy_117 |
             Tk__tok_CNameList_dummy_116 |
             Tk__tok_Class_dummy_115 |
             Tk__tok_ClassList_dummy_114 |
             Tk__tok_Con_dummy_113 |
             Tk__tok_Constr_dummy_112 |
             Tk__tok_Constrs_dummy_111 |
             Tk__tok_Context_dummy_110 |
             Tk__tok_DClass_dummy_109 |
             Tk__tok_DClassList_dummy_108 |
             Tk__tok_Decl_dummy_107 |
             Tk__tok_DeclList_dummy_106 |
             Tk__tok_Decls_dummy_105 |
             Tk__tok_Deriving_dummy_104 |
             Tk__tok_Exp_dummy_103 |
             Tk__tok_ExpI_dummy_102 |
             Tk__tok_Export_dummy_101 |
             Tk__tok_ExportsList_dummy_100 |
             Tk__tok_ExportsOpt_dummy_99 |
             Tk__tok_FieldDecl_dummy_98 |
             Tk__tok_FieldDeclList_dummy_97 |
             Tk__tok_Fixity_dummy_96 |
             Tk__tok_FunLhs_dummy_95 |
             Tk__tok_GTyCon_dummy_94 |
             Tk__tok_Gd_dummy_93 |
             Tk__tok_GdRhs_dummy_92 |
             Tk__tok_GenDecl_dummy_91 |
             Tk__tok_Haskell_dummy_122 |
             Tk__tok_ImpDecl_dummy_90 |
             Tk__tok_ImpDeclList_dummy_89 |
             Tk__tok_Import_dummy_88 |
             Tk__tok_ImportList_dummy_87 |
             Tk__tok_ModId_dummy_86 |
             Tk__tok_ModIdList_dummy_85 |
             Tk__tok_Module_dummy_84 |
             Tk__tok_Op_dummy_83 |
             Tk__tok_Ops_dummy_82 |
             Tk__tok_OptContext_dummy_81 |
             Tk__tok_OptDeriving_dummy_80 |
             Tk__tok_OptExpTypeSignature_dummy_79 |
             Tk__tok_OptGdRhs_dummy_78 |
             Tk__tok_OptImpSpec_dummy_77 |
             Tk__tok_OptInteger_dummy_76 |
             Tk__tok_OptQualified_dummy_75 |
             Tk__tok_OptQualifiedAs_dummy_74 |
             Tk__tok_OptWhere_dummy_73 |
             Tk__tok_Pat_dummy_72 |
             Tk__tok_QOp_dummy_71 |
             Tk__tok_QTyCls_dummy_70 |
             Tk__tok_QTyCon_dummy_69 |
             Tk__tok_QVar_dummy_68 |
             Tk__tok_QVarId_dummy_67 |
             Tk__tok_QVarList_dummy_66 |
             Tk__tok_Rhs_dummy_65 |
             Tk__tok_SimpleType_dummy_64 |
             Tk__tok_TopDecl_dummy_63 |
             Tk__tok_TopDecls_dummy_62 |
             Tk__tok_TyCls_dummy_61 |
             Tk__tok_TyCon_dummy_60 |
             Tk__tok_TyVar_dummy_59 |
             Tk__tok_TyVars_dummy_58 |
             Tk__tok_Type_dummy_57 |
             Tk__tok_TypeList_dummy_56 |
             Tk__tok_Var_dummy_55 |
             Tk__tok_Vars_dummy_54 |
             Tk__tok__symbol__8 |
             Tk__tok__pipe__21 |
             Tk__tok__symbol__6 |
             Tk__tok_where_1 |
             Tk__tok_type_13 |
             Tk__tok_qualified_10 |
             Tk__tok_module_0 |
             Tk__tok_infixr_19 |
             Tk__tok_infixl_18 |
             Tk__tok_infix_20 |
             Tk__tok_import_12 |
             Tk__tok_deriving_23 |
             Tk__tok_data_15 |
             Tk__tok_as_11 |
             Tk__tok__sq_bkt_r__26 |
             Tk__tok__sq_bkt_l__25 |
             Tk__tok__eql__symbol__16 |
             Tk__tok__eql__14 |
             Tk__tok__semi__7 |
             Tk__tok__colon__colon__17 |
             Tk__tok__dot__dot__5 |
             Tk__tok__dot__9 |
             Tk__tok__minus__symbol__24 |
             Tk__tok__coma__3 |
             Tk__tok__rparen__4 |
             Tk__tok__lparen__2 |
             Tk__tok__exclamation__22 |
             Tk__th String |
             Tk__ncomment String |
             Tk__whitespace String |
             Tk__integer String |
             Tk__hexadecimal String |
             Tk__octal String |
             Tk__decimal String |
             Tk__qq_QOp String |
             Tk__qq_Op String |
             Tk__qq_TyCls String |
             Tk__qq_ModId String |
             Tk__qq_TyCon String |
             Tk__qq_TyVar String |
             Tk__varid String |
             Tk__conid String |
             Tk__qq_TyVars String |
             Tk__qq_SimpleType String |
             Tk__qq_TypeList String |
             Tk__qq_GTyCon String |
             Tk__qq_AType String |
             Tk__qq_ATypeList String |
             Tk__qq_BType String |
             Tk__qq_Type String |
             Tk__qq_Class String |
             Tk__qq_ClassList String |
             Tk__qq_Context String |
             Tk__qq_DClass String |
             Tk__qq_DClassList String |
             Tk__qq_Deriving String |
             Tk__qq_OptDeriving String |
             Tk__qq_Vars String |
             Tk__qq_FieldDecl String |
             Tk__qq_FieldDeclList String |
             Tk__qq_Constr String |
             Tk__qq_Constrs String |
             Tk__qq_GdRhs String |
             Tk__qq_ExpI String |
             Tk__qq_Exp String |
             Tk__qq_OptExpTypeSignature String |
             Tk__qq_Gd String |
             Tk__qq_OptGdRhs String |
             Tk__qq_Rhs String |
             Tk__qq_Decls String |
             Tk__qq_DeclList String |
             Tk__qq_OptWhere String |
             Tk__qq_Pat String |
             Tk__qq_FunLhs String |
             Tk__qq_Fixity String |
             Tk__qq_Ops String |
             Tk__qq_OptInteger String |
             Tk__qq_GenDecl String |
             Tk__qq_OptContext String |
             Tk__qq_Decl String |
             Tk__qq_TopDecl String |
             Tk__qq_TopDecls String |
             Tk__qq_ImpDecl String |
             Tk__qq_OptImpSpec String |
             Tk__qq_OptQualifiedAs String |
             Tk__qq_OptQualified String |
             Tk__qq_Import String |
             Tk__qq_QVarList String |
             Tk__qq_CNameList String |
             Tk__qq_CName String |
             Tk__qq_QTyCon String |
             Tk__qq_QTyCls String |
             Tk__qq_QVar String |
             Tk__qq_QVarId String |
             Tk__qq_ModIdList String |
             Tk__qq_Con String |
             Tk__qq_Var String |
             Tk__qq_ImportList String |
             Tk__qq_ImpDeclList String |
             Tk__qq_Body String |
             Tk__qq_Export String |
             Tk__qq_ExportsList String |
             Tk__qq_ExportsOpt String |
             Tk__qq_Module String |
             Tk__qq_Haskell String
             deriving (Show)

-- A token together with the source position where it starts
data PosToken = PosToken { ptPos :: AlexPosn, ptToken :: Token }
                deriving (Show)

alexEOF = do
  (pos, _, _, _) <- alexGetInput
  return $ PosToken pos EndOfFile

-- The returned list always ends with an EndOfFile token that carries the
-- position of the end of input, so parse errors at end of input can be
-- reported with a position too
alexScanTokens :: String -> [PosToken]
alexScanTokens str =
               case alexScanTokens1 str of
                  Right toks -> toks
                  Left err -> errorWithoutStackTrace err

alexScanTokens1 str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                       PosToken _ EndOfFile -> return $ reverse (tok : toks)
                       _ -> let toks' = tok : toks
                            in toks' `seq` loop toks'
  loop []

simple1 :: (String -> Token) -> AlexInput -> Int -> Alex PosToken
simple1 t (pos, _, _, str) len = return $ PosToken pos (t (take len str))

simple :: Token -> AlexInput -> Int -> Alex PosToken
simple t (pos, _, _, _) len = return $ PosToken pos t

rtkError ((AlexPn _ line column), _, _, str) len = alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column) ++ ". Following chars: " ++ (take 10 str)

}