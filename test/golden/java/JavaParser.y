{
{-# LANGUAGE DeriveDataTypeable #-}
module JavaParser where
import qualified Data.Generics as Gen
import qualified JavaLexer as L (Token(..), PosToken(..), AlexPosn(..), alexScanTokens)
}

%name parseJava
%tokentype { L.PosToken }
%monad { Either String }
%error { parseError }

%token

rtk__eof { L.PosToken _ L.EndOfFile }
tok_AdditiveOp_dummy_162 { L.PosToken _ L.Tk__tok_AdditiveOp_dummy_162 }
tok_Annotation_dummy_161 { L.PosToken _ L.Tk__tok_Annotation_dummy_161 }
tok_AnnotationArguments_dummy_160 { L.PosToken _ L.Tk__tok_AnnotationArguments_dummy_160 }
tok_AnnotationDeclaration_dummy_159 { L.PosToken _ L.Tk__tok_AnnotationDeclaration_dummy_159 }
tok_AnnotationElement_dummy_158 { L.PosToken _ L.Tk__tok_AnnotationElement_dummy_158 }
tok_AnnotationList_dummy_157 { L.PosToken _ L.Tk__tok_AnnotationList_dummy_157 }
tok_AnnotationTypeElement_dummy_156 { L.PosToken _ L.Tk__tok_AnnotationTypeElement_dummy_156 }
tok_AnnotationTypeElementList_dummy_155 { L.PosToken _ L.Tk__tok_AnnotationTypeElementList_dummy_155 }
tok_Arglist_dummy_154 { L.PosToken _ L.Tk__tok_Arglist_dummy_154 }
tok_AssignmentOp_dummy_153 { L.PosToken _ L.Tk__tok_AssignmentOp_dummy_153 }
tok_CatchList_dummy_152 { L.PosToken _ L.Tk__tok_CatchList_dummy_152 }
tok_ClassDeclaration_dummy_151 { L.PosToken _ L.Tk__tok_ClassDeclaration_dummy_151 }
tok_CompilationUnit_dummy_150 { L.PosToken _ L.Tk__tok_CompilationUnit_dummy_150 }
tok_CompoundName_dummy_149 { L.PosToken _ L.Tk__tok_CompoundName_dummy_149 }
tok_CreationExpression_dummy_148 { L.PosToken _ L.Tk__tok_CreationExpression_dummy_148 }
tok_DimExprs_dummy_147 { L.PosToken _ L.Tk__tok_DimExprs_dummy_147 }
tok_Dims_dummy_146 { L.PosToken _ L.Tk__tok_Dims_dummy_146 }
tok_DoStatement_dummy_145 { L.PosToken _ L.Tk__tok_DoStatement_dummy_145 }
tok_DocComment_dummy_144 { L.PosToken _ L.Tk__tok_DocComment_dummy_144 }
tok_EnumConstant_dummy_143 { L.PosToken _ L.Tk__tok_EnumConstant_dummy_143 }
tok_EnumConstantList_dummy_142 { L.PosToken _ L.Tk__tok_EnumConstantList_dummy_142 }
tok_EnumDeclaration_dummy_141 { L.PosToken _ L.Tk__tok_EnumDeclaration_dummy_141 }
tok_EqualityOp_dummy_140 { L.PosToken _ L.Tk__tok_EqualityOp_dummy_140 }
tok_Expression_dummy_139 { L.PosToken _ L.Tk__tok_Expression_dummy_139 }
tok_ExtendsList_dummy_138 { L.PosToken _ L.Tk__tok_ExtendsList_dummy_138 }
tok_FieldDeclaration_dummy_137 { L.PosToken _ L.Tk__tok_FieldDeclaration_dummy_137 }
tok_FieldDeclarationList_dummy_136 { L.PosToken _ L.Tk__tok_FieldDeclarationList_dummy_136 }
tok_ForStatement_dummy_135 { L.PosToken _ L.Tk__tok_ForStatement_dummy_135 }
tok_IfStatement_dummy_134 { L.PosToken _ L.Tk__tok_IfStatement_dummy_134 }
tok_ImplementsList_dummy_133 { L.PosToken _ L.Tk__tok_ImplementsList_dummy_133 }
tok_ImportList_dummy_132 { L.PosToken _ L.Tk__tok_ImportList_dummy_132 }
tok_ImportStatement_dummy_131 { L.PosToken _ L.Tk__tok_ImportStatement_dummy_131 }
tok_InterfaceDeclaration_dummy_130 { L.PosToken _ L.Tk__tok_InterfaceDeclaration_dummy_130 }
tok_Java_dummy_163 { L.PosToken _ L.Tk__tok_Java_dummy_163 }
tok_Literal_dummy_129 { L.PosToken _ L.Tk__tok_Literal_dummy_129 }
tok_MemberAfterFirstId_dummy_128 { L.PosToken _ L.Tk__tok_MemberAfterFirstId_dummy_128 }
tok_MemberDeclaration_dummy_127 { L.PosToken _ L.Tk__tok_MemberDeclaration_dummy_127 }
tok_MemberRest_dummy_126 { L.PosToken _ L.Tk__tok_MemberRest_dummy_126 }
tok_Modifier_dummy_125 { L.PosToken _ L.Tk__tok_Modifier_dummy_125 }
tok_ModifierList_dummy_124 { L.PosToken _ L.Tk__tok_ModifierList_dummy_124 }
tok_MoreTypeSpecifier_dummy_123 { L.PosToken _ L.Tk__tok_MoreTypeSpecifier_dummy_123 }
tok_MoreVariableDeclarators_dummy_122 { L.PosToken _ L.Tk__tok_MoreVariableDeclarators_dummy_122 }
tok_MultiplicativeOp_dummy_121 { L.PosToken _ L.Tk__tok_MultiplicativeOp_dummy_121 }
tok_NonEmptyDims_dummy_120 { L.PosToken _ L.Tk__tok_NonEmptyDims_dummy_120 }
tok_NonEmptyTypeArguments_dummy_119 { L.PosToken _ L.Tk__tok_NonEmptyTypeArguments_dummy_119 }
tok_OptDocComment_dummy_118 { L.PosToken _ L.Tk__tok_OptDocComment_dummy_118 }
tok_OptElsePart_dummy_117 { L.PosToken _ L.Tk__tok_OptElsePart_dummy_117 }
tok_OptExpression_dummy_116 { L.PosToken _ L.Tk__tok_OptExpression_dummy_116 }
tok_OptFinally_dummy_115 { L.PosToken _ L.Tk__tok_OptFinally_dummy_115 }
tok_OptId_dummy_114 { L.PosToken _ L.Tk__tok_OptId_dummy_114 }
tok_OptVariableInitializer_dummy_113 { L.PosToken _ L.Tk__tok_OptVariableInitializer_dummy_113 }
tok_Package_dummy_112 { L.PosToken _ L.Tk__tok_Package_dummy_112 }
tok_Parameter_dummy_111 { L.PosToken _ L.Tk__tok_Parameter_dummy_111 }
tok_ParameterList_dummy_110 { L.PosToken _ L.Tk__tok_ParameterList_dummy_110 }
tok_PostfixOp_dummy_109 { L.PosToken _ L.Tk__tok_PostfixOp_dummy_109 }
tok_PrefixOp_dummy_108 { L.PosToken _ L.Tk__tok_PrefixOp_dummy_108 }
tok_PrimitiveTypeKeyword_dummy_107 { L.PosToken _ L.Tk__tok_PrimitiveTypeKeyword_dummy_107 }
tok_RelationalOp_dummy_106 { L.PosToken _ L.Tk__tok_RelationalOp_dummy_106 }
tok_ShiftOp_dummy_105 { L.PosToken _ L.Tk__tok_ShiftOp_dummy_105 }
tok_Statement_dummy_104 { L.PosToken _ L.Tk__tok_Statement_dummy_104 }
tok_StatementBlock_dummy_103 { L.PosToken _ L.Tk__tok_StatementBlock_dummy_103 }
tok_StatementList_dummy_102 { L.PosToken _ L.Tk__tok_StatementList_dummy_102 }
tok_StaticInitializer_dummy_101 { L.PosToken _ L.Tk__tok_StaticInitializer_dummy_101 }
tok_SwitchCaseList_dummy_100 { L.PosToken _ L.Tk__tok_SwitchCaseList_dummy_100 }
tok_SwitchStatement_dummy_99 { L.PosToken _ L.Tk__tok_SwitchStatement_dummy_99 }
tok_TryStatement_dummy_98 { L.PosToken _ L.Tk__tok_TryStatement_dummy_98 }
tok_Type_dummy_97 { L.PosToken _ L.Tk__tok_Type_dummy_97 }
tok_TypeArgument_dummy_96 { L.PosToken _ L.Tk__tok_TypeArgument_dummy_96 }
tok_TypeArguments_dummy_95 { L.PosToken _ L.Tk__tok_TypeArguments_dummy_95 }
tok_TypeDeclRest_dummy_94 { L.PosToken _ L.Tk__tok_TypeDeclRest_dummy_94 }
tok_TypeDeclaration_dummy_93 { L.PosToken _ L.Tk__tok_TypeDeclaration_dummy_93 }
tok_TypeParameter_dummy_92 { L.PosToken _ L.Tk__tok_TypeParameter_dummy_92 }
tok_TypeParameters_dummy_91 { L.PosToken _ L.Tk__tok_TypeParameters_dummy_91 }
tok_TypeSpecifier_dummy_90 { L.PosToken _ L.Tk__tok_TypeSpecifier_dummy_90 }
tok_VariableDeclaration_dummy_89 { L.PosToken _ L.Tk__tok_VariableDeclaration_dummy_89 }
tok_VariableDeclarator_dummy_88 { L.PosToken _ L.Tk__tok_VariableDeclarator_dummy_88 }
tok_VariableDeclaratorList_dummy_87 { L.PosToken _ L.Tk__tok_VariableDeclaratorList_dummy_87 }
tok_VariableInitializer_dummy_86 { L.PosToken _ L.Tk__tok_VariableInitializer_dummy_86 }
tok_VariableInitializerList_dummy_85 { L.PosToken _ L.Tk__tok_VariableInitializerList_dummy_85 }
tok_WhileStatement_dummy_84 { L.PosToken _ L.Tk__tok_WhileStatement_dummy_84 }
tok_WildcardType_dummy_83 { L.PosToken _ L.Tk__tok_WildcardType_dummy_83 }
tok__tilde__78 { L.PosToken _ L.Tk__tok__tilde__78 }
tok__symbol__14 { L.PosToken _ L.Tk__tok__symbol__14 }
tok__pipe__pipe__57 { L.PosToken _ L.Tk__tok__pipe__pipe__57 }
tok__pipe__eql__49 { L.PosToken _ L.Tk__tok__pipe__eql__49 }
tok__pipe__59 { L.PosToken _ L.Tk__tok__pipe__59 }
tok__symbol__13 { L.PosToken _ L.Tk__tok__symbol__13 }
tok_while_38 { L.PosToken _ L.Tk__tok_while_38 }
tok_void_27 { L.PosToken _ L.Tk__tok_void_27 }
tok_try_42 { L.PosToken _ L.Tk__tok_try_42 }
tok_true_83 { L.PosToken _ L.Tk__tok_true_83 }
tok_transient_94 { L.PosToken _ L.Tk__tok_transient_94 }
tok_throw_31 { L.PosToken _ L.Tk__tok_throw_31 }
tok_threadsafe_93 { L.PosToken _ L.Tk__tok_threadsafe_93 }
tok_this_80 { L.PosToken _ L.Tk__tok_this_80 }
tok_synchronized_30 { L.PosToken _ L.Tk__tok_synchronized_30 }
tok_switch_44 { L.PosToken _ L.Tk__tok_switch_44 }
tok_super_81 { L.PosToken _ L.Tk__tok_super_81 }
tok_static_89 { L.PosToken _ L.Tk__tok_static_89 }
tok_short_22 { L.PosToken _ L.Tk__tok_short_22 }
tok_return_29 { L.PosToken _ L.Tk__tok_return_29 }
tok_public_86 { L.PosToken _ L.Tk__tok_public_86 }
tok_protected_88 { L.PosToken _ L.Tk__tok_protected_88 }
tok_private_87 { L.PosToken _ L.Tk__tok_private_87 }
tok_package_0 { L.PosToken _ L.Tk__tok_package_0 }
tok_null_85 { L.PosToken _ L.Tk__tok_null_85 }
tok_new_82 { L.PosToken _ L.Tk__tok_new_82 }
tok_native_91 { L.PosToken _ L.Tk__tok_native_91 }
tok_long_25 { L.PosToken _ L.Tk__tok_long_25 }
tok_interface_15 { L.PosToken _ L.Tk__tok_interface_15 }
tok_int_23 { L.PosToken _ L.Tk__tok_int_23 }
tok_instanceof_68 { L.PosToken _ L.Tk__tok_instanceof_68 }
tok_import_2 { L.PosToken _ L.Tk__tok_import_2 }
tok_implements_11 { L.PosToken _ L.Tk__tok_implements_11 }
tok_if_36 { L.PosToken _ L.Tk__tok_if_36 }
tok_for_39 { L.PosToken _ L.Tk__tok_for_39 }
tok_float_24 { L.PosToken _ L.Tk__tok_float_24 }
tok_finally_41 { L.PosToken _ L.Tk__tok_finally_41 }
tok_final_90 { L.PosToken _ L.Tk__tok_final_90 }
tok_false_84 { L.PosToken _ L.Tk__tok_false_84 }
tok_extends_10 { L.PosToken _ L.Tk__tok_extends_10 }
tok_enum_16 { L.PosToken _ L.Tk__tok_enum_16 }
tok_else_35 { L.PosToken _ L.Tk__tok_else_35 }
tok_double_26 { L.PosToken _ L.Tk__tok_double_26 }
tok_do_37 { L.PosToken _ L.Tk__tok_do_37 }
tok_default_28 { L.PosToken _ L.Tk__tok_default_28 }
tok_continue_34 { L.PosToken _ L.Tk__tok_continue_34 }
tok_class_12 { L.PosToken _ L.Tk__tok_class_12 }
tok_char_21 { L.PosToken _ L.Tk__tok_char_21 }
tok_catch_40 { L.PosToken _ L.Tk__tok_catch_40 }
tok_case_43 { L.PosToken _ L.Tk__tok_case_43 }
tok_byte_20 { L.PosToken _ L.Tk__tok_byte_20 }
tok_break_33 { L.PosToken _ L.Tk__tok_break_33 }
tok_boolean_19 { L.PosToken _ L.Tk__tok_boolean_19 }
tok_abstract_92 { L.PosToken _ L.Tk__tok_abstract_92 }
tok__symbol__eql__51 { L.PosToken _ L.Tk__tok__symbol__eql__51 }
tok__symbol__60 { L.PosToken _ L.Tk__tok__symbol__60 }
tok__sq_bkt_r__18 { L.PosToken _ L.Tk__tok__sq_bkt_r__18 }
tok__sq_bkt_l__17 { L.PosToken _ L.Tk__tok__sq_bkt_l__17 }
tok__symbol__5 { L.PosToken _ L.Tk__tok__symbol__5 }
tok__symbol__56 { L.PosToken _ L.Tk__tok__symbol__56 }
tok__symbol__symbol__symbol__eql__55 { L.PosToken _ L.Tk__tok__symbol__symbol__symbol__eql__55 }
tok__symbol__symbol__symbol__71 { L.PosToken _ L.Tk__tok__symbol__symbol__symbol__71 }
tok__symbol__symbol__eql__54 { L.PosToken _ L.Tk__tok__symbol__symbol__eql__54 }
tok__symbol__symbol__69 { L.PosToken _ L.Tk__tok__symbol__symbol__69 }
tok__symbol__eql__67 { L.PosToken _ L.Tk__tok__symbol__eql__67 }
tok__symbol__65 { L.PosToken _ L.Tk__tok__symbol__65 }
tok__eql__eql__62 { L.PosToken _ L.Tk__tok__eql__eql__62 }
tok__eql__9 { L.PosToken _ L.Tk__tok__eql__9 }
tok__symbol__eql__66 { L.PosToken _ L.Tk__tok__symbol__eql__66 }
tok__symbol__symbol__eql__53 { L.PosToken _ L.Tk__tok__symbol__symbol__eql__53 }
tok__symbol__symbol__70 { L.PosToken _ L.Tk__tok__symbol__symbol__70 }
tok__symbol__64 { L.PosToken _ L.Tk__tok__symbol__64 }
tok__semi__1 { L.PosToken _ L.Tk__tok__semi__1 }
tok__colon__32 { L.PosToken _ L.Tk__tok__colon__32 }
tok__symbol__eql__48 { L.PosToken _ L.Tk__tok__symbol__eql__48 }
tok__symbol__74 { L.PosToken _ L.Tk__tok__symbol__74 }
tok__dot__3 { L.PosToken _ L.Tk__tok__dot__3 }
tok__minus__eql__46 { L.PosToken _ L.Tk__tok__minus__eql__46 }
tok__minus__minus__77 { L.PosToken _ L.Tk__tok__minus__minus__77 }
tok__minus__73 { L.PosToken _ L.Tk__tok__minus__73 }
tok__coma__8 { L.PosToken _ L.Tk__tok__coma__8 }
tok__plus__eql__45 { L.PosToken _ L.Tk__tok__plus__eql__45 }
tok__plus__plus__76 { L.PosToken _ L.Tk__tok__plus__plus__76 }
tok__plus__72 { L.PosToken _ L.Tk__tok__plus__72 }
tok__star__eql__47 { L.PosToken _ L.Tk__tok__star__eql__47 }
tok__star__4 { L.PosToken _ L.Tk__tok__star__4 }
tok__rparen__7 { L.PosToken _ L.Tk__tok__rparen__7 }
tok__lparen__6 { L.PosToken _ L.Tk__tok__lparen__6 }
tok__symbol__eql__50 { L.PosToken _ L.Tk__tok__symbol__eql__50 }
tok__symbol__symbol__58 { L.PosToken _ L.Tk__tok__symbol__symbol__58 }
tok__symbol__61 { L.PosToken _ L.Tk__tok__symbol__61 }
tok__symbol__eql__52 { L.PosToken _ L.Tk__tok__symbol__eql__52 }
tok__symbol__75 { L.PosToken _ L.Tk__tok__symbol__75 }
tok__exclamation__eql__63 { L.PosToken _ L.Tk__tok__exclamation__eql__63 }
tok__exclamation__79 { L.PosToken _ L.Tk__tok__exclamation__79 }
doccomment { L.PosToken _ (L.Tk__doccomment _) }
id { L.PosToken _ (L.Tk__id _) }
string { L.PosToken _ (L.Tk__string _) }
char { L.PosToken _ (L.Tk__char _) }
floatTypeSuffix { L.PosToken _ (L.Tk__floatTypeSuffix _) }
exponentPart { L.PosToken _ (L.Tk__exponentPart _) }
floatLiteral { L.PosToken _ (L.Tk__floatLiteral _) }
integerLiteral { L.PosToken _ (L.Tk__integerLiteral _) }
qq_CompoundName { L.PosToken _ (L.Tk__qq_CompoundName _) }
qq_Modifier { L.PosToken _ (L.Tk__qq_Modifier _) }
qq_TypeSpecifier { L.PosToken _ (L.Tk__qq_TypeSpecifier _) }
qq_Type { L.PosToken _ (L.Tk__qq_Type _) }
qq_TypeParameter { L.PosToken _ (L.Tk__qq_TypeParameter _) }
qq_TypeParameters { L.PosToken _ (L.Tk__qq_TypeParameters _) }
qq_WildcardType { L.PosToken _ (L.Tk__qq_WildcardType _) }
qq_TypeArgument { L.PosToken _ (L.Tk__qq_TypeArgument _) }
qq_NonEmptyTypeArguments { L.PosToken _ (L.Tk__qq_NonEmptyTypeArguments _) }
qq_TypeArguments { L.PosToken _ (L.Tk__qq_TypeArguments _) }
qq_Arglist { L.PosToken _ (L.Tk__qq_Arglist _) }
qq_Literal { L.PosToken _ (L.Tk__qq_Literal _) }
qq_DimExprs { L.PosToken _ (L.Tk__qq_DimExprs _) }
qq_CreationExpression { L.PosToken _ (L.Tk__qq_CreationExpression _) }
qq_PostfixOp { L.PosToken _ (L.Tk__qq_PostfixOp _) }
qq_PrefixOp { L.PosToken _ (L.Tk__qq_PrefixOp _) }
qq_MultiplicativeOp { L.PosToken _ (L.Tk__qq_MultiplicativeOp _) }
qq_AdditiveOp { L.PosToken _ (L.Tk__qq_AdditiveOp _) }
qq_ShiftOp { L.PosToken _ (L.Tk__qq_ShiftOp _) }
qq_RelationalOp { L.PosToken _ (L.Tk__qq_RelationalOp _) }
qq_EqualityOp { L.PosToken _ (L.Tk__qq_EqualityOp _) }
qq_AssignmentOp { L.PosToken _ (L.Tk__qq_AssignmentOp _) }
qq_Expression { L.PosToken _ (L.Tk__qq_Expression _) }
qq_SwitchStatement { L.PosToken _ (L.Tk__qq_SwitchStatement _) }
qq_SwitchCaseList { L.PosToken _ (L.Tk__qq_SwitchCaseList _) }
qq_TryStatement { L.PosToken _ (L.Tk__qq_TryStatement _) }
qq_OptFinally { L.PosToken _ (L.Tk__qq_OptFinally _) }
qq_CatchList { L.PosToken _ (L.Tk__qq_CatchList _) }
qq_ForStatement { L.PosToken _ (L.Tk__qq_ForStatement _) }
qq_WhileStatement { L.PosToken _ (L.Tk__qq_WhileStatement _) }
qq_DoStatement { L.PosToken _ (L.Tk__qq_DoStatement _) }
qq_IfStatement { L.PosToken _ (L.Tk__qq_IfStatement _) }
qq_OptElsePart { L.PosToken _ (L.Tk__qq_OptElsePart _) }
qq_Statement { L.PosToken _ (L.Tk__qq_Statement _) }
qq_OptId { L.PosToken _ (L.Tk__qq_OptId _) }
qq_OptExpression { L.PosToken _ (L.Tk__qq_OptExpression _) }
qq_StatementList { L.PosToken _ (L.Tk__qq_StatementList _) }
qq_Parameter { L.PosToken _ (L.Tk__qq_Parameter _) }
qq_ParameterList { L.PosToken _ (L.Tk__qq_ParameterList _) }
qq_StaticInitializer { L.PosToken _ (L.Tk__qq_StaticInitializer _) }
qq_VariableInitializer { L.PosToken _ (L.Tk__qq_VariableInitializer _) }
qq_VariableInitializerList { L.PosToken _ (L.Tk__qq_VariableInitializerList _) }
qq_VariableDeclarator { L.PosToken _ (L.Tk__qq_VariableDeclarator _) }
qq_OptVariableInitializer { L.PosToken _ (L.Tk__qq_OptVariableInitializer _) }
qq_VariableDeclaration { L.PosToken _ (L.Tk__qq_VariableDeclaration _) }
qq_VariableDeclaratorList { L.PosToken _ (L.Tk__qq_VariableDeclaratorList _) }
qq_StatementBlock { L.PosToken _ (L.Tk__qq_StatementBlock _) }
qq_MoreVariableDeclarators { L.PosToken _ (L.Tk__qq_MoreVariableDeclarators _) }
qq_MemberRest { L.PosToken _ (L.Tk__qq_MemberRest _) }
qq_MoreTypeSpecifier { L.PosToken _ (L.Tk__qq_MoreTypeSpecifier _) }
qq_MemberAfterFirstId { L.PosToken _ (L.Tk__qq_MemberAfterFirstId _) }
qq_PrimitiveTypeKeyword { L.PosToken _ (L.Tk__qq_PrimitiveTypeKeyword _) }
qq_MemberDeclaration { L.PosToken _ (L.Tk__qq_MemberDeclaration _) }
qq_NonEmptyDims { L.PosToken _ (L.Tk__qq_NonEmptyDims _) }
qq_Dims { L.PosToken _ (L.Tk__qq_Dims _) }
qq_FieldDeclaration { L.PosToken _ (L.Tk__qq_FieldDeclaration _) }
qq_TypeDeclRest { L.PosToken _ (L.Tk__qq_TypeDeclRest _) }
qq_EnumDeclaration { L.PosToken _ (L.Tk__qq_EnumDeclaration _) }
qq_EnumConstantList { L.PosToken _ (L.Tk__qq_EnumConstantList _) }
qq_EnumConstant { L.PosToken _ (L.Tk__qq_EnumConstant _) }
qq_AnnotationTypeElement { L.PosToken _ (L.Tk__qq_AnnotationTypeElement _) }
qq_AnnotationTypeElementList { L.PosToken _ (L.Tk__qq_AnnotationTypeElementList _) }
qq_AnnotationDeclaration { L.PosToken _ (L.Tk__qq_AnnotationDeclaration _) }
qq_InterfaceDeclaration { L.PosToken _ (L.Tk__qq_InterfaceDeclaration _) }
qq_ClassDeclaration { L.PosToken _ (L.Tk__qq_ClassDeclaration _) }
qq_FieldDeclarationList { L.PosToken _ (L.Tk__qq_FieldDeclarationList _) }
qq_ImplementsList { L.PosToken _ (L.Tk__qq_ImplementsList _) }
qq_ExtendsList { L.PosToken _ (L.Tk__qq_ExtendsList _) }
qq_ModifierList { L.PosToken _ (L.Tk__qq_ModifierList _) }
qq_AnnotationList { L.PosToken _ (L.Tk__qq_AnnotationList _) }
qq_AnnotationElement { L.PosToken _ (L.Tk__qq_AnnotationElement _) }
qq_AnnotationArguments { L.PosToken _ (L.Tk__qq_AnnotationArguments _) }
qq_Annotation { L.PosToken _ (L.Tk__qq_Annotation _) }
qq_DocComment { L.PosToken _ (L.Tk__qq_DocComment _) }
qq_ImportStatement { L.PosToken _ (L.Tk__qq_ImportStatement _) }
qq_Package { L.PosToken _ (L.Tk__qq_Package _) }
qq_CompilationUnit { L.PosToken _ (L.Tk__qq_CompilationUnit _) }
qq_ImportList { L.PosToken _ (L.Tk__qq_ImportList _) }
qq_TypeDeclaration { L.PosToken _ (L.Tk__qq_TypeDeclaration _) }
qq_OptDocComment { L.PosToken _ (L.Tk__qq_OptDocComment _) }
qq_Java { L.PosToken _ (L.Tk__qq_Java _) }

%%

Java__top : Java rtk__eof { $1 }

Java : tok_Java_dummy_163 Java tok_Java_dummy_163 { Ctr__Java__0 (rtkPosOf $1) $2 } |
       tok_AdditiveOp_dummy_162 AdditiveOp tok_AdditiveOp_dummy_162 { Ctr__Java__1 (rtkPosOf $1) $2 } |
       tok_Annotation_dummy_161 Annotation tok_Annotation_dummy_161 { Ctr__Java__2 (rtkPosOf $1) $2 } |
       tok_AnnotationArguments_dummy_160 AnnotationArguments tok_AnnotationArguments_dummy_160 { Ctr__Java__3 (rtkPosOf $1) $2 } |
       tok_AnnotationDeclaration_dummy_159 AnnotationDeclaration tok_AnnotationDeclaration_dummy_159 { Ctr__Java__4 (rtkPosOf $1) $2 } |
       tok_AnnotationElement_dummy_158 AnnotationElement tok_AnnotationElement_dummy_158 { Ctr__Java__5 (rtkPosOf $1) $2 } |
       tok_AnnotationList_dummy_157 AnnotationList tok_AnnotationList_dummy_157 { Ctr__Java__6 (rtkPosOf $1) (reverse $2) } |
       tok_AnnotationTypeElement_dummy_156 AnnotationTypeElement tok_AnnotationTypeElement_dummy_156 { Ctr__Java__7 (rtkPosOf $1) $2 } |
       tok_AnnotationTypeElementList_dummy_155 AnnotationTypeElementList tok_AnnotationTypeElementList_dummy_155 { Ctr__Java__8 (rtkPosOf $1) (reverse $2) } |
       tok_Arglist_dummy_154 Arglist tok_Arglist_dummy_154 { Ctr__Java__9 (rtkPosOf $1) $2 } |
       tok_AssignmentOp_dummy_153 AssignmentOp tok_AssignmentOp_dummy_153 { Ctr__Java__10 (rtkPosOf $1) $2 } |
       tok_CatchList_dummy_152 CatchList tok_CatchList_dummy_152 { Ctr__Java__11 (rtkPosOf $1) (reverse $2) } |
       tok_ClassDeclaration_dummy_151 ClassDeclaration tok_ClassDeclaration_dummy_151 { Ctr__Java__12 (rtkPosOf $1) $2 } |
       tok_CompilationUnit_dummy_150 CompilationUnit tok_CompilationUnit_dummy_150 { Ctr__Java__13 (rtkPosOf $1) $2 } |
       tok_CompoundName_dummy_149 CompoundName tok_CompoundName_dummy_149 { Ctr__Java__14 (rtkPosOf $1) $2 } |
       tok_CreationExpression_dummy_148 CreationExpression tok_CreationExpression_dummy_148 { Ctr__Java__15 (rtkPosOf $1) $2 } |
       tok_DimExprs_dummy_147 DimExprs tok_DimExprs_dummy_147 { Ctr__Java__16 (rtkPosOf $1) (reverse $2) } |
       tok_Dims_dummy_146 Dims tok_Dims_dummy_146 { Ctr__Java__17 (rtkPosOf $1) (reverse $2) } |
       tok_DoStatement_dummy_145 DoStatement tok_DoStatement_dummy_145 { Ctr__Java__18 (rtkPosOf $1) $2 } |
       tok_DocComment_dummy_144 DocComment tok_DocComment_dummy_144 { Ctr__Java__19 (rtkPosOf $1) $2 } |
       tok_EnumConstant_dummy_143 EnumConstant tok_EnumConstant_dummy_143 { Ctr__Java__20 (rtkPosOf $1) $2 } |
       tok_EnumConstantList_dummy_142 EnumConstantList tok_EnumConstantList_dummy_142 { Ctr__Java__21 (rtkPosOf $1) $2 } |
       tok_EnumDeclaration_dummy_141 EnumDeclaration tok_EnumDeclaration_dummy_141 { Ctr__Java__22 (rtkPosOf $1) $2 } |
       tok_EqualityOp_dummy_140 EqualityOp tok_EqualityOp_dummy_140 { Ctr__Java__23 (rtkPosOf $1) $2 } |
       tok_Expression_dummy_139 Expression tok_Expression_dummy_139 { Ctr__Java__24 (rtkPosOf $1) $2 } |
       tok_ExtendsList_dummy_138 ExtendsList tok_ExtendsList_dummy_138 { Ctr__Java__25 (rtkPosOf $1) $2 } |
       tok_FieldDeclaration_dummy_137 FieldDeclaration tok_FieldDeclaration_dummy_137 { Ctr__Java__26 (rtkPosOf $1) $2 } |
       tok_FieldDeclarationList_dummy_136 FieldDeclarationList tok_FieldDeclarationList_dummy_136 { Ctr__Java__27 (rtkPosOf $1) (reverse $2) } |
       tok_ForStatement_dummy_135 ForStatement tok_ForStatement_dummy_135 { Ctr__Java__28 (rtkPosOf $1) $2 } |
       tok_IfStatement_dummy_134 IfStatement tok_IfStatement_dummy_134 { Ctr__Java__29 (rtkPosOf $1) $2 } |
       tok_ImplementsList_dummy_133 ImplementsList tok_ImplementsList_dummy_133 { Ctr__Java__30 (rtkPosOf $1) $2 } |
       tok_ImportList_dummy_132 ImportList tok_ImportList_dummy_132 { Ctr__Java__31 (rtkPosOf $1) (reverse $2) } |
       tok_ImportStatement_dummy_131 ImportStatement tok_ImportStatement_dummy_131 { Ctr__Java__32 (rtkPosOf $1) $2 } |
       tok_InterfaceDeclaration_dummy_130 InterfaceDeclaration tok_InterfaceDeclaration_dummy_130 { Ctr__Java__33 (rtkPosOf $1) $2 } |
       tok_Literal_dummy_129 Literal tok_Literal_dummy_129 { Ctr__Java__34 (rtkPosOf $1) $2 } |
       tok_MemberAfterFirstId_dummy_128 MemberAfterFirstId tok_MemberAfterFirstId_dummy_128 { Ctr__Java__35 (rtkPosOf $1) $2 } |
       tok_MemberDeclaration_dummy_127 MemberDeclaration tok_MemberDeclaration_dummy_127 { Ctr__Java__36 (rtkPosOf $1) $2 } |
       tok_MemberRest_dummy_126 MemberRest tok_MemberRest_dummy_126 { Ctr__Java__37 (rtkPosOf $1) $2 } |
       tok_Modifier_dummy_125 Modifier tok_Modifier_dummy_125 { Ctr__Java__38 (rtkPosOf $1) $2 } |
       tok_ModifierList_dummy_124 ModifierList tok_ModifierList_dummy_124 { Ctr__Java__39 (rtkPosOf $1) (reverse $2) } |
       tok_MoreTypeSpecifier_dummy_123 MoreTypeSpecifier tok_MoreTypeSpecifier_dummy_123 { Ctr__Java__40 (rtkPosOf $1) $2 } |
       tok_MoreVariableDeclarators_dummy_122 MoreVariableDeclarators tok_MoreVariableDeclarators_dummy_122 { Ctr__Java__41 (rtkPosOf $1) (reverse $2) } |
       tok_MultiplicativeOp_dummy_121 MultiplicativeOp tok_MultiplicativeOp_dummy_121 { Ctr__Java__42 (rtkPosOf $1) $2 } |
       tok_NonEmptyDims_dummy_120 NonEmptyDims tok_NonEmptyDims_dummy_120 { Ctr__Java__43 (rtkPosOf $1) (reverse $2) } |
       tok_NonEmptyTypeArguments_dummy_119 NonEmptyTypeArguments tok_NonEmptyTypeArguments_dummy_119 { Ctr__Java__44 (rtkPosOf $1) $2 } |
       tok_OptDocComment_dummy_118 OptDocComment tok_OptDocComment_dummy_118 { Ctr__Java__45 (rtkPosOf $1) $2 } |
       tok_OptElsePart_dummy_117 OptElsePart tok_OptElsePart_dummy_117 { Ctr__Java__46 (rtkPosOf $1) $2 } |
       tok_OptExpression_dummy_116 OptExpression tok_OptExpression_dummy_116 { Ctr__Java__47 (rtkPosOf $1) $2 } |
       tok_OptFinally_dummy_115 OptFinally tok_OptFinally_dummy_115 { Ctr__Java__48 (rtkPosOf $1) $2 } |
       tok_OptId_dummy_114 OptId tok_OptId_dummy_114 { Ctr__Java__49 (rtkPosOf $1) $2 } |
       tok_OptVariableInitializer_dummy_113 OptVariableInitializer tok_OptVariableInitializer_dummy_113 { Ctr__Java__50 (rtkPosOf $1) $2 } |
       tok_Package_dummy_112 Package tok_Package_dummy_112 { Ctr__Java__51 (rtkPosOf $1) $2 } |
       tok_Parameter_dummy_111 Parameter tok_Parameter_dummy_111 { Ctr__Java__52 (rtkPosOf $1) $2 } |
       tok_ParameterList_dummy_110 ParameterList tok_ParameterList_dummy_110 { Ctr__Java__53 (rtkPosOf $1) $2 } |
       tok_PostfixOp_dummy_109 PostfixOp tok_PostfixOp_dummy_109 { Ctr__Java__54 (rtkPosOf $1) $2 } |
       tok_PrefixOp_dummy_108 PrefixOp tok_PrefixOp_dummy_108 { Ctr__Java__55 (rtkPosOf $1) $2 } |
       tok_PrimitiveTypeKeyword_dummy_107 PrimitiveTypeKeyword tok_PrimitiveTypeKeyword_dummy_107 { Ctr__Java__56 (rtkPosOf $1) $2 } |
       tok_RelationalOp_dummy_106 RelationalOp tok_RelationalOp_dummy_106 { Ctr__Java__57 (rtkPosOf $1) $2 } |
       tok_ShiftOp_dummy_105 ShiftOp tok_ShiftOp_dummy_105 { Ctr__Java__58 (rtkPosOf $1) $2 } |
       tok_Statement_dummy_104 Statement tok_Statement_dummy_104 { Ctr__Java__59 (rtkPosOf $1) $2 } |
       tok_StatementBlock_dummy_103 StatementBlock tok_StatementBlock_dummy_103 { Ctr__Java__60 (rtkPosOf $1) $2 } |
       tok_StatementList_dummy_102 StatementList tok_StatementList_dummy_102 { Ctr__Java__61 (rtkPosOf $1) (reverse $2) } |
       tok_StaticInitializer_dummy_101 StaticInitializer tok_StaticInitializer_dummy_101 { Ctr__Java__62 (rtkPosOf $1) $2 } |
       tok_SwitchCaseList_dummy_100 SwitchCaseList tok_SwitchCaseList_dummy_100 { Ctr__Java__63 (rtkPosOf $1) (reverse $2) } |
       tok_SwitchStatement_dummy_99 SwitchStatement tok_SwitchStatement_dummy_99 { Ctr__Java__64 (rtkPosOf $1) $2 } |
       tok_TryStatement_dummy_98 TryStatement tok_TryStatement_dummy_98 { Ctr__Java__65 (rtkPosOf $1) $2 } |
       tok_Type_dummy_97 Type tok_Type_dummy_97 { Ctr__Java__66 (rtkPosOf $1) $2 } |
       tok_TypeArgument_dummy_96 TypeArgument tok_TypeArgument_dummy_96 { Ctr__Java__67 (rtkPosOf $1) $2 } |
       tok_TypeArguments_dummy_95 TypeArguments tok_TypeArguments_dummy_95 { Ctr__Java__68 (rtkPosOf $1) $2 } |
       tok_TypeDeclRest_dummy_94 TypeDeclRest tok_TypeDeclRest_dummy_94 { Ctr__Java__69 (rtkPosOf $1) $2 } |
       tok_TypeDeclaration_dummy_93 TypeDeclaration tok_TypeDeclaration_dummy_93 { Ctr__Java__70 (rtkPosOf $1) $2 } |
       tok_TypeParameter_dummy_92 TypeParameter tok_TypeParameter_dummy_92 { Ctr__Java__71 (rtkPosOf $1) $2 } |
       tok_TypeParameters_dummy_91 TypeParameters tok_TypeParameters_dummy_91 { Ctr__Java__72 (rtkPosOf $1) $2 } |
       tok_TypeSpecifier_dummy_90 TypeSpecifier tok_TypeSpecifier_dummy_90 { Ctr__Java__73 (rtkPosOf $1) $2 } |
       tok_VariableDeclaration_dummy_89 VariableDeclaration tok_VariableDeclaration_dummy_89 { Ctr__Java__74 (rtkPosOf $1) $2 } |
       tok_VariableDeclarator_dummy_88 VariableDeclarator tok_VariableDeclarator_dummy_88 { Ctr__Java__75 (rtkPosOf $1) $2 } |
       tok_VariableDeclaratorList_dummy_87 VariableDeclaratorList tok_VariableDeclaratorList_dummy_87 { Ctr__Java__76 (rtkPosOf $1) $2 } |
       tok_VariableInitializer_dummy_86 VariableInitializer tok_VariableInitializer_dummy_86 { Ctr__Java__77 (rtkPosOf $1) $2 } |
       tok_VariableInitializerList_dummy_85 VariableInitializerList tok_VariableInitializerList_dummy_85 { Ctr__Java__78 (rtkPosOf $1) $2 } |
       tok_WhileStatement_dummy_84 WhileStatement tok_WhileStatement_dummy_84 { Ctr__Java__79 (rtkPosOf $1) $2 } |
       tok_WildcardType_dummy_83 WildcardType tok_WildcardType_dummy_83 { Ctr__Java__80 (rtkPosOf $1) $2 }

Java : qq_Java { Anti_Java (tkVal_qq_Java $1) } |
       CompilationUnit { Ctr__Java__81 (rtkPosOf $1) $1 }

AdditiveOp : qq_AdditiveOp { Anti_AdditiveOp (tkVal_qq_AdditiveOp $1) } |
             tok__plus__72 { Ctr__AdditiveOp__0 (rtkPosOf $1) } |
             tok__minus__73 { Ctr__AdditiveOp__1 (rtkPosOf $1) }

ListElem_AnnotationList9 : qq_AnnotationList { Anti_Annotation (tkVal_qq_AnnotationList $1) } |
                           Annotation { $1 }

Annotation : qq_Annotation { Anti_Annotation (tkVal_qq_Annotation $1) } |
             tok__symbol__5 CompoundName Rule_4 { Ctr__Annotation__1 (rtkPosOf $1) $2 $3 }

AnnotationArguments : qq_AnnotationArguments { Anti_AnnotationArguments (tkVal_qq_AnnotationArguments $1) } |
                      AnnotationElement Rule_7 { Ctr__AnnotationArguments__0 (rtkPosOf $1) $1 (reverse $2) }

AnnotationDeclaration : qq_AnnotationDeclaration { Anti_AnnotationDeclaration (tkVal_qq_AnnotationDeclaration $1) } |
                        tok__symbol__5 tok_interface_15 id tok__symbol__13 AnnotationTypeElementList tok__symbol__14 { Ctr__AnnotationDeclaration__0 (rtkPosOf $1) (tkVal_id $3) (reverse $5) }

AnnotationElement : qq_AnnotationElement { Anti_AnnotationElement (tkVal_qq_AnnotationElement $1) } |
                    id tok__eql__9 ConditionalExpression { Ctr__AnnotationElement__0 (rtkPosOf $1) (tkVal_id $1) $3 } |
                    ConditionalExpression { Ctr__AnnotationElement__1 (rtkPosOf $1) $1 }

AnnotationList : {- empty -} { [] } |
                 AnnotationList ListElem_AnnotationList9 { $2 : $1 }

AnnotationTypeElement : qq_AnnotationTypeElement { Anti_AnnotationTypeElement (tkVal_qq_AnnotationTypeElement $1) } |
                        FieldDeclaration { Ctr__AnnotationTypeElement__0 (rtkPosOf $1) $1 }

ListElem_AnnotationTypeElementList19 : qq_AnnotationTypeElementList { Anti_AnnotationTypeElement (tkVal_qq_AnnotationTypeElementList $1) } |
                                       AnnotationTypeElement { $1 }

AnnotationTypeElementList : {- empty -} { [] } |
                            AnnotationTypeElementList ListElem_AnnotationTypeElementList19 { $2 : $1 }

Arglist : qq_Arglist { Anti_Arglist (tkVal_qq_Arglist $1) } |
          { Ctr__Arglist__0 rtkNoPos } |
          Rule_69 { Ctr__Arglist__1 (rtkPosOf $1) $1 }

AssignmentOp : qq_AssignmentOp { Anti_AssignmentOp (tkVal_qq_AssignmentOp $1) } |
               tok__eql__9 { Ctr__AssignmentOp__0 (rtkPosOf $1) } |
               tok__plus__eql__45 { Ctr__AssignmentOp__1 (rtkPosOf $1) } |
               tok__minus__eql__46 { Ctr__AssignmentOp__2 (rtkPosOf $1) } |
               tok__star__eql__47 { Ctr__AssignmentOp__3 (rtkPosOf $1) } |
               tok__symbol__eql__48 { Ctr__AssignmentOp__4 (rtkPosOf $1) } |
               tok__pipe__eql__49 { Ctr__AssignmentOp__5 (rtkPosOf $1) } |
               tok__symbol__eql__50 { Ctr__AssignmentOp__6 (rtkPosOf $1) } |
               tok__symbol__eql__51 { Ctr__AssignmentOp__7 (rtkPosOf $1) } |
               tok__symbol__eql__52 { Ctr__AssignmentOp__8 (rtkPosOf $1) } |
               tok__symbol__symbol__eql__53 { Ctr__AssignmentOp__9 (rtkPosOf $1) } |
               tok__symbol__symbol__eql__54 { Ctr__AssignmentOp__10 (rtkPosOf $1) } |
               tok__symbol__symbol__symbol__eql__55 { Ctr__AssignmentOp__11 (rtkPosOf $1) }

CatchList : {- empty -} { [] } |
            CatchList ListElem_CatchList55 { $2 : $1 }

ClassDeclaration : qq_ClassDeclaration { Anti_ClassDeclaration (tkVal_qq_ClassDeclaration $1) } |
                   tok_class_12 id TypeParameters Rule_16 Rule_17 tok__symbol__13 FieldDeclarationList tok__symbol__14 { Ctr__ClassDeclaration__0 (rtkPosOf $1) (tkVal_id $2) $3 $4 $5 (reverse $7) }

CompilationUnit : qq_CompilationUnit { Anti_CompilationUnit (tkVal_qq_CompilationUnit $1) } |
                  Rule_1 ImportList Rule_2 { Ctr__CompilationUnit__0 (rtkPosOf $1) $1 (reverse $2) $3 }

CompoundName : qq_CompoundName { Anti_CompoundName (tkVal_qq_CompoundName $1) } |
               id Rule_81 { Ctr__CompoundName__0 (rtkPosOf $1) (tkVal_id $1) (reverse $2) }

CreationExpression : qq_CreationExpression { Anti_CreationExpression (tkVal_qq_CreationExpression $1) } |
                     tok_new_82 TypeSpecifier Rule_65 { Ctr__CreationExpression__0 (rtkPosOf $1) $2 $3 }

DimExprs : ListElem_DimExprs68 { [$1] } |
           DimExprs ListElem_DimExprs68 { $2 : $1 }

Dims : {- empty -} { [] } |
       Dims ListElem_Dims32 { $2 : $1 }

DoStatement : qq_DoStatement { Anti_DoStatement (tkVal_qq_DoStatement $1) } |
              tok_do_37 Statement tok_while_38 tok__lparen__6 Expression tok__rparen__7 tok__semi__1 { Ctr__DoStatement__0 (rtkPosOf $1) $2 $5 }

DocComment : qq_DocComment { Anti_DocComment (tkVal_qq_DocComment $1) } |
             doccomment { Ctr__DocComment__0 (rtkPosOf $1) (tkVal_doccomment $1) }

EnumConstant : qq_EnumConstant { Anti_EnumConstant (tkVal_qq_EnumConstant $1) } |
               AnnotationList id Rule_20 Rule_22 { Ctr__EnumConstant__0 (rtkPosOf (reverse $1)) (reverse $1) (tkVal_id $2) $3 $4 }

EnumConstantList : qq_EnumConstantList { Anti_EnumConstantList (tkVal_qq_EnumConstantList $1) } |
                   EnumConstant Rule_24 Rule_26 { Ctr__EnumConstantList__0 (rtkPosOf $1) $1 (reverse $2) $3 }

EnumDeclaration : qq_EnumDeclaration { Anti_EnumDeclaration (tkVal_qq_EnumDeclaration $1) } |
                  tok_enum_16 id Rule_27 tok__symbol__13 EnumConstantList Rule_28 tok__symbol__14 { Ctr__EnumDeclaration__0 (rtkPosOf $1) (tkVal_id $2) $3 $5 $6 }

EqualityOp : qq_EqualityOp { Anti_EqualityOp (tkVal_qq_EqualityOp $1) } |
             tok__eql__eql__62 { Ctr__EqualityOp__0 (rtkPosOf $1) } |
             tok__exclamation__eql__63 { Ctr__EqualityOp__1 (rtkPosOf $1) }

PrimaryNoPostfix : qq_Expression { Anti_Expression (tkVal_qq_Expression $1) } |
                   Literal { Ctr__Expression__0 (rtkPosOf $1) $1 } |
                   tok_this_80 { Ctr__Expression__1 (rtkPosOf $1) } |
                   tok__lparen__6 Expression tok__rparen__7 { Ctr__Expression__2 (rtkPosOf $1) $2 } |
                   CreationExpression { Ctr__Expression__3 (rtkPosOf $1) $1 } |
                   CompoundName Rule_61 { Ctr__Expression__4 (rtkPosOf $1) $1 $2 } |
                   CompoundName tok__sq_bkt_l__17 Expression tok__sq_bkt_r__18 { Ctr__Expression__5 (rtkPosOf $1) $1 $3 } |
                   tok_super_81 tok__dot__3 id Rule_63 { Ctr__Expression__6 (rtkPosOf $1) (tkVal_id $3) $4 }

PostfixExpression : PrimaryNoPostfix { Ctr__Expression__7 (rtkPosOf $1) $1 } |
                    PostfixExpression PostfixOp { Ctr__Expression__8 (rtkPosOf $1) $1 $2 } |
                    PostfixExpression tok__dot__3 id { Ctr__Expression__9 (rtkPosOf $1) $1 (tkVal_id $3) } |
                    PostfixExpression tok__dot__3 id tok__lparen__6 Arglist tok__rparen__7 { Ctr__Expression__10 (rtkPosOf $1) $1 (tkVal_id $3) $5 } |
                    PostfixExpression tok__sq_bkt_l__17 Expression tok__sq_bkt_r__18 { Ctr__Expression__11 (rtkPosOf $1) $1 $3 }

UnaryExpressionNotPlusMinus : PostfixExpression { Ctr__Expression__12 (rtkPosOf $1) $1 } |
                              tok__tilde__78 UnaryExpression { Ctr__Expression__13 (rtkPosOf $1) $2 } |
                              tok__exclamation__79 UnaryExpression { Ctr__Expression__14 (rtkPosOf $1) $2 } |
                              CastExpression { Ctr__Expression__15 (rtkPosOf $1) $1 }

UnaryExpression : PrefixOp UnaryExpression { Ctr__Expression__16 (rtkPosOf $1) $1 $2 } |
                  UnaryExpressionNotPlusMinus { Ctr__Expression__17 (rtkPosOf $1) $1 }

CastExpression : tok__lparen__6 PrimitiveTypeKeyword Dims tok__rparen__7 UnaryExpression { Ctr__Expression__18 (rtkPosOf $1) $2 (reverse $3) $5 } |
                 tok__lparen__6 CompoundName NonEmptyTypeArguments Dims tok__rparen__7 UnaryExpressionNotPlusMinus { Ctr__Expression__19 (rtkPosOf $1) $2 $3 (reverse $4) $6 } |
                 tok__lparen__6 CompoundName NonEmptyDims tok__rparen__7 UnaryExpressionNotPlusMinus { Ctr__Expression__20 (rtkPosOf $1) $2 (reverse $3) $5 } |
                 tok__lparen__6 Expression tok__rparen__7 UnaryExpressionNotPlusMinus { Ctr__Expression__21 (rtkPosOf $1) $2 $4 }

MultiplicativeExpression : UnaryExpression { Ctr__Expression__22 (rtkPosOf $1) $1 } |
                           MultiplicativeExpression MultiplicativeOp UnaryExpression { Ctr__Expression__23 (rtkPosOf $1) $1 $2 $3 }

AdditiveExpression : MultiplicativeExpression { Ctr__Expression__24 (rtkPosOf $1) $1 } |
                     AdditiveExpression AdditiveOp MultiplicativeExpression { Ctr__Expression__25 (rtkPosOf $1) $1 $2 $3 }

ShiftExpression : AdditiveExpression { Ctr__Expression__26 (rtkPosOf $1) $1 } |
                  ShiftExpression ShiftOp AdditiveExpression { Ctr__Expression__27 (rtkPosOf $1) $1 $2 $3 }

RelationalExpression : ShiftExpression { Ctr__Expression__28 (rtkPosOf $1) $1 } |
                       RelationalExpression RelationalOp ShiftExpression { Ctr__Expression__29 (rtkPosOf $1) $1 $2 $3 } |
                       RelationalExpression tok_instanceof_68 Type { Ctr__Expression__30 (rtkPosOf $1) $1 $3 }

EqualityExpression : RelationalExpression { Ctr__Expression__31 (rtkPosOf $1) $1 } |
                     EqualityExpression EqualityOp RelationalExpression { Ctr__Expression__32 (rtkPosOf $1) $1 $2 $3 }

AndExpression : EqualityExpression { Ctr__Expression__33 (rtkPosOf $1) $1 } |
                AndExpression tok__symbol__61 EqualityExpression { Ctr__Expression__34 (rtkPosOf $1) $1 $3 }

ExclusiveOrExpression : AndExpression { Ctr__Expression__35 (rtkPosOf $1) $1 } |
                        ExclusiveOrExpression tok__symbol__60 AndExpression { Ctr__Expression__36 (rtkPosOf $1) $1 $3 }

InclusiveOrEpression : ExclusiveOrExpression { Ctr__Expression__37 (rtkPosOf $1) $1 } |
                       InclusiveOrEpression tok__pipe__59 ExclusiveOrExpression { Ctr__Expression__38 (rtkPosOf $1) $1 $3 }

ConditionalAndExpression : InclusiveOrEpression { Ctr__Expression__39 (rtkPosOf $1) $1 } |
                           ConditionalAndExpression tok__symbol__symbol__58 InclusiveOrEpression { Ctr__Expression__40 (rtkPosOf $1) $1 $3 }

ConditionalOrExpression : ConditionalAndExpression { Ctr__Expression__41 (rtkPosOf $1) $1 } |
                          ConditionalOrExpression tok__pipe__pipe__57 ConditionalAndExpression { Ctr__Expression__42 (rtkPosOf $1) $1 $3 }

ConditionalExpression : ConditionalOrExpression { Ctr__Expression__43 (rtkPosOf $1) $1 } |
                        ConditionalOrExpression tok__symbol__56 Expression tok__colon__32 ConditionalExpression { Ctr__Expression__44 (rtkPosOf $1) $1 $3 $5 }

AssignmentExpression : ConditionalExpression Rule_59 { Ctr__Expression__45 (rtkPosOf $1) $1 $2 }

Expression : AssignmentExpression { Ctr__Expression__46 (rtkPosOf $1) $1 }

ExtendsList : qq_ExtendsList { Anti_ExtendsList (tkVal_qq_ExtendsList $1) } |
              tok_extends_10 CompoundName Rule_12 { Ctr__ExtendsList__0 (rtkPosOf $1) $2 (reverse $3) }

FieldDeclaration : qq_FieldDeclaration { Anti_FieldDeclaration (tkVal_qq_FieldDeclaration $1) } |
                   OptDocComment ModifierList Rule_30 { Ctr__FieldDeclaration__0 (rtkPosOf $1) $1 (reverse $2) $3 } |
                   tok__semi__1 { Ctr__FieldDeclaration__1 (rtkPosOf $1) }

ListElem_FieldDeclarationList15 : qq_FieldDeclarationList { Anti_FieldDeclaration (tkVal_qq_FieldDeclarationList $1) } |
                                  FieldDeclaration { $1 }

FieldDeclarationList : {- empty -} { [] } |
                       FieldDeclarationList ListElem_FieldDeclarationList15 { $2 : $1 }

ForStatement : qq_ForStatement { Anti_ForStatement (tkVal_qq_ForStatement $1) } |
               tok_for_39 tok__lparen__6 Rule_53 OptExpression tok__semi__1 OptExpression tok__rparen__7 Statement { Ctr__ForStatement__0 (rtkPosOf $1) $3 $4 $6 $8 }

IfStatement : qq_IfStatement { Anti_IfStatement (tkVal_qq_IfStatement $1) } |
              tok_if_36 tok__lparen__6 Expression tok__rparen__7 Statement OptElsePart { Ctr__IfStatement__0 (rtkPosOf $1) $3 $5 $6 }

ImplementsList : qq_ImplementsList { Anti_ImplementsList (tkVal_qq_ImplementsList $1) } |
                 tok_implements_11 Rule_14 { Ctr__ImplementsList__0 (rtkPosOf $1) (reverse $2) }

ImportList : {- empty -} { [] } |
             ImportList ListElem_ImportList0 { $2 : $1 }

ImportStatement : qq_ImportStatement { Anti_ImportStatement (tkVal_qq_ImportStatement $1) } |
                  tok_import_2 Rule_3 tok__semi__1 { Ctr__ImportStatement__0 (rtkPosOf $1) $2 }

ListElem_ImportList0 : qq_ImportList { Anti_ImportStatement (tkVal_qq_ImportList $1) } |
                       ImportStatement { $1 }

InterfaceDeclaration : qq_InterfaceDeclaration { Anti_InterfaceDeclaration (tkVal_qq_InterfaceDeclaration $1) } |
                       tok_interface_15 id TypeParameters Rule_18 tok__symbol__13 FieldDeclarationList tok__symbol__14 { Ctr__InterfaceDeclaration__0 (rtkPosOf $1) (tkVal_id $2) $3 $4 (reverse $6) }

Literal : qq_Literal { Anti_Literal (tkVal_qq_Literal $1) } |
          integerLiteral { Ctr__Literal__0 (rtkPosOf $1) (tkVal_integerLiteral $1) } |
          floatLiteral { Ctr__Literal__1 (rtkPosOf $1) (tkVal_floatLiteral $1) } |
          tok_true_83 { Ctr__Literal__2 (rtkPosOf $1) } |
          tok_false_84 { Ctr__Literal__3 (rtkPosOf $1) } |
          char { Ctr__Literal__4 (rtkPosOf $1) (tkVal_char $1) } |
          string { Ctr__Literal__5 (rtkPosOf $1) (tkVal_string $1) } |
          tok_null_85 { Ctr__Literal__6 (rtkPosOf $1) }

MemberAfterFirstId : qq_MemberAfterFirstId { Anti_MemberAfterFirstId (tkVal_qq_MemberAfterFirstId $1) } |
                     tok__lparen__6 Rule_35 tok__rparen__7 StatementBlock { Ctr__MemberAfterFirstId__0 (rtkPosOf $1) $2 $4 } |
                     MoreTypeSpecifier id MemberRest { Ctr__MemberAfterFirstId__1 (rtkPosOf $1) $1 (tkVal_id $2) $3 }

MemberDeclaration : qq_MemberDeclaration { Anti_MemberDeclaration (tkVal_qq_MemberDeclaration $1) } |
                    PrimitiveTypeKeyword Dims id MemberRest { Ctr__MemberDeclaration__0 (rtkPosOf $1) $1 (reverse $2) (tkVal_id $3) $4 } |
                    TypeParameters id MoreTypeSpecifier id MemberRest { Ctr__MemberDeclaration__1 (rtkPosOf $1) $1 (tkVal_id $2) $3 (tkVal_id $4) $5 } |
                    id MemberAfterFirstId { Ctr__MemberDeclaration__2 (rtkPosOf $1) (tkVal_id $1) $2 }

MemberRest : qq_MemberRest { Anti_MemberRest (tkVal_qq_MemberRest $1) } |
             tok__lparen__6 Rule_36 tok__rparen__7 Dims Rule_37 { Ctr__MemberRest__0 (rtkPosOf $1) $2 (reverse $4) $5 } |
             Dims OptVariableInitializer MoreVariableDeclarators tok__semi__1 { Ctr__MemberRest__1 (rtkPosOf (reverse $1)) (reverse $1) $2 (reverse $3) }

Modifier : qq_Modifier { Anti_Modifier (tkVal_qq_Modifier $1) } |
           tok_public_86 { Ctr__Modifier__0 (rtkPosOf $1) } |
           tok_private_87 { Ctr__Modifier__1 (rtkPosOf $1) } |
           tok_protected_88 { Ctr__Modifier__2 (rtkPosOf $1) } |
           tok_static_89 { Ctr__Modifier__3 (rtkPosOf $1) } |
           tok_final_90 { Ctr__Modifier__4 (rtkPosOf $1) } |
           tok_native_91 { Ctr__Modifier__5 (rtkPosOf $1) } |
           tok_synchronized_30 { Ctr__Modifier__6 (rtkPosOf $1) } |
           tok_abstract_92 { Ctr__Modifier__7 (rtkPosOf $1) } |
           tok_threadsafe_93 { Ctr__Modifier__8 (rtkPosOf $1) } |
           tok_transient_94 { Ctr__Modifier__9 (rtkPosOf $1) }

ModifierList : {- empty -} { [] } |
               ModifierList ListElem_ModifierList11 { $2 : $1 }

MoreTypeSpecifier : qq_MoreTypeSpecifier { Anti_MoreTypeSpecifier (tkVal_qq_MoreTypeSpecifier $1) } |
                    tok__dot__3 id MoreTypeSpecifier { Ctr__MoreTypeSpecifier__0 (rtkPosOf $1) (tkVal_id $2) $3 } |
                    TypeArguments Dims { Ctr__MoreTypeSpecifier__1 (rtkPosOf $1) $1 (reverse $2) }

MoreVariableDeclarators : {- empty -} { [] } |
                          MoreVariableDeclarators ListElem_MoreVariableDeclarators41 { $2 : $1 }

MultiplicativeOp : qq_MultiplicativeOp { Anti_MultiplicativeOp (tkVal_qq_MultiplicativeOp $1) } |
                   tok__star__4 { Ctr__MultiplicativeOp__0 (rtkPosOf $1) } |
                   tok__symbol__74 { Ctr__MultiplicativeOp__1 (rtkPosOf $1) } |
                   tok__symbol__75 { Ctr__MultiplicativeOp__2 (rtkPosOf $1) }

NonEmptyDims : ListElem_NonEmptyDims34 { [$1] } |
               NonEmptyDims ListElem_NonEmptyDims34 { $2 : $1 }

NonEmptyTypeArguments : qq_NonEmptyTypeArguments { Anti_NonEmptyTypeArguments (tkVal_qq_NonEmptyTypeArguments $1) } |
                        tok__symbol__64 TypeArgument Rule_72 tok__symbol__65 { Ctr__NonEmptyTypeArguments__0 (rtkPosOf $1) $2 (reverse $3) }

OptDocComment : qq_OptDocComment { Anti_OptDocComment (tkVal_qq_OptDocComment $1) } |
                { Ctr__OptDocComment__0 rtkNoPos } |
                DocComment { Ctr__OptDocComment__1 (rtkPosOf $1) $1 }

OptElsePart : qq_OptElsePart { Anti_OptElsePart (tkVal_qq_OptElsePart $1) } |
              { Ctr__OptElsePart__0 rtkNoPos } |
              Rule_52 { Ctr__OptElsePart__1 (rtkPosOf $1) $1 }

OptExpression : qq_OptExpression { Anti_OptExpression (tkVal_qq_OptExpression $1) } |
                { Ctr__OptExpression__0 rtkNoPos } |
                Expression { Ctr__OptExpression__1 (rtkPosOf $1) $1 }

OptFinally : qq_OptFinally { Anti_OptFinally (tkVal_qq_OptFinally $1) } |
             { Ctr__OptFinally__0 rtkNoPos } |
             Rule_56 { Ctr__OptFinally__1 (rtkPosOf $1) $1 }

OptId : qq_OptId { Anti_OptId (tkVal_qq_OptId $1) } |
        { Ctr__OptId__0 rtkNoPos } |
        id { Ctr__OptId__1 (rtkPosOf $1) (tkVal_id $1) }

OptVariableInitializer : qq_OptVariableInitializer { Anti_OptVariableInitializer (tkVal_qq_OptVariableInitializer $1) } |
                         { Ctr__OptVariableInitializer__0 rtkNoPos } |
                         Rule_44 { Ctr__OptVariableInitializer__1 (rtkPosOf $1) $1 }

Package : qq_Package { Anti_Package (tkVal_qq_Package $1) } |
          tok_package_0 CompoundName tok__semi__1 { Ctr__Package__0 (rtkPosOf $1) $2 }

Parameter : qq_Parameter { Anti_Parameter (tkVal_qq_Parameter $1) } |
            Type id Dims { Ctr__Parameter__0 (rtkPosOf $1) $1 (tkVal_id $2) (reverse $3) }

ParameterList : qq_ParameterList { Anti_ParameterList (tkVal_qq_ParameterList $1) } |
                Parameter Rule_49 { Ctr__ParameterList__0 (rtkPosOf $1) $1 (reverse $2) }

PostfixOp : qq_PostfixOp { Anti_PostfixOp (tkVal_qq_PostfixOp $1) } |
            tok__plus__plus__76 { Ctr__PostfixOp__0 (rtkPosOf $1) } |
            tok__minus__minus__77 { Ctr__PostfixOp__1 (rtkPosOf $1) }

PrefixOp : qq_PrefixOp { Anti_PrefixOp (tkVal_qq_PrefixOp $1) } |
           tok__plus__plus__76 { Ctr__PrefixOp__0 (rtkPosOf $1) } |
           tok__minus__minus__77 { Ctr__PrefixOp__1 (rtkPosOf $1) } |
           tok__plus__72 { Ctr__PrefixOp__2 (rtkPosOf $1) } |
           tok__minus__73 { Ctr__PrefixOp__3 (rtkPosOf $1) }

PrimitiveTypeKeyword : qq_PrimitiveTypeKeyword { Anti_PrimitiveTypeKeyword (tkVal_qq_PrimitiveTypeKeyword $1) } |
                       tok_boolean_19 { Ctr__PrimitiveTypeKeyword__0 (rtkPosOf $1) } |
                       tok_byte_20 { Ctr__PrimitiveTypeKeyword__1 (rtkPosOf $1) } |
                       tok_char_21 { Ctr__PrimitiveTypeKeyword__2 (rtkPosOf $1) } |
                       tok_short_22 { Ctr__PrimitiveTypeKeyword__3 (rtkPosOf $1) } |
                       tok_int_23 { Ctr__PrimitiveTypeKeyword__4 (rtkPosOf $1) } |
                       tok_float_24 { Ctr__PrimitiveTypeKeyword__5 (rtkPosOf $1) } |
                       tok_long_25 { Ctr__PrimitiveTypeKeyword__6 (rtkPosOf $1) } |
                       tok_double_26 { Ctr__PrimitiveTypeKeyword__7 (rtkPosOf $1) } |
                       tok_void_27 { Ctr__PrimitiveTypeKeyword__8 (rtkPosOf $1) }

RelationalOp : qq_RelationalOp { Anti_RelationalOp (tkVal_qq_RelationalOp $1) } |
               tok__symbol__64 { Ctr__RelationalOp__0 (rtkPosOf $1) } |
               tok__symbol__65 { Ctr__RelationalOp__1 (rtkPosOf $1) } |
               tok__symbol__eql__66 { Ctr__RelationalOp__2 (rtkPosOf $1) } |
               tok__symbol__eql__67 { Ctr__RelationalOp__3 (rtkPosOf $1) }

Rule_1 : { Ctr__Rule_1__0 rtkNoPos } |
         Package { Ctr__Rule_1__1 (rtkPosOf $1) $1 }

ListElem_ModifierList11 : qq_ModifierList { Anti_Rule_10 (tkVal_qq_ModifierList $1) } |
                          Rule_10 { $1 }

Rule_10 : Modifier { Ctr__Rule_10__1 (rtkPosOf $1) $1 } |
          Annotation { Ctr__Rule_10__2 (rtkPosOf $1) $1 }

Rule_12 : {- empty -} { [] } |
          Rule_12 Rule_13 { $2 : $1 }

Rule_13 : tok__coma__8 CompoundName { Ctr__Rule_13__0 (rtkPosOf $1) $2 }

Rule_14 : CompoundName { [$1] } |
          Rule_14 tok__coma__8 CompoundName { $3 : $1 }

Rule_16 : { Ctr__Rule_16__0 rtkNoPos } |
          ExtendsList { Ctr__Rule_16__1 (rtkPosOf $1) $1 }

Rule_17 : { Ctr__Rule_17__0 rtkNoPos } |
          ImplementsList { Ctr__Rule_17__1 (rtkPosOf $1) $1 }

Rule_18 : { Ctr__Rule_18__0 rtkNoPos } |
          ExtendsList { Ctr__Rule_18__1 (rtkPosOf $1) $1 }

Rule_2 : { Ctr__Rule_2__0 rtkNoPos } |
         TypeDeclaration { Ctr__Rule_2__1 (rtkPosOf $1) $1 }

Rule_20 : { Ctr__Rule_20__0 rtkNoPos } |
          Rule_21 { Ctr__Rule_20__1 (rtkPosOf $1) $1 }

Rule_21 : tok__lparen__6 Arglist tok__rparen__7 { Ctr__Rule_21__0 (rtkPosOf $1) $2 }

Rule_22 : { Ctr__Rule_22__0 rtkNoPos } |
          Rule_23 { Ctr__Rule_22__1 (rtkPosOf $1) $1 }

Rule_23 : tok__symbol__13 FieldDeclarationList tok__symbol__14 { Ctr__Rule_23__0 (rtkPosOf $1) (reverse $2) }

Rule_24 : {- empty -} { [] } |
          Rule_24 Rule_25 { $2 : $1 }

Rule_25 : tok__coma__8 EnumConstant { Ctr__Rule_25__0 (rtkPosOf $1) $2 }

Rule_26 : { Ctr__Rule_26__0 rtkNoPos } |
          tok__coma__8 { Ctr__Rule_26__1 (rtkPosOf $1) }

Rule_27 : { Ctr__Rule_27__0 rtkNoPos } |
          ImplementsList { Ctr__Rule_27__1 (rtkPosOf $1) $1 }

Rule_28 : { Ctr__Rule_28__0 rtkNoPos } |
          Rule_29 { Ctr__Rule_28__1 (rtkPosOf $1) $1 }

Rule_29 : tok__semi__1 FieldDeclarationList { Ctr__Rule_29__0 (rtkPosOf $1) (reverse $2) }

Rule_3 : CompoundName tok__dot__3 tok__star__4 { Ctr__Rule_3__0 (rtkPosOf $1) $1 } |
         CompoundName { Ctr__Rule_3__1 (rtkPosOf $1) $1 }

Rule_30 : MemberDeclaration { Ctr__Rule_30__0 (rtkPosOf $1) $1 } |
          TypeDeclRest { Ctr__Rule_30__1 (rtkPosOf $1) $1 } |
          StaticInitializer { Ctr__Rule_30__2 (rtkPosOf $1) $1 }

ListElem_Dims32 : qq_Dims { Anti_Rule_31 (tkVal_qq_Dims $1) } |
                  Rule_31 { $1 }

Rule_31 : tok__sq_bkt_l__17 tok__sq_bkt_r__18 { Ctr__Rule_31__1 (rtkPosOf $1) }

ListElem_NonEmptyDims34 : qq_NonEmptyDims { Anti_Rule_33 (tkVal_qq_NonEmptyDims $1) } |
                          Rule_33 { $1 }

Rule_33 : tok__sq_bkt_l__17 tok__sq_bkt_r__18 { Ctr__Rule_33__1 (rtkPosOf $1) }

Rule_35 : { Ctr__Rule_35__0 rtkNoPos } |
          ParameterList { Ctr__Rule_35__1 (rtkPosOf $1) $1 }

Rule_36 : { Ctr__Rule_36__0 rtkNoPos } |
          ParameterList { Ctr__Rule_36__1 (rtkPosOf $1) $1 }

Rule_37 : StatementBlock { Ctr__Rule_37__0 (rtkPosOf $1) $1 } |
          Rule_38 tok__semi__1 { Ctr__Rule_37__1 (rtkPosOf $1) $1 }

Rule_38 : { Ctr__Rule_38__0 rtkNoPos } |
          Rule_39 { Ctr__Rule_38__1 (rtkPosOf $1) $1 }

Rule_39 : tok_default_28 Expression { Ctr__Rule_39__0 (rtkPosOf $1) $2 }

Rule_4 : { Ctr__Rule_4__0 rtkNoPos } |
         Rule_5 { Ctr__Rule_4__1 (rtkPosOf $1) $1 }

ListElem_MoreVariableDeclarators41 : qq_MoreVariableDeclarators { Anti_Rule_40 (tkVal_qq_MoreVariableDeclarators $1) } |
                                     Rule_40 { $1 }

Rule_40 : tok__coma__8 VariableDeclarator { Ctr__Rule_40__1 (rtkPosOf $1) $2 }

Rule_42 : {- empty -} { [] } |
          Rule_42 Rule_43 { $2 : $1 }

Rule_43 : tok__coma__8 VariableDeclarator { Ctr__Rule_43__0 (rtkPosOf $1) $2 }

Rule_44 : tok__eql__9 VariableInitializer { Ctr__Rule_44__0 (rtkPosOf $1) $2 }

Rule_45 : VariableInitializer Rule_46 Rule_48 { Ctr__Rule_45__0 (rtkPosOf $1) $1 (reverse $2) $3 }

Rule_46 : {- empty -} { [] } |
          Rule_46 Rule_47 { $2 : $1 }

Rule_47 : tok__coma__8 VariableInitializer { Ctr__Rule_47__0 (rtkPosOf $1) $2 }

Rule_48 : { Ctr__Rule_48__0 rtkNoPos } |
          tok__coma__8 { Ctr__Rule_48__1 (rtkPosOf $1) }

Rule_49 : {- empty -} { [] } |
          Rule_49 Rule_50 { $2 : $1 }

Rule_5 : tok__lparen__6 Rule_6 tok__rparen__7 { Ctr__Rule_5__0 (rtkPosOf $1) $2 }

Rule_50 : tok__coma__8 Parameter { Ctr__Rule_50__0 (rtkPosOf $1) $2 }

Rule_52 : tok_else_35 Statement { Ctr__Rule_52__0 (rtkPosOf $1) $2 }

Rule_53 : VariableDeclaration { Ctr__Rule_53__0 (rtkPosOf $1) $1 } |
          Expression tok__semi__1 { Ctr__Rule_53__1 (rtkPosOf $1) $1 } |
          tok__semi__1 { Ctr__Rule_53__2 (rtkPosOf $1) }

ListElem_CatchList55 : qq_CatchList { Anti_Rule_54 (tkVal_qq_CatchList $1) } |
                       Rule_54 { $1 }

Rule_54 : tok_catch_40 tok__lparen__6 Parameter tok__rparen__7 Statement { Ctr__Rule_54__1 (rtkPosOf $1) $3 $5 }

Rule_56 : tok_finally_41 Statement { Ctr__Rule_56__0 (rtkPosOf $1) $2 }

ListElem_SwitchCaseList58 : qq_SwitchCaseList { Anti_Rule_57 (tkVal_qq_SwitchCaseList $1) } |
                            Rule_57 { $1 }

Rule_57 : tok_case_43 Expression tok__colon__32 { Ctr__Rule_57__1 (rtkPosOf $1) $2 } |
          tok_default_28 tok__colon__32 { Ctr__Rule_57__2 (rtkPosOf $1) } |
          Statement { Ctr__Rule_57__3 (rtkPosOf $1) $1 }

Rule_59 : { Ctr__Rule_59__0 rtkNoPos } |
          Rule_60 { Ctr__Rule_59__1 (rtkPosOf $1) $1 }

Rule_6 : { Ctr__Rule_6__0 rtkNoPos } |
         AnnotationArguments { Ctr__Rule_6__1 (rtkPosOf $1) $1 }

Rule_60 : AssignmentOp AssignmentExpression { Ctr__Rule_60__0 (rtkPosOf $1) $1 $2 }

Rule_61 : { Ctr__Rule_61__0 rtkNoPos } |
          Rule_62 { Ctr__Rule_61__1 (rtkPosOf $1) $1 }

Rule_62 : tok__lparen__6 Arglist tok__rparen__7 { Ctr__Rule_62__0 (rtkPosOf $1) $2 }

Rule_63 : { Ctr__Rule_63__0 rtkNoPos } |
          Rule_64 { Ctr__Rule_63__1 (rtkPosOf $1) $1 }

Rule_64 : tok__lparen__6 Arglist tok__rparen__7 { Ctr__Rule_64__0 (rtkPosOf $1) $2 }

Rule_65 : tok__lparen__6 Arglist tok__rparen__7 { Ctr__Rule_65__0 (rtkPosOf $1) $2 } |
          DimExprs Rule_66 { Ctr__Rule_65__1 (rtkPosOf (reverse $1)) (reverse $1) $2 }

Rule_66 : { Ctr__Rule_66__0 rtkNoPos } |
          NonEmptyDims { Ctr__Rule_66__1 (rtkPosOf (reverse $1)) (reverse $1) }

ListElem_DimExprs68 : qq_DimExprs { Anti_Rule_67 (tkVal_qq_DimExprs $1) } |
                      Rule_67 { $1 }

Rule_67 : tok__sq_bkt_l__17 Expression tok__sq_bkt_r__18 { Ctr__Rule_67__1 (rtkPosOf $1) $2 }

Rule_69 : Expression Rule_70 { Ctr__Rule_69__0 (rtkPosOf $1) $1 (reverse $2) }

Rule_7 : {- empty -} { [] } |
         Rule_7 Rule_8 { $2 : $1 }

Rule_70 : {- empty -} { [] } |
          Rule_70 Rule_71 { $2 : $1 }

Rule_71 : tok__coma__8 Expression { Ctr__Rule_71__0 (rtkPosOf $1) $2 }

Rule_72 : {- empty -} { [] } |
          Rule_72 Rule_73 { $2 : $1 }

Rule_73 : tok__coma__8 TypeArgument { Ctr__Rule_73__0 (rtkPosOf $1) $2 }

Rule_74 : tok__symbol__64 TypeParameter Rule_75 tok__symbol__65 { Ctr__Rule_74__0 (rtkPosOf $1) $2 (reverse $3) }

Rule_75 : {- empty -} { [] } |
          Rule_75 Rule_76 { $2 : $1 }

Rule_76 : tok__coma__8 TypeParameter { Ctr__Rule_76__0 (rtkPosOf $1) $2 }

Rule_77 : { Ctr__Rule_77__0 rtkNoPos } |
          Rule_78 { Ctr__Rule_77__1 (rtkPosOf $1) $1 }

Rule_78 : tok_extends_10 Type Rule_79 { Ctr__Rule_78__0 (rtkPosOf $1) $2 (reverse $3) }

Rule_79 : {- empty -} { [] } |
          Rule_79 Rule_80 { $2 : $1 }

Rule_8 : tok__coma__8 AnnotationElement { Ctr__Rule_8__0 (rtkPosOf $1) $2 }

Rule_80 : tok__symbol__61 Type { Ctr__Rule_80__0 (rtkPosOf $1) $2 }

Rule_81 : {- empty -} { [] } |
          Rule_81 Rule_82 { $2 : $1 }

Rule_82 : tok__dot__3 id { Ctr__Rule_82__0 (rtkPosOf $1) (tkVal_id $2) }

ShiftOp : qq_ShiftOp { Anti_ShiftOp (tkVal_qq_ShiftOp $1) } |
          tok__symbol__symbol__69 { Ctr__ShiftOp__0 (rtkPosOf $1) } |
          tok__symbol__symbol__70 { Ctr__ShiftOp__1 (rtkPosOf $1) } |
          tok__symbol__symbol__symbol__71 { Ctr__ShiftOp__2 (rtkPosOf $1) }

Statement : qq_Statement { Anti_Statement (tkVal_qq_Statement $1) } |
            VariableDeclaration { Ctr__Statement__0 (rtkPosOf $1) $1 } |
            tok_return_29 OptExpression tok__semi__1 { Ctr__Statement__1 (rtkPosOf $1) $2 } |
            Expression tok__semi__1 { Ctr__Statement__2 (rtkPosOf $1) $1 } |
            StatementBlock { Ctr__Statement__3 (rtkPosOf $1) $1 } |
            IfStatement { Ctr__Statement__4 (rtkPosOf $1) $1 } |
            DoStatement { Ctr__Statement__5 (rtkPosOf $1) $1 } |
            WhileStatement { Ctr__Statement__6 (rtkPosOf $1) $1 } |
            ForStatement { Ctr__Statement__7 (rtkPosOf $1) $1 } |
            TryStatement { Ctr__Statement__8 (rtkPosOf $1) $1 } |
            SwitchStatement { Ctr__Statement__9 (rtkPosOf $1) $1 } |
            tok_synchronized_30 tok__lparen__6 Expression tok__rparen__7 Statement { Ctr__Statement__10 (rtkPosOf $1) $3 $5 } |
            tok_throw_31 Expression tok__semi__1 { Ctr__Statement__11 (rtkPosOf $1) $2 } |
            id tok__colon__32 Statement { Ctr__Statement__12 (rtkPosOf $1) (tkVal_id $1) $3 } |
            tok_break_33 OptId tok__semi__1 { Ctr__Statement__13 (rtkPosOf $1) $2 } |
            tok_continue_34 OptId tok__semi__1 { Ctr__Statement__14 (rtkPosOf $1) $2 } |
            tok__semi__1 { Ctr__Statement__15 (rtkPosOf $1) }

ListElem_StatementList51 : qq_StatementList { Anti_Statement (tkVal_qq_StatementList $1) } |
                           Statement { $1 }

StatementBlock : qq_StatementBlock { Anti_StatementBlock (tkVal_qq_StatementBlock $1) } |
                 tok__symbol__13 StatementList tok__symbol__14 { Ctr__StatementBlock__0 (rtkPosOf $1) (reverse $2) }

StatementList : {- empty -} { [] } |
                StatementList ListElem_StatementList51 { $2 : $1 }

StaticInitializer : qq_StaticInitializer { Anti_StaticInitializer (tkVal_qq_StaticInitializer $1) } |
                    StatementBlock { Ctr__StaticInitializer__0 (rtkPosOf $1) $1 }

SwitchCaseList : {- empty -} { [] } |
                 SwitchCaseList ListElem_SwitchCaseList58 { $2 : $1 }

SwitchStatement : qq_SwitchStatement { Anti_SwitchStatement (tkVal_qq_SwitchStatement $1) } |
                  tok_switch_44 tok__lparen__6 Expression tok__rparen__7 tok__symbol__13 SwitchCaseList tok__symbol__14 { Ctr__SwitchStatement__0 (rtkPosOf $1) $3 (reverse $6) }

TryStatement : qq_TryStatement { Anti_TryStatement (tkVal_qq_TryStatement $1) } |
               tok_try_42 Statement CatchList OptFinally { Ctr__TryStatement__0 (rtkPosOf $1) $2 (reverse $3) $4 }

Type : qq_Type { Anti_Type (tkVal_qq_Type $1) } |
       PrimitiveTypeKeyword Dims { Ctr__Type__0 (rtkPosOf $1) $1 (reverse $2) } |
       CompoundName NonEmptyTypeArguments Dims { Ctr__Type__1 (rtkPosOf $1) $1 $2 (reverse $3) } |
       CompoundName NonEmptyDims { Ctr__Type__2 (rtkPosOf $1) $1 (reverse $2) } |
       CompoundName { Ctr__Type__3 (rtkPosOf $1) $1 }

TypeArgument : qq_TypeArgument { Anti_TypeArgument (tkVal_qq_TypeArgument $1) } |
               Type { Ctr__TypeArgument__0 (rtkPosOf $1) $1 } |
               WildcardType { Ctr__TypeArgument__1 (rtkPosOf $1) $1 }

TypeArguments : qq_TypeArguments { Anti_TypeArguments (tkVal_qq_TypeArguments $1) } |
                { Ctr__TypeArguments__0 rtkNoPos } |
                NonEmptyTypeArguments { Ctr__TypeArguments__1 (rtkPosOf $1) $1 }

TypeDeclRest : qq_TypeDeclRest { Anti_TypeDeclRest (tkVal_qq_TypeDeclRest $1) } |
               ClassDeclaration { Ctr__TypeDeclRest__0 (rtkPosOf $1) $1 } |
               InterfaceDeclaration { Ctr__TypeDeclRest__1 (rtkPosOf $1) $1 } |
               EnumDeclaration { Ctr__TypeDeclRest__2 (rtkPosOf $1) $1 } |
               AnnotationDeclaration { Ctr__TypeDeclRest__3 (rtkPosOf $1) $1 }

TypeDeclaration : qq_TypeDeclaration { Anti_TypeDeclaration (tkVal_qq_TypeDeclaration $1) } |
                  OptDocComment ModifierList TypeDeclRest { Ctr__TypeDeclaration__0 (rtkPosOf $1) $1 (reverse $2) $3 }

TypeParameter : qq_TypeParameter { Anti_TypeParameter (tkVal_qq_TypeParameter $1) } |
                id Rule_77 { Ctr__TypeParameter__0 (rtkPosOf $1) (tkVal_id $1) $2 }

TypeParameters : qq_TypeParameters { Anti_TypeParameters (tkVal_qq_TypeParameters $1) } |
                 { Ctr__TypeParameters__0 rtkNoPos } |
                 Rule_74 { Ctr__TypeParameters__1 (rtkPosOf $1) $1 }

TypeSpecifier : qq_TypeSpecifier { Anti_TypeSpecifier (tkVal_qq_TypeSpecifier $1) } |
                tok_boolean_19 { Ctr__TypeSpecifier__0 (rtkPosOf $1) } |
                tok_byte_20 { Ctr__TypeSpecifier__1 (rtkPosOf $1) } |
                tok_char_21 { Ctr__TypeSpecifier__2 (rtkPosOf $1) } |
                tok_short_22 { Ctr__TypeSpecifier__3 (rtkPosOf $1) } |
                tok_int_23 { Ctr__TypeSpecifier__4 (rtkPosOf $1) } |
                tok_float_24 { Ctr__TypeSpecifier__5 (rtkPosOf $1) } |
                tok_long_25 { Ctr__TypeSpecifier__6 (rtkPosOf $1) } |
                tok_double_26 { Ctr__TypeSpecifier__7 (rtkPosOf $1) } |
                tok_void_27 { Ctr__TypeSpecifier__8 (rtkPosOf $1) } |
                CompoundName TypeArguments { Ctr__TypeSpecifier__9 (rtkPosOf $1) $1 $2 }

VariableDeclaration : qq_VariableDeclaration { Anti_VariableDeclaration (tkVal_qq_VariableDeclaration $1) } |
                      Type VariableDeclaratorList tok__semi__1 { Ctr__VariableDeclaration__0 (rtkPosOf $1) $1 $2 }

VariableDeclarator : qq_VariableDeclarator { Anti_VariableDeclarator (tkVal_qq_VariableDeclarator $1) } |
                     id Dims OptVariableInitializer { Ctr__VariableDeclarator__0 (rtkPosOf $1) (tkVal_id $1) (reverse $2) $3 }

VariableDeclaratorList : qq_VariableDeclaratorList { Anti_VariableDeclaratorList (tkVal_qq_VariableDeclaratorList $1) } |
                         VariableDeclarator Rule_42 { Ctr__VariableDeclaratorList__0 (rtkPosOf $1) $1 (reverse $2) }

VariableInitializer : qq_VariableInitializer { Anti_VariableInitializer (tkVal_qq_VariableInitializer $1) } |
                      Expression { Ctr__VariableInitializer__0 (rtkPosOf $1) $1 } |
                      tok__symbol__13 VariableInitializerList tok__symbol__14 { Ctr__VariableInitializer__1 (rtkPosOf $1) $2 }

VariableInitializerList : qq_VariableInitializerList { Anti_VariableInitializerList (tkVal_qq_VariableInitializerList $1) } |
                          { Ctr__VariableInitializerList__0 rtkNoPos } |
                          Rule_45 { Ctr__VariableInitializerList__1 (rtkPosOf $1) $1 }

WhileStatement : qq_WhileStatement { Anti_WhileStatement (tkVal_qq_WhileStatement $1) } |
                 tok_while_38 tok__lparen__6 Expression tok__rparen__7 Statement { Ctr__WhileStatement__0 (rtkPosOf $1) $3 $5 }

WildcardType : qq_WildcardType { Anti_WildcardType (tkVal_qq_WildcardType $1) } |
               tok__symbol__56 { Ctr__WildcardType__0 (rtkPosOf $1) } |
               tok__symbol__56 tok_extends_10 Type { Ctr__WildcardType__1 (rtkPosOf $1) $3 } |
               tok__symbol__56 tok_super_81 Type { Ctr__WildcardType__2 (rtkPosOf $1) $3 }


{
parseError :: [L.PosToken] -> Either String a
parseError [] = Left "unexpected end of input"
parseError (L.PosToken (L.AlexPn _ line col) tok : _) =
    Left $ show line ++ ":" ++ show col ++ ":unexpected " ++ showRtkToken tok

-- Render a token the way it appears in the source, for error messages
showRtkToken :: L.Token -> String
showRtkToken L.EndOfFile = "end of input"
showRtkToken L.Tk__tok_AdditiveOp_dummy_162 = "'tok_AdditiveOp_dummy_162'"
showRtkToken L.Tk__tok_Annotation_dummy_161 = "'tok_Annotation_dummy_161'"
showRtkToken L.Tk__tok_AnnotationArguments_dummy_160 = "'tok_AnnotationArguments_dummy_160'"
showRtkToken L.Tk__tok_AnnotationDeclaration_dummy_159 = "'tok_AnnotationDeclaration_dummy_159'"
showRtkToken L.Tk__tok_AnnotationElement_dummy_158 = "'tok_AnnotationElement_dummy_158'"
showRtkToken L.Tk__tok_AnnotationList_dummy_157 = "'tok_AnnotationList_dummy_157'"
showRtkToken L.Tk__tok_AnnotationTypeElement_dummy_156 = "'tok_AnnotationTypeElement_dummy_156'"
showRtkToken L.Tk__tok_AnnotationTypeElementList_dummy_155 = "'tok_AnnotationTypeElementList_dummy_155'"
showRtkToken L.Tk__tok_Arglist_dummy_154 = "'tok_Arglist_dummy_154'"
showRtkToken L.Tk__tok_AssignmentOp_dummy_153 = "'tok_AssignmentOp_dummy_153'"
showRtkToken L.Tk__tok_CatchList_dummy_152 = "'tok_CatchList_dummy_152'"
showRtkToken L.Tk__tok_ClassDeclaration_dummy_151 = "'tok_ClassDeclaration_dummy_151'"
showRtkToken L.Tk__tok_CompilationUnit_dummy_150 = "'tok_CompilationUnit_dummy_150'"
showRtkToken L.Tk__tok_CompoundName_dummy_149 = "'tok_CompoundName_dummy_149'"
showRtkToken L.Tk__tok_CreationExpression_dummy_148 = "'tok_CreationExpression_dummy_148'"
showRtkToken L.Tk__tok_DimExprs_dummy_147 = "'tok_DimExprs_dummy_147'"
showRtkToken L.Tk__tok_Dims_dummy_146 = "'tok_Dims_dummy_146'"
showRtkToken L.Tk__tok_DoStatement_dummy_145 = "'tok_DoStatement_dummy_145'"
showRtkToken L.Tk__tok_DocComment_dummy_144 = "'tok_DocComment_dummy_144'"
showRtkToken L.Tk__tok_EnumConstant_dummy_143 = "'tok_EnumConstant_dummy_143'"
showRtkToken L.Tk__tok_EnumConstantList_dummy_142 = "'tok_EnumConstantList_dummy_142'"
showRtkToken L.Tk__tok_EnumDeclaration_dummy_141 = "'tok_EnumDeclaration_dummy_141'"
showRtkToken L.Tk__tok_EqualityOp_dummy_140 = "'tok_EqualityOp_dummy_140'"
showRtkToken L.Tk__tok_Expression_dummy_139 = "'tok_Expression_dummy_139'"
showRtkToken L.Tk__tok_ExtendsList_dummy_138 = "'tok_ExtendsList_dummy_138'"
showRtkToken L.Tk__tok_FieldDeclaration_dummy_137 = "'tok_FieldDeclaration_dummy_137'"
showRtkToken L.Tk__tok_FieldDeclarationList_dummy_136 = "'tok_FieldDeclarationList_dummy_136'"
showRtkToken L.Tk__tok_ForStatement_dummy_135 = "'tok_ForStatement_dummy_135'"
showRtkToken L.Tk__tok_IfStatement_dummy_134 = "'tok_IfStatement_dummy_134'"
showRtkToken L.Tk__tok_ImplementsList_dummy_133 = "'tok_ImplementsList_dummy_133'"
showRtkToken L.Tk__tok_ImportList_dummy_132 = "'tok_ImportList_dummy_132'"
showRtkToken L.Tk__tok_ImportStatement_dummy_131 = "'tok_ImportStatement_dummy_131'"
showRtkToken L.Tk__tok_InterfaceDeclaration_dummy_130 = "'tok_InterfaceDeclaration_dummy_130'"
showRtkToken L.Tk__tok_Java_dummy_163 = "'tok_Java_dummy_163'"
showRtkToken L.Tk__tok_Literal_dummy_129 = "'tok_Literal_dummy_129'"
showRtkToken L.Tk__tok_MemberAfterFirstId_dummy_128 = "'tok_MemberAfterFirstId_dummy_128'"
showRtkToken L.Tk__tok_MemberDeclaration_dummy_127 = "'tok_MemberDeclaration_dummy_127'"
showRtkToken L.Tk__tok_MemberRest_dummy_126 = "'tok_MemberRest_dummy_126'"
showRtkToken L.Tk__tok_Modifier_dummy_125 = "'tok_Modifier_dummy_125'"
showRtkToken L.Tk__tok_ModifierList_dummy_124 = "'tok_ModifierList_dummy_124'"
showRtkToken L.Tk__tok_MoreTypeSpecifier_dummy_123 = "'tok_MoreTypeSpecifier_dummy_123'"
showRtkToken L.Tk__tok_MoreVariableDeclarators_dummy_122 = "'tok_MoreVariableDeclarators_dummy_122'"
showRtkToken L.Tk__tok_MultiplicativeOp_dummy_121 = "'tok_MultiplicativeOp_dummy_121'"
showRtkToken L.Tk__tok_NonEmptyDims_dummy_120 = "'tok_NonEmptyDims_dummy_120'"
showRtkToken L.Tk__tok_NonEmptyTypeArguments_dummy_119 = "'tok_NonEmptyTypeArguments_dummy_119'"
showRtkToken L.Tk__tok_OptDocComment_dummy_118 = "'tok_OptDocComment_dummy_118'"
showRtkToken L.Tk__tok_OptElsePart_dummy_117 = "'tok_OptElsePart_dummy_117'"
showRtkToken L.Tk__tok_OptExpression_dummy_116 = "'tok_OptExpression_dummy_116'"
showRtkToken L.Tk__tok_OptFinally_dummy_115 = "'tok_OptFinally_dummy_115'"
showRtkToken L.Tk__tok_OptId_dummy_114 = "'tok_OptId_dummy_114'"
showRtkToken L.Tk__tok_OptVariableInitializer_dummy_113 = "'tok_OptVariableInitializer_dummy_113'"
showRtkToken L.Tk__tok_Package_dummy_112 = "'tok_Package_dummy_112'"
showRtkToken L.Tk__tok_Parameter_dummy_111 = "'tok_Parameter_dummy_111'"
showRtkToken L.Tk__tok_ParameterList_dummy_110 = "'tok_ParameterList_dummy_110'"
showRtkToken L.Tk__tok_PostfixOp_dummy_109 = "'tok_PostfixOp_dummy_109'"
showRtkToken L.Tk__tok_PrefixOp_dummy_108 = "'tok_PrefixOp_dummy_108'"
showRtkToken L.Tk__tok_PrimitiveTypeKeyword_dummy_107 = "'tok_PrimitiveTypeKeyword_dummy_107'"
showRtkToken L.Tk__tok_RelationalOp_dummy_106 = "'tok_RelationalOp_dummy_106'"
showRtkToken L.Tk__tok_ShiftOp_dummy_105 = "'tok_ShiftOp_dummy_105'"
showRtkToken L.Tk__tok_Statement_dummy_104 = "'tok_Statement_dummy_104'"
showRtkToken L.Tk__tok_StatementBlock_dummy_103 = "'tok_StatementBlock_dummy_103'"
showRtkToken L.Tk__tok_StatementList_dummy_102 = "'tok_StatementList_dummy_102'"
showRtkToken L.Tk__tok_StaticInitializer_dummy_101 = "'tok_StaticInitializer_dummy_101'"
showRtkToken L.Tk__tok_SwitchCaseList_dummy_100 = "'tok_SwitchCaseList_dummy_100'"
showRtkToken L.Tk__tok_SwitchStatement_dummy_99 = "'tok_SwitchStatement_dummy_99'"
showRtkToken L.Tk__tok_TryStatement_dummy_98 = "'tok_TryStatement_dummy_98'"
showRtkToken L.Tk__tok_Type_dummy_97 = "'tok_Type_dummy_97'"
showRtkToken L.Tk__tok_TypeArgument_dummy_96 = "'tok_TypeArgument_dummy_96'"
showRtkToken L.Tk__tok_TypeArguments_dummy_95 = "'tok_TypeArguments_dummy_95'"
showRtkToken L.Tk__tok_TypeDeclRest_dummy_94 = "'tok_TypeDeclRest_dummy_94'"
showRtkToken L.Tk__tok_TypeDeclaration_dummy_93 = "'tok_TypeDeclaration_dummy_93'"
showRtkToken L.Tk__tok_TypeParameter_dummy_92 = "'tok_TypeParameter_dummy_92'"
showRtkToken L.Tk__tok_TypeParameters_dummy_91 = "'tok_TypeParameters_dummy_91'"
showRtkToken L.Tk__tok_TypeSpecifier_dummy_90 = "'tok_TypeSpecifier_dummy_90'"
showRtkToken L.Tk__tok_VariableDeclaration_dummy_89 = "'tok_VariableDeclaration_dummy_89'"
showRtkToken L.Tk__tok_VariableDeclarator_dummy_88 = "'tok_VariableDeclarator_dummy_88'"
showRtkToken L.Tk__tok_VariableDeclaratorList_dummy_87 = "'tok_VariableDeclaratorList_dummy_87'"
showRtkToken L.Tk__tok_VariableInitializer_dummy_86 = "'tok_VariableInitializer_dummy_86'"
showRtkToken L.Tk__tok_VariableInitializerList_dummy_85 = "'tok_VariableInitializerList_dummy_85'"
showRtkToken L.Tk__tok_WhileStatement_dummy_84 = "'tok_WhileStatement_dummy_84'"
showRtkToken L.Tk__tok_WildcardType_dummy_83 = "'tok_WildcardType_dummy_83'"
showRtkToken L.Tk__tok__tilde__78 = "'~'"
showRtkToken L.Tk__tok__symbol__14 = "'}'"
showRtkToken L.Tk__tok__pipe__pipe__57 = "'||'"
showRtkToken L.Tk__tok__pipe__eql__49 = "'|='"
showRtkToken L.Tk__tok__pipe__59 = "'|'"
showRtkToken L.Tk__tok__symbol__13 = "'{'"
showRtkToken L.Tk__tok_while_38 = "'while'"
showRtkToken L.Tk__tok_void_27 = "'void'"
showRtkToken L.Tk__tok_try_42 = "'try'"
showRtkToken L.Tk__tok_true_83 = "'true'"
showRtkToken L.Tk__tok_transient_94 = "'transient'"
showRtkToken L.Tk__tok_throw_31 = "'throw'"
showRtkToken L.Tk__tok_threadsafe_93 = "'threadsafe'"
showRtkToken L.Tk__tok_this_80 = "'this'"
showRtkToken L.Tk__tok_synchronized_30 = "'synchronized'"
showRtkToken L.Tk__tok_switch_44 = "'switch'"
showRtkToken L.Tk__tok_super_81 = "'super'"
showRtkToken L.Tk__tok_static_89 = "'static'"
showRtkToken L.Tk__tok_short_22 = "'short'"
showRtkToken L.Tk__tok_return_29 = "'return'"
showRtkToken L.Tk__tok_public_86 = "'public'"
showRtkToken L.Tk__tok_protected_88 = "'protected'"
showRtkToken L.Tk__tok_private_87 = "'private'"
showRtkToken L.Tk__tok_package_0 = "'package'"
showRtkToken L.Tk__tok_null_85 = "'null'"
showRtkToken L.Tk__tok_new_82 = "'new'"
showRtkToken L.Tk__tok_native_91 = "'native'"
showRtkToken L.Tk__tok_long_25 = "'long'"
showRtkToken L.Tk__tok_interface_15 = "'interface'"
showRtkToken L.Tk__tok_int_23 = "'int'"
showRtkToken L.Tk__tok_instanceof_68 = "'instanceof'"
showRtkToken L.Tk__tok_import_2 = "'import'"
showRtkToken L.Tk__tok_implements_11 = "'implements'"
showRtkToken L.Tk__tok_if_36 = "'if'"
showRtkToken L.Tk__tok_for_39 = "'for'"
showRtkToken L.Tk__tok_float_24 = "'float'"
showRtkToken L.Tk__tok_finally_41 = "'finally'"
showRtkToken L.Tk__tok_final_90 = "'final'"
showRtkToken L.Tk__tok_false_84 = "'false'"
showRtkToken L.Tk__tok_extends_10 = "'extends'"
showRtkToken L.Tk__tok_enum_16 = "'enum'"
showRtkToken L.Tk__tok_else_35 = "'else'"
showRtkToken L.Tk__tok_double_26 = "'double'"
showRtkToken L.Tk__tok_do_37 = "'do'"
showRtkToken L.Tk__tok_default_28 = "'default'"
showRtkToken L.Tk__tok_continue_34 = "'continue'"
showRtkToken L.Tk__tok_class_12 = "'class'"
showRtkToken L.Tk__tok_char_21 = "'char'"
showRtkToken L.Tk__tok_catch_40 = "'catch'"
showRtkToken L.Tk__tok_case_43 = "'case'"
showRtkToken L.Tk__tok_byte_20 = "'byte'"
showRtkToken L.Tk__tok_break_33 = "'break'"
showRtkToken L.Tk__tok_boolean_19 = "'boolean'"
showRtkToken L.Tk__tok_abstract_92 = "'abstract'"
showRtkToken L.Tk__tok__symbol__eql__51 = "'^='"
showRtkToken L.Tk__tok__symbol__60 = "'^'"
showRtkToken L.Tk__tok__sq_bkt_r__18 = "']'"
showRtkToken L.Tk__tok__sq_bkt_l__17 = "'['"
showRtkToken L.Tk__tok__symbol__5 = "'@'"
showRtkToken L.Tk__tok__symbol__56 = "'?'"
showRtkToken L.Tk__tok__symbol__symbol__symbol__eql__55 = "'>>>='"
showRtkToken L.Tk__tok__symbol__symbol__symbol__71 = "'>>>'"
showRtkToken L.Tk__tok__symbol__symbol__eql__54 = "'>>='"
showRtkToken L.Tk__tok__symbol__symbol__69 = "'>>'"
showRtkToken L.Tk__tok__symbol__eql__67 = "'>='"
showRtkToken L.Tk__tok__symbol__65 = "'>'"
showRtkToken L.Tk__tok__eql__eql__62 = "'=='"
showRtkToken L.Tk__tok__eql__9 = "'='"
showRtkToken L.Tk__tok__symbol__eql__66 = "'<='"
showRtkToken L.Tk__tok__symbol__symbol__eql__53 = "'<<='"
showRtkToken L.Tk__tok__symbol__symbol__70 = "'<<'"
showRtkToken L.Tk__tok__symbol__64 = "'<'"
showRtkToken L.Tk__tok__semi__1 = "';'"
showRtkToken L.Tk__tok__colon__32 = "':'"
showRtkToken L.Tk__tok__symbol__eql__48 = "'/='"
showRtkToken L.Tk__tok__symbol__74 = "'/'"
showRtkToken L.Tk__tok__dot__3 = "'.'"
showRtkToken L.Tk__tok__minus__eql__46 = "'-='"
showRtkToken L.Tk__tok__minus__minus__77 = "'--'"
showRtkToken L.Tk__tok__minus__73 = "'-'"
showRtkToken L.Tk__tok__coma__8 = "','"
showRtkToken L.Tk__tok__plus__eql__45 = "'+='"
showRtkToken L.Tk__tok__plus__plus__76 = "'++'"
showRtkToken L.Tk__tok__plus__72 = "'+'"
showRtkToken L.Tk__tok__star__eql__47 = "'*='"
showRtkToken L.Tk__tok__star__4 = "'*'"
showRtkToken L.Tk__tok__rparen__7 = "')'"
showRtkToken L.Tk__tok__lparen__6 = "'('"
showRtkToken L.Tk__tok__symbol__eql__50 = "'&='"
showRtkToken L.Tk__tok__symbol__symbol__58 = "'&&'"
showRtkToken L.Tk__tok__symbol__61 = "'&'"
showRtkToken L.Tk__tok__symbol__eql__52 = "'%='"
showRtkToken L.Tk__tok__symbol__75 = "'%'"
showRtkToken L.Tk__tok__exclamation__eql__63 = "'!='"
showRtkToken L.Tk__tok__exclamation__79 = "'!'"
showRtkToken (L.Tk__doccomment v) = "doccomment " ++ show v
showRtkToken (L.Tk__id v) = "id " ++ show v
showRtkToken (L.Tk__string v) = "string " ++ show v
showRtkToken (L.Tk__char v) = "char " ++ show v
showRtkToken (L.Tk__floatTypeSuffix v) = "floatTypeSuffix " ++ show v
showRtkToken (L.Tk__exponentPart v) = "exponentPart " ++ show v
showRtkToken (L.Tk__floatLiteral v) = "floatLiteral " ++ show v
showRtkToken (L.Tk__integerLiteral v) = "integerLiteral " ++ show v
showRtkToken (L.Tk__qq_CompoundName v) = "qq_CompoundName " ++ show v
showRtkToken (L.Tk__qq_Modifier v) = "qq_Modifier " ++ show v
showRtkToken (L.Tk__qq_TypeSpecifier v) = "qq_TypeSpecifier " ++ show v
showRtkToken (L.Tk__qq_Type v) = "qq_Type " ++ show v
showRtkToken (L.Tk__qq_TypeParameter v) = "qq_TypeParameter " ++ show v
showRtkToken (L.Tk__qq_TypeParameters v) = "qq_TypeParameters " ++ show v
showRtkToken (L.Tk__qq_WildcardType v) = "qq_WildcardType " ++ show v
showRtkToken (L.Tk__qq_TypeArgument v) = "qq_TypeArgument " ++ show v
showRtkToken (L.Tk__qq_NonEmptyTypeArguments v) = "qq_NonEmptyTypeArguments " ++ show v
showRtkToken (L.Tk__qq_TypeArguments v) = "qq_TypeArguments " ++ show v
showRtkToken (L.Tk__qq_Arglist v) = "qq_Arglist " ++ show v
showRtkToken (L.Tk__qq_Literal v) = "qq_Literal " ++ show v
showRtkToken (L.Tk__qq_DimExprs v) = "qq_DimExprs " ++ show v
showRtkToken (L.Tk__qq_CreationExpression v) = "qq_CreationExpression " ++ show v
showRtkToken (L.Tk__qq_PostfixOp v) = "qq_PostfixOp " ++ show v
showRtkToken (L.Tk__qq_PrefixOp v) = "qq_PrefixOp " ++ show v
showRtkToken (L.Tk__qq_MultiplicativeOp v) = "qq_MultiplicativeOp " ++ show v
showRtkToken (L.Tk__qq_AdditiveOp v) = "qq_AdditiveOp " ++ show v
showRtkToken (L.Tk__qq_ShiftOp v) = "qq_ShiftOp " ++ show v
showRtkToken (L.Tk__qq_RelationalOp v) = "qq_RelationalOp " ++ show v
showRtkToken (L.Tk__qq_EqualityOp v) = "qq_EqualityOp " ++ show v
showRtkToken (L.Tk__qq_AssignmentOp v) = "qq_AssignmentOp " ++ show v
showRtkToken (L.Tk__qq_Expression v) = "qq_Expression " ++ show v
showRtkToken (L.Tk__qq_SwitchStatement v) = "qq_SwitchStatement " ++ show v
showRtkToken (L.Tk__qq_SwitchCaseList v) = "qq_SwitchCaseList " ++ show v
showRtkToken (L.Tk__qq_TryStatement v) = "qq_TryStatement " ++ show v
showRtkToken (L.Tk__qq_OptFinally v) = "qq_OptFinally " ++ show v
showRtkToken (L.Tk__qq_CatchList v) = "qq_CatchList " ++ show v
showRtkToken (L.Tk__qq_ForStatement v) = "qq_ForStatement " ++ show v
showRtkToken (L.Tk__qq_WhileStatement v) = "qq_WhileStatement " ++ show v
showRtkToken (L.Tk__qq_DoStatement v) = "qq_DoStatement " ++ show v
showRtkToken (L.Tk__qq_IfStatement v) = "qq_IfStatement " ++ show v
showRtkToken (L.Tk__qq_OptElsePart v) = "qq_OptElsePart " ++ show v
showRtkToken (L.Tk__qq_Statement v) = "qq_Statement " ++ show v
showRtkToken (L.Tk__qq_OptId v) = "qq_OptId " ++ show v
showRtkToken (L.Tk__qq_OptExpression v) = "qq_OptExpression " ++ show v
showRtkToken (L.Tk__qq_StatementList v) = "qq_StatementList " ++ show v
showRtkToken (L.Tk__qq_Parameter v) = "qq_Parameter " ++ show v
showRtkToken (L.Tk__qq_ParameterList v) = "qq_ParameterList " ++ show v
showRtkToken (L.Tk__qq_StaticInitializer v) = "qq_StaticInitializer " ++ show v
showRtkToken (L.Tk__qq_VariableInitializer v) = "qq_VariableInitializer " ++ show v
showRtkToken (L.Tk__qq_VariableInitializerList v) = "qq_VariableInitializerList " ++ show v
showRtkToken (L.Tk__qq_VariableDeclarator v) = "qq_VariableDeclarator " ++ show v
showRtkToken (L.Tk__qq_OptVariableInitializer v) = "qq_OptVariableInitializer " ++ show v
showRtkToken (L.Tk__qq_VariableDeclaration v) = "qq_VariableDeclaration " ++ show v
showRtkToken (L.Tk__qq_VariableDeclaratorList v) = "qq_VariableDeclaratorList " ++ show v
showRtkToken (L.Tk__qq_StatementBlock v) = "qq_StatementBlock " ++ show v
showRtkToken (L.Tk__qq_MoreVariableDeclarators v) = "qq_MoreVariableDeclarators " ++ show v
showRtkToken (L.Tk__qq_MemberRest v) = "qq_MemberRest " ++ show v
showRtkToken (L.Tk__qq_MoreTypeSpecifier v) = "qq_MoreTypeSpecifier " ++ show v
showRtkToken (L.Tk__qq_MemberAfterFirstId v) = "qq_MemberAfterFirstId " ++ show v
showRtkToken (L.Tk__qq_PrimitiveTypeKeyword v) = "qq_PrimitiveTypeKeyword " ++ show v
showRtkToken (L.Tk__qq_MemberDeclaration v) = "qq_MemberDeclaration " ++ show v
showRtkToken (L.Tk__qq_NonEmptyDims v) = "qq_NonEmptyDims " ++ show v
showRtkToken (L.Tk__qq_Dims v) = "qq_Dims " ++ show v
showRtkToken (L.Tk__qq_FieldDeclaration v) = "qq_FieldDeclaration " ++ show v
showRtkToken (L.Tk__qq_TypeDeclRest v) = "qq_TypeDeclRest " ++ show v
showRtkToken (L.Tk__qq_EnumDeclaration v) = "qq_EnumDeclaration " ++ show v
showRtkToken (L.Tk__qq_EnumConstantList v) = "qq_EnumConstantList " ++ show v
showRtkToken (L.Tk__qq_EnumConstant v) = "qq_EnumConstant " ++ show v
showRtkToken (L.Tk__qq_AnnotationTypeElement v) = "qq_AnnotationTypeElement " ++ show v
showRtkToken (L.Tk__qq_AnnotationTypeElementList v) = "qq_AnnotationTypeElementList " ++ show v
showRtkToken (L.Tk__qq_AnnotationDeclaration v) = "qq_AnnotationDeclaration " ++ show v
showRtkToken (L.Tk__qq_InterfaceDeclaration v) = "qq_InterfaceDeclaration " ++ show v
showRtkToken (L.Tk__qq_ClassDeclaration v) = "qq_ClassDeclaration " ++ show v
showRtkToken (L.Tk__qq_FieldDeclarationList v) = "qq_FieldDeclarationList " ++ show v
showRtkToken (L.Tk__qq_ImplementsList v) = "qq_ImplementsList " ++ show v
showRtkToken (L.Tk__qq_ExtendsList v) = "qq_ExtendsList " ++ show v
showRtkToken (L.Tk__qq_ModifierList v) = "qq_ModifierList " ++ show v
showRtkToken (L.Tk__qq_AnnotationList v) = "qq_AnnotationList " ++ show v
showRtkToken (L.Tk__qq_AnnotationElement v) = "qq_AnnotationElement " ++ show v
showRtkToken (L.Tk__qq_AnnotationArguments v) = "qq_AnnotationArguments " ++ show v
showRtkToken (L.Tk__qq_Annotation v) = "qq_Annotation " ++ show v
showRtkToken (L.Tk__qq_DocComment v) = "qq_DocComment " ++ show v
showRtkToken (L.Tk__qq_ImportStatement v) = "qq_ImportStatement " ++ show v
showRtkToken (L.Tk__qq_Package v) = "qq_Package " ++ show v
showRtkToken (L.Tk__qq_CompilationUnit v) = "qq_CompilationUnit " ++ show v
showRtkToken (L.Tk__qq_ImportList v) = "qq_ImportList " ++ show v
showRtkToken (L.Tk__qq_TypeDeclaration v) = "qq_TypeDeclaration " ++ show v
showRtkToken (L.Tk__qq_OptDocComment v) = "qq_OptDocComment " ++ show v
showRtkToken (L.Tk__qq_Java v) = "qq_Java " ++ show v

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
tkVal_doccomment :: L.PosToken -> String
tkVal_doccomment (L.PosToken _ (L.Tk__doccomment v)) = v
tkVal_doccomment t = error ("rtk internal error: token doccomment expected, got " ++ showRtkToken (L.ptToken t))
tkVal_id :: L.PosToken -> String
tkVal_id (L.PosToken _ (L.Tk__id v)) = v
tkVal_id t = error ("rtk internal error: token id expected, got " ++ showRtkToken (L.ptToken t))
tkVal_string :: L.PosToken -> String
tkVal_string (L.PosToken _ (L.Tk__string v)) = v
tkVal_string t = error ("rtk internal error: token string expected, got " ++ showRtkToken (L.ptToken t))
tkVal_char :: L.PosToken -> String
tkVal_char (L.PosToken _ (L.Tk__char v)) = v
tkVal_char t = error ("rtk internal error: token char expected, got " ++ showRtkToken (L.ptToken t))
tkVal_floatTypeSuffix :: L.PosToken -> String
tkVal_floatTypeSuffix (L.PosToken _ (L.Tk__floatTypeSuffix v)) = v
tkVal_floatTypeSuffix t = error ("rtk internal error: token floatTypeSuffix expected, got " ++ showRtkToken (L.ptToken t))
tkVal_exponentPart :: L.PosToken -> String
tkVal_exponentPart (L.PosToken _ (L.Tk__exponentPart v)) = v
tkVal_exponentPart t = error ("rtk internal error: token exponentPart expected, got " ++ showRtkToken (L.ptToken t))
tkVal_floatLiteral :: L.PosToken -> String
tkVal_floatLiteral (L.PosToken _ (L.Tk__floatLiteral v)) = v
tkVal_floatLiteral t = error ("rtk internal error: token floatLiteral expected, got " ++ showRtkToken (L.ptToken t))
tkVal_integerLiteral :: L.PosToken -> String
tkVal_integerLiteral (L.PosToken _ (L.Tk__integerLiteral v)) = v
tkVal_integerLiteral t = error ("rtk internal error: token integerLiteral expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_CompoundName :: L.PosToken -> String
tkVal_qq_CompoundName (L.PosToken _ (L.Tk__qq_CompoundName v)) = v
tkVal_qq_CompoundName t = error ("rtk internal error: token qq_CompoundName expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Modifier :: L.PosToken -> String
tkVal_qq_Modifier (L.PosToken _ (L.Tk__qq_Modifier v)) = v
tkVal_qq_Modifier t = error ("rtk internal error: token qq_Modifier expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TypeSpecifier :: L.PosToken -> String
tkVal_qq_TypeSpecifier (L.PosToken _ (L.Tk__qq_TypeSpecifier v)) = v
tkVal_qq_TypeSpecifier t = error ("rtk internal error: token qq_TypeSpecifier expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Type :: L.PosToken -> String
tkVal_qq_Type (L.PosToken _ (L.Tk__qq_Type v)) = v
tkVal_qq_Type t = error ("rtk internal error: token qq_Type expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TypeParameter :: L.PosToken -> String
tkVal_qq_TypeParameter (L.PosToken _ (L.Tk__qq_TypeParameter v)) = v
tkVal_qq_TypeParameter t = error ("rtk internal error: token qq_TypeParameter expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TypeParameters :: L.PosToken -> String
tkVal_qq_TypeParameters (L.PosToken _ (L.Tk__qq_TypeParameters v)) = v
tkVal_qq_TypeParameters t = error ("rtk internal error: token qq_TypeParameters expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_WildcardType :: L.PosToken -> String
tkVal_qq_WildcardType (L.PosToken _ (L.Tk__qq_WildcardType v)) = v
tkVal_qq_WildcardType t = error ("rtk internal error: token qq_WildcardType expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TypeArgument :: L.PosToken -> String
tkVal_qq_TypeArgument (L.PosToken _ (L.Tk__qq_TypeArgument v)) = v
tkVal_qq_TypeArgument t = error ("rtk internal error: token qq_TypeArgument expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_NonEmptyTypeArguments :: L.PosToken -> String
tkVal_qq_NonEmptyTypeArguments (L.PosToken _ (L.Tk__qq_NonEmptyTypeArguments v)) = v
tkVal_qq_NonEmptyTypeArguments t = error ("rtk internal error: token qq_NonEmptyTypeArguments expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TypeArguments :: L.PosToken -> String
tkVal_qq_TypeArguments (L.PosToken _ (L.Tk__qq_TypeArguments v)) = v
tkVal_qq_TypeArguments t = error ("rtk internal error: token qq_TypeArguments expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Arglist :: L.PosToken -> String
tkVal_qq_Arglist (L.PosToken _ (L.Tk__qq_Arglist v)) = v
tkVal_qq_Arglist t = error ("rtk internal error: token qq_Arglist expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Literal :: L.PosToken -> String
tkVal_qq_Literal (L.PosToken _ (L.Tk__qq_Literal v)) = v
tkVal_qq_Literal t = error ("rtk internal error: token qq_Literal expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_DimExprs :: L.PosToken -> String
tkVal_qq_DimExprs (L.PosToken _ (L.Tk__qq_DimExprs v)) = v
tkVal_qq_DimExprs t = error ("rtk internal error: token qq_DimExprs expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_CreationExpression :: L.PosToken -> String
tkVal_qq_CreationExpression (L.PosToken _ (L.Tk__qq_CreationExpression v)) = v
tkVal_qq_CreationExpression t = error ("rtk internal error: token qq_CreationExpression expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_PostfixOp :: L.PosToken -> String
tkVal_qq_PostfixOp (L.PosToken _ (L.Tk__qq_PostfixOp v)) = v
tkVal_qq_PostfixOp t = error ("rtk internal error: token qq_PostfixOp expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_PrefixOp :: L.PosToken -> String
tkVal_qq_PrefixOp (L.PosToken _ (L.Tk__qq_PrefixOp v)) = v
tkVal_qq_PrefixOp t = error ("rtk internal error: token qq_PrefixOp expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_MultiplicativeOp :: L.PosToken -> String
tkVal_qq_MultiplicativeOp (L.PosToken _ (L.Tk__qq_MultiplicativeOp v)) = v
tkVal_qq_MultiplicativeOp t = error ("rtk internal error: token qq_MultiplicativeOp expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_AdditiveOp :: L.PosToken -> String
tkVal_qq_AdditiveOp (L.PosToken _ (L.Tk__qq_AdditiveOp v)) = v
tkVal_qq_AdditiveOp t = error ("rtk internal error: token qq_AdditiveOp expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ShiftOp :: L.PosToken -> String
tkVal_qq_ShiftOp (L.PosToken _ (L.Tk__qq_ShiftOp v)) = v
tkVal_qq_ShiftOp t = error ("rtk internal error: token qq_ShiftOp expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_RelationalOp :: L.PosToken -> String
tkVal_qq_RelationalOp (L.PosToken _ (L.Tk__qq_RelationalOp v)) = v
tkVal_qq_RelationalOp t = error ("rtk internal error: token qq_RelationalOp expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_EqualityOp :: L.PosToken -> String
tkVal_qq_EqualityOp (L.PosToken _ (L.Tk__qq_EqualityOp v)) = v
tkVal_qq_EqualityOp t = error ("rtk internal error: token qq_EqualityOp expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_AssignmentOp :: L.PosToken -> String
tkVal_qq_AssignmentOp (L.PosToken _ (L.Tk__qq_AssignmentOp v)) = v
tkVal_qq_AssignmentOp t = error ("rtk internal error: token qq_AssignmentOp expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Expression :: L.PosToken -> String
tkVal_qq_Expression (L.PosToken _ (L.Tk__qq_Expression v)) = v
tkVal_qq_Expression t = error ("rtk internal error: token qq_Expression expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_SwitchStatement :: L.PosToken -> String
tkVal_qq_SwitchStatement (L.PosToken _ (L.Tk__qq_SwitchStatement v)) = v
tkVal_qq_SwitchStatement t = error ("rtk internal error: token qq_SwitchStatement expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_SwitchCaseList :: L.PosToken -> String
tkVal_qq_SwitchCaseList (L.PosToken _ (L.Tk__qq_SwitchCaseList v)) = v
tkVal_qq_SwitchCaseList t = error ("rtk internal error: token qq_SwitchCaseList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TryStatement :: L.PosToken -> String
tkVal_qq_TryStatement (L.PosToken _ (L.Tk__qq_TryStatement v)) = v
tkVal_qq_TryStatement t = error ("rtk internal error: token qq_TryStatement expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptFinally :: L.PosToken -> String
tkVal_qq_OptFinally (L.PosToken _ (L.Tk__qq_OptFinally v)) = v
tkVal_qq_OptFinally t = error ("rtk internal error: token qq_OptFinally expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_CatchList :: L.PosToken -> String
tkVal_qq_CatchList (L.PosToken _ (L.Tk__qq_CatchList v)) = v
tkVal_qq_CatchList t = error ("rtk internal error: token qq_CatchList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ForStatement :: L.PosToken -> String
tkVal_qq_ForStatement (L.PosToken _ (L.Tk__qq_ForStatement v)) = v
tkVal_qq_ForStatement t = error ("rtk internal error: token qq_ForStatement expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_WhileStatement :: L.PosToken -> String
tkVal_qq_WhileStatement (L.PosToken _ (L.Tk__qq_WhileStatement v)) = v
tkVal_qq_WhileStatement t = error ("rtk internal error: token qq_WhileStatement expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_DoStatement :: L.PosToken -> String
tkVal_qq_DoStatement (L.PosToken _ (L.Tk__qq_DoStatement v)) = v
tkVal_qq_DoStatement t = error ("rtk internal error: token qq_DoStatement expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_IfStatement :: L.PosToken -> String
tkVal_qq_IfStatement (L.PosToken _ (L.Tk__qq_IfStatement v)) = v
tkVal_qq_IfStatement t = error ("rtk internal error: token qq_IfStatement expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptElsePart :: L.PosToken -> String
tkVal_qq_OptElsePart (L.PosToken _ (L.Tk__qq_OptElsePart v)) = v
tkVal_qq_OptElsePart t = error ("rtk internal error: token qq_OptElsePart expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Statement :: L.PosToken -> String
tkVal_qq_Statement (L.PosToken _ (L.Tk__qq_Statement v)) = v
tkVal_qq_Statement t = error ("rtk internal error: token qq_Statement expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptId :: L.PosToken -> String
tkVal_qq_OptId (L.PosToken _ (L.Tk__qq_OptId v)) = v
tkVal_qq_OptId t = error ("rtk internal error: token qq_OptId expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptExpression :: L.PosToken -> String
tkVal_qq_OptExpression (L.PosToken _ (L.Tk__qq_OptExpression v)) = v
tkVal_qq_OptExpression t = error ("rtk internal error: token qq_OptExpression expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_StatementList :: L.PosToken -> String
tkVal_qq_StatementList (L.PosToken _ (L.Tk__qq_StatementList v)) = v
tkVal_qq_StatementList t = error ("rtk internal error: token qq_StatementList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Parameter :: L.PosToken -> String
tkVal_qq_Parameter (L.PosToken _ (L.Tk__qq_Parameter v)) = v
tkVal_qq_Parameter t = error ("rtk internal error: token qq_Parameter expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ParameterList :: L.PosToken -> String
tkVal_qq_ParameterList (L.PosToken _ (L.Tk__qq_ParameterList v)) = v
tkVal_qq_ParameterList t = error ("rtk internal error: token qq_ParameterList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_StaticInitializer :: L.PosToken -> String
tkVal_qq_StaticInitializer (L.PosToken _ (L.Tk__qq_StaticInitializer v)) = v
tkVal_qq_StaticInitializer t = error ("rtk internal error: token qq_StaticInitializer expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_VariableInitializer :: L.PosToken -> String
tkVal_qq_VariableInitializer (L.PosToken _ (L.Tk__qq_VariableInitializer v)) = v
tkVal_qq_VariableInitializer t = error ("rtk internal error: token qq_VariableInitializer expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_VariableInitializerList :: L.PosToken -> String
tkVal_qq_VariableInitializerList (L.PosToken _ (L.Tk__qq_VariableInitializerList v)) = v
tkVal_qq_VariableInitializerList t = error ("rtk internal error: token qq_VariableInitializerList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_VariableDeclarator :: L.PosToken -> String
tkVal_qq_VariableDeclarator (L.PosToken _ (L.Tk__qq_VariableDeclarator v)) = v
tkVal_qq_VariableDeclarator t = error ("rtk internal error: token qq_VariableDeclarator expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptVariableInitializer :: L.PosToken -> String
tkVal_qq_OptVariableInitializer (L.PosToken _ (L.Tk__qq_OptVariableInitializer v)) = v
tkVal_qq_OptVariableInitializer t = error ("rtk internal error: token qq_OptVariableInitializer expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_VariableDeclaration :: L.PosToken -> String
tkVal_qq_VariableDeclaration (L.PosToken _ (L.Tk__qq_VariableDeclaration v)) = v
tkVal_qq_VariableDeclaration t = error ("rtk internal error: token qq_VariableDeclaration expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_VariableDeclaratorList :: L.PosToken -> String
tkVal_qq_VariableDeclaratorList (L.PosToken _ (L.Tk__qq_VariableDeclaratorList v)) = v
tkVal_qq_VariableDeclaratorList t = error ("rtk internal error: token qq_VariableDeclaratorList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_StatementBlock :: L.PosToken -> String
tkVal_qq_StatementBlock (L.PosToken _ (L.Tk__qq_StatementBlock v)) = v
tkVal_qq_StatementBlock t = error ("rtk internal error: token qq_StatementBlock expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_MoreVariableDeclarators :: L.PosToken -> String
tkVal_qq_MoreVariableDeclarators (L.PosToken _ (L.Tk__qq_MoreVariableDeclarators v)) = v
tkVal_qq_MoreVariableDeclarators t = error ("rtk internal error: token qq_MoreVariableDeclarators expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_MemberRest :: L.PosToken -> String
tkVal_qq_MemberRest (L.PosToken _ (L.Tk__qq_MemberRest v)) = v
tkVal_qq_MemberRest t = error ("rtk internal error: token qq_MemberRest expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_MoreTypeSpecifier :: L.PosToken -> String
tkVal_qq_MoreTypeSpecifier (L.PosToken _ (L.Tk__qq_MoreTypeSpecifier v)) = v
tkVal_qq_MoreTypeSpecifier t = error ("rtk internal error: token qq_MoreTypeSpecifier expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_MemberAfterFirstId :: L.PosToken -> String
tkVal_qq_MemberAfterFirstId (L.PosToken _ (L.Tk__qq_MemberAfterFirstId v)) = v
tkVal_qq_MemberAfterFirstId t = error ("rtk internal error: token qq_MemberAfterFirstId expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_PrimitiveTypeKeyword :: L.PosToken -> String
tkVal_qq_PrimitiveTypeKeyword (L.PosToken _ (L.Tk__qq_PrimitiveTypeKeyword v)) = v
tkVal_qq_PrimitiveTypeKeyword t = error ("rtk internal error: token qq_PrimitiveTypeKeyword expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_MemberDeclaration :: L.PosToken -> String
tkVal_qq_MemberDeclaration (L.PosToken _ (L.Tk__qq_MemberDeclaration v)) = v
tkVal_qq_MemberDeclaration t = error ("rtk internal error: token qq_MemberDeclaration expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_NonEmptyDims :: L.PosToken -> String
tkVal_qq_NonEmptyDims (L.PosToken _ (L.Tk__qq_NonEmptyDims v)) = v
tkVal_qq_NonEmptyDims t = error ("rtk internal error: token qq_NonEmptyDims expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Dims :: L.PosToken -> String
tkVal_qq_Dims (L.PosToken _ (L.Tk__qq_Dims v)) = v
tkVal_qq_Dims t = error ("rtk internal error: token qq_Dims expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_FieldDeclaration :: L.PosToken -> String
tkVal_qq_FieldDeclaration (L.PosToken _ (L.Tk__qq_FieldDeclaration v)) = v
tkVal_qq_FieldDeclaration t = error ("rtk internal error: token qq_FieldDeclaration expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TypeDeclRest :: L.PosToken -> String
tkVal_qq_TypeDeclRest (L.PosToken _ (L.Tk__qq_TypeDeclRest v)) = v
tkVal_qq_TypeDeclRest t = error ("rtk internal error: token qq_TypeDeclRest expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_EnumDeclaration :: L.PosToken -> String
tkVal_qq_EnumDeclaration (L.PosToken _ (L.Tk__qq_EnumDeclaration v)) = v
tkVal_qq_EnumDeclaration t = error ("rtk internal error: token qq_EnumDeclaration expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_EnumConstantList :: L.PosToken -> String
tkVal_qq_EnumConstantList (L.PosToken _ (L.Tk__qq_EnumConstantList v)) = v
tkVal_qq_EnumConstantList t = error ("rtk internal error: token qq_EnumConstantList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_EnumConstant :: L.PosToken -> String
tkVal_qq_EnumConstant (L.PosToken _ (L.Tk__qq_EnumConstant v)) = v
tkVal_qq_EnumConstant t = error ("rtk internal error: token qq_EnumConstant expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_AnnotationTypeElement :: L.PosToken -> String
tkVal_qq_AnnotationTypeElement (L.PosToken _ (L.Tk__qq_AnnotationTypeElement v)) = v
tkVal_qq_AnnotationTypeElement t = error ("rtk internal error: token qq_AnnotationTypeElement expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_AnnotationTypeElementList :: L.PosToken -> String
tkVal_qq_AnnotationTypeElementList (L.PosToken _ (L.Tk__qq_AnnotationTypeElementList v)) = v
tkVal_qq_AnnotationTypeElementList t = error ("rtk internal error: token qq_AnnotationTypeElementList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_AnnotationDeclaration :: L.PosToken -> String
tkVal_qq_AnnotationDeclaration (L.PosToken _ (L.Tk__qq_AnnotationDeclaration v)) = v
tkVal_qq_AnnotationDeclaration t = error ("rtk internal error: token qq_AnnotationDeclaration expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_InterfaceDeclaration :: L.PosToken -> String
tkVal_qq_InterfaceDeclaration (L.PosToken _ (L.Tk__qq_InterfaceDeclaration v)) = v
tkVal_qq_InterfaceDeclaration t = error ("rtk internal error: token qq_InterfaceDeclaration expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ClassDeclaration :: L.PosToken -> String
tkVal_qq_ClassDeclaration (L.PosToken _ (L.Tk__qq_ClassDeclaration v)) = v
tkVal_qq_ClassDeclaration t = error ("rtk internal error: token qq_ClassDeclaration expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_FieldDeclarationList :: L.PosToken -> String
tkVal_qq_FieldDeclarationList (L.PosToken _ (L.Tk__qq_FieldDeclarationList v)) = v
tkVal_qq_FieldDeclarationList t = error ("rtk internal error: token qq_FieldDeclarationList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ImplementsList :: L.PosToken -> String
tkVal_qq_ImplementsList (L.PosToken _ (L.Tk__qq_ImplementsList v)) = v
tkVal_qq_ImplementsList t = error ("rtk internal error: token qq_ImplementsList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ExtendsList :: L.PosToken -> String
tkVal_qq_ExtendsList (L.PosToken _ (L.Tk__qq_ExtendsList v)) = v
tkVal_qq_ExtendsList t = error ("rtk internal error: token qq_ExtendsList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ModifierList :: L.PosToken -> String
tkVal_qq_ModifierList (L.PosToken _ (L.Tk__qq_ModifierList v)) = v
tkVal_qq_ModifierList t = error ("rtk internal error: token qq_ModifierList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_AnnotationList :: L.PosToken -> String
tkVal_qq_AnnotationList (L.PosToken _ (L.Tk__qq_AnnotationList v)) = v
tkVal_qq_AnnotationList t = error ("rtk internal error: token qq_AnnotationList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_AnnotationElement :: L.PosToken -> String
tkVal_qq_AnnotationElement (L.PosToken _ (L.Tk__qq_AnnotationElement v)) = v
tkVal_qq_AnnotationElement t = error ("rtk internal error: token qq_AnnotationElement expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_AnnotationArguments :: L.PosToken -> String
tkVal_qq_AnnotationArguments (L.PosToken _ (L.Tk__qq_AnnotationArguments v)) = v
tkVal_qq_AnnotationArguments t = error ("rtk internal error: token qq_AnnotationArguments expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Annotation :: L.PosToken -> String
tkVal_qq_Annotation (L.PosToken _ (L.Tk__qq_Annotation v)) = v
tkVal_qq_Annotation t = error ("rtk internal error: token qq_Annotation expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_DocComment :: L.PosToken -> String
tkVal_qq_DocComment (L.PosToken _ (L.Tk__qq_DocComment v)) = v
tkVal_qq_DocComment t = error ("rtk internal error: token qq_DocComment expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ImportStatement :: L.PosToken -> String
tkVal_qq_ImportStatement (L.PosToken _ (L.Tk__qq_ImportStatement v)) = v
tkVal_qq_ImportStatement t = error ("rtk internal error: token qq_ImportStatement expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Package :: L.PosToken -> String
tkVal_qq_Package (L.PosToken _ (L.Tk__qq_Package v)) = v
tkVal_qq_Package t = error ("rtk internal error: token qq_Package expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_CompilationUnit :: L.PosToken -> String
tkVal_qq_CompilationUnit (L.PosToken _ (L.Tk__qq_CompilationUnit v)) = v
tkVal_qq_CompilationUnit t = error ("rtk internal error: token qq_CompilationUnit expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ImportList :: L.PosToken -> String
tkVal_qq_ImportList (L.PosToken _ (L.Tk__qq_ImportList v)) = v
tkVal_qq_ImportList t = error ("rtk internal error: token qq_ImportList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_TypeDeclaration :: L.PosToken -> String
tkVal_qq_TypeDeclaration (L.PosToken _ (L.Tk__qq_TypeDeclaration v)) = v
tkVal_qq_TypeDeclaration t = error ("rtk internal error: token qq_TypeDeclaration expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptDocComment :: L.PosToken -> String
tkVal_qq_OptDocComment (L.PosToken _ (L.Tk__qq_OptDocComment v)) = v
tkVal_qq_OptDocComment t = error ("rtk internal error: token qq_OptDocComment expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Java :: L.PosToken -> String
tkVal_qq_Java (L.PosToken _ (L.Tk__qq_Java v)) = v
tkVal_qq_Java t = error ("rtk internal error: token qq_Java expected, got " ++ showRtkToken (L.ptToken t))

data Java = Ctr__Java__0 RtkPos Java |
            Ctr__Java__1 RtkPos AdditiveOp |
            Ctr__Java__2 RtkPos Annotation |
            Ctr__Java__3 RtkPos AnnotationArguments |
            Ctr__Java__4 RtkPos AnnotationDeclaration |
            Ctr__Java__5 RtkPos AnnotationElement |
            Ctr__Java__6 RtkPos AnnotationList |
            Ctr__Java__7 RtkPos AnnotationTypeElement |
            Ctr__Java__8 RtkPos AnnotationTypeElementList |
            Ctr__Java__9 RtkPos Arglist |
            Ctr__Java__10 RtkPos AssignmentOp |
            Ctr__Java__11 RtkPos CatchList |
            Ctr__Java__12 RtkPos ClassDeclaration |
            Ctr__Java__13 RtkPos CompilationUnit |
            Ctr__Java__14 RtkPos CompoundName |
            Ctr__Java__15 RtkPos CreationExpression |
            Ctr__Java__16 RtkPos DimExprs |
            Ctr__Java__17 RtkPos Dims |
            Ctr__Java__18 RtkPos DoStatement |
            Ctr__Java__19 RtkPos DocComment |
            Ctr__Java__20 RtkPos EnumConstant |
            Ctr__Java__21 RtkPos EnumConstantList |
            Ctr__Java__22 RtkPos EnumDeclaration |
            Ctr__Java__23 RtkPos EqualityOp |
            Ctr__Java__24 RtkPos Expression |
            Ctr__Java__25 RtkPos ExtendsList |
            Ctr__Java__26 RtkPos FieldDeclaration |
            Ctr__Java__27 RtkPos FieldDeclarationList |
            Ctr__Java__28 RtkPos ForStatement |
            Ctr__Java__29 RtkPos IfStatement |
            Ctr__Java__30 RtkPos ImplementsList |
            Ctr__Java__31 RtkPos ImportList |
            Ctr__Java__32 RtkPos ImportStatement |
            Ctr__Java__33 RtkPos InterfaceDeclaration |
            Ctr__Java__34 RtkPos Literal |
            Ctr__Java__35 RtkPos MemberAfterFirstId |
            Ctr__Java__36 RtkPos MemberDeclaration |
            Ctr__Java__37 RtkPos MemberRest |
            Ctr__Java__38 RtkPos Modifier |
            Ctr__Java__39 RtkPos ModifierList |
            Ctr__Java__40 RtkPos MoreTypeSpecifier |
            Ctr__Java__41 RtkPos MoreVariableDeclarators |
            Ctr__Java__42 RtkPos MultiplicativeOp |
            Ctr__Java__43 RtkPos NonEmptyDims |
            Ctr__Java__44 RtkPos NonEmptyTypeArguments |
            Ctr__Java__45 RtkPos OptDocComment |
            Ctr__Java__46 RtkPos OptElsePart |
            Ctr__Java__47 RtkPos OptExpression |
            Ctr__Java__48 RtkPos OptFinally |
            Ctr__Java__49 RtkPos OptId |
            Ctr__Java__50 RtkPos OptVariableInitializer |
            Ctr__Java__51 RtkPos Package |
            Ctr__Java__52 RtkPos Parameter |
            Ctr__Java__53 RtkPos ParameterList |
            Ctr__Java__54 RtkPos PostfixOp |
            Ctr__Java__55 RtkPos PrefixOp |
            Ctr__Java__56 RtkPos PrimitiveTypeKeyword |
            Ctr__Java__57 RtkPos RelationalOp |
            Ctr__Java__58 RtkPos ShiftOp |
            Ctr__Java__59 RtkPos Statement |
            Ctr__Java__60 RtkPos StatementBlock |
            Ctr__Java__61 RtkPos StatementList |
            Ctr__Java__62 RtkPos StaticInitializer |
            Ctr__Java__63 RtkPos SwitchCaseList |
            Ctr__Java__64 RtkPos SwitchStatement |
            Ctr__Java__65 RtkPos TryStatement |
            Ctr__Java__66 RtkPos Type |
            Ctr__Java__67 RtkPos TypeArgument |
            Ctr__Java__68 RtkPos TypeArguments |
            Ctr__Java__69 RtkPos TypeDeclRest |
            Ctr__Java__70 RtkPos TypeDeclaration |
            Ctr__Java__71 RtkPos TypeParameter |
            Ctr__Java__72 RtkPos TypeParameters |
            Ctr__Java__73 RtkPos TypeSpecifier |
            Ctr__Java__74 RtkPos VariableDeclaration |
            Ctr__Java__75 RtkPos VariableDeclarator |
            Ctr__Java__76 RtkPos VariableDeclaratorList |
            Ctr__Java__77 RtkPos VariableInitializer |
            Ctr__Java__78 RtkPos VariableInitializerList |
            Ctr__Java__79 RtkPos WhileStatement |
            Ctr__Java__80 RtkPos WildcardType |
            Anti_Java String |
            Ctr__Java__81 RtkPos CompilationUnit
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Java where
    rtkPosOf (Ctr__Java__0 p _) = p
    rtkPosOf (Ctr__Java__1 p _) = p
    rtkPosOf (Ctr__Java__2 p _) = p
    rtkPosOf (Ctr__Java__3 p _) = p
    rtkPosOf (Ctr__Java__4 p _) = p
    rtkPosOf (Ctr__Java__5 p _) = p
    rtkPosOf (Ctr__Java__6 p _) = p
    rtkPosOf (Ctr__Java__7 p _) = p
    rtkPosOf (Ctr__Java__8 p _) = p
    rtkPosOf (Ctr__Java__9 p _) = p
    rtkPosOf (Ctr__Java__10 p _) = p
    rtkPosOf (Ctr__Java__11 p _) = p
    rtkPosOf (Ctr__Java__12 p _) = p
    rtkPosOf (Ctr__Java__13 p _) = p
    rtkPosOf (Ctr__Java__14 p _) = p
    rtkPosOf (Ctr__Java__15 p _) = p
    rtkPosOf (Ctr__Java__16 p _) = p
    rtkPosOf (Ctr__Java__17 p _) = p
    rtkPosOf (Ctr__Java__18 p _) = p
    rtkPosOf (Ctr__Java__19 p _) = p
    rtkPosOf (Ctr__Java__20 p _) = p
    rtkPosOf (Ctr__Java__21 p _) = p
    rtkPosOf (Ctr__Java__22 p _) = p
    rtkPosOf (Ctr__Java__23 p _) = p
    rtkPosOf (Ctr__Java__24 p _) = p
    rtkPosOf (Ctr__Java__25 p _) = p
    rtkPosOf (Ctr__Java__26 p _) = p
    rtkPosOf (Ctr__Java__27 p _) = p
    rtkPosOf (Ctr__Java__28 p _) = p
    rtkPosOf (Ctr__Java__29 p _) = p
    rtkPosOf (Ctr__Java__30 p _) = p
    rtkPosOf (Ctr__Java__31 p _) = p
    rtkPosOf (Ctr__Java__32 p _) = p
    rtkPosOf (Ctr__Java__33 p _) = p
    rtkPosOf (Ctr__Java__34 p _) = p
    rtkPosOf (Ctr__Java__35 p _) = p
    rtkPosOf (Ctr__Java__36 p _) = p
    rtkPosOf (Ctr__Java__37 p _) = p
    rtkPosOf (Ctr__Java__38 p _) = p
    rtkPosOf (Ctr__Java__39 p _) = p
    rtkPosOf (Ctr__Java__40 p _) = p
    rtkPosOf (Ctr__Java__41 p _) = p
    rtkPosOf (Ctr__Java__42 p _) = p
    rtkPosOf (Ctr__Java__43 p _) = p
    rtkPosOf (Ctr__Java__44 p _) = p
    rtkPosOf (Ctr__Java__45 p _) = p
    rtkPosOf (Ctr__Java__46 p _) = p
    rtkPosOf (Ctr__Java__47 p _) = p
    rtkPosOf (Ctr__Java__48 p _) = p
    rtkPosOf (Ctr__Java__49 p _) = p
    rtkPosOf (Ctr__Java__50 p _) = p
    rtkPosOf (Ctr__Java__51 p _) = p
    rtkPosOf (Ctr__Java__52 p _) = p
    rtkPosOf (Ctr__Java__53 p _) = p
    rtkPosOf (Ctr__Java__54 p _) = p
    rtkPosOf (Ctr__Java__55 p _) = p
    rtkPosOf (Ctr__Java__56 p _) = p
    rtkPosOf (Ctr__Java__57 p _) = p
    rtkPosOf (Ctr__Java__58 p _) = p
    rtkPosOf (Ctr__Java__59 p _) = p
    rtkPosOf (Ctr__Java__60 p _) = p
    rtkPosOf (Ctr__Java__61 p _) = p
    rtkPosOf (Ctr__Java__62 p _) = p
    rtkPosOf (Ctr__Java__63 p _) = p
    rtkPosOf (Ctr__Java__64 p _) = p
    rtkPosOf (Ctr__Java__65 p _) = p
    rtkPosOf (Ctr__Java__66 p _) = p
    rtkPosOf (Ctr__Java__67 p _) = p
    rtkPosOf (Ctr__Java__68 p _) = p
    rtkPosOf (Ctr__Java__69 p _) = p
    rtkPosOf (Ctr__Java__70 p _) = p
    rtkPosOf (Ctr__Java__71 p _) = p
    rtkPosOf (Ctr__Java__72 p _) = p
    rtkPosOf (Ctr__Java__73 p _) = p
    rtkPosOf (Ctr__Java__74 p _) = p
    rtkPosOf (Ctr__Java__75 p _) = p
    rtkPosOf (Ctr__Java__76 p _) = p
    rtkPosOf (Ctr__Java__77 p _) = p
    rtkPosOf (Ctr__Java__78 p _) = p
    rtkPosOf (Ctr__Java__79 p _) = p
    rtkPosOf (Ctr__Java__80 p _) = p
    rtkPosOf (Anti_Java _) = rtkNoPos
    rtkPosOf (Ctr__Java__81 p _) = p
data AdditiveOp = Anti_AdditiveOp String |
                  Ctr__AdditiveOp__0 RtkPos |
                  Ctr__AdditiveOp__1 RtkPos
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf AdditiveOp where
    rtkPosOf (Anti_AdditiveOp _) = rtkNoPos
    rtkPosOf (Ctr__AdditiveOp__0 p) = p
    rtkPosOf (Ctr__AdditiveOp__1 p) = p
data Annotation = Anti_Annotation String |
                  Ctr__Annotation__1 RtkPos CompoundName Rule_4
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Annotation where
    rtkPosOf (Anti_Annotation _) = rtkNoPos
    rtkPosOf (Ctr__Annotation__1 p _ _) = p
data AnnotationArguments = Anti_AnnotationArguments String |
                           Ctr__AnnotationArguments__0 RtkPos AnnotationElement Rule_7
                           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf AnnotationArguments where
    rtkPosOf (Anti_AnnotationArguments _) = rtkNoPos
    rtkPosOf (Ctr__AnnotationArguments__0 p _ _) = p
data AnnotationDeclaration = Anti_AnnotationDeclaration String |
                             Ctr__AnnotationDeclaration__0 RtkPos String AnnotationTypeElementList
                             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf AnnotationDeclaration where
    rtkPosOf (Anti_AnnotationDeclaration _) = rtkNoPos
    rtkPosOf (Ctr__AnnotationDeclaration__0 p _ _) = p
data AnnotationElement = Anti_AnnotationElement String |
                         Ctr__AnnotationElement__0 RtkPos String Expression |
                         Ctr__AnnotationElement__1 RtkPos Expression
                         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf AnnotationElement where
    rtkPosOf (Anti_AnnotationElement _) = rtkNoPos
    rtkPosOf (Ctr__AnnotationElement__0 p _ _) = p
    rtkPosOf (Ctr__AnnotationElement__1 p _) = p
type AnnotationList = [Annotation]
data AnnotationTypeElement = Anti_AnnotationTypeElement String |
                             Ctr__AnnotationTypeElement__0 RtkPos FieldDeclaration
                             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf AnnotationTypeElement where
    rtkPosOf (Anti_AnnotationTypeElement _) = rtkNoPos
    rtkPosOf (Ctr__AnnotationTypeElement__0 p _) = p
type AnnotationTypeElementList = [AnnotationTypeElement]
data Arglist = Anti_Arglist String |
               Ctr__Arglist__0 RtkPos |
               Ctr__Arglist__1 RtkPos Rule_69
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Arglist where
    rtkPosOf (Anti_Arglist _) = rtkNoPos
    rtkPosOf (Ctr__Arglist__0 p) = p
    rtkPosOf (Ctr__Arglist__1 p _) = p
data AssignmentOp = Anti_AssignmentOp String |
                    Ctr__AssignmentOp__0 RtkPos |
                    Ctr__AssignmentOp__1 RtkPos |
                    Ctr__AssignmentOp__2 RtkPos |
                    Ctr__AssignmentOp__3 RtkPos |
                    Ctr__AssignmentOp__4 RtkPos |
                    Ctr__AssignmentOp__5 RtkPos |
                    Ctr__AssignmentOp__6 RtkPos |
                    Ctr__AssignmentOp__7 RtkPos |
                    Ctr__AssignmentOp__8 RtkPos |
                    Ctr__AssignmentOp__9 RtkPos |
                    Ctr__AssignmentOp__10 RtkPos |
                    Ctr__AssignmentOp__11 RtkPos
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf AssignmentOp where
    rtkPosOf (Anti_AssignmentOp _) = rtkNoPos
    rtkPosOf (Ctr__AssignmentOp__0 p) = p
    rtkPosOf (Ctr__AssignmentOp__1 p) = p
    rtkPosOf (Ctr__AssignmentOp__2 p) = p
    rtkPosOf (Ctr__AssignmentOp__3 p) = p
    rtkPosOf (Ctr__AssignmentOp__4 p) = p
    rtkPosOf (Ctr__AssignmentOp__5 p) = p
    rtkPosOf (Ctr__AssignmentOp__6 p) = p
    rtkPosOf (Ctr__AssignmentOp__7 p) = p
    rtkPosOf (Ctr__AssignmentOp__8 p) = p
    rtkPosOf (Ctr__AssignmentOp__9 p) = p
    rtkPosOf (Ctr__AssignmentOp__10 p) = p
    rtkPosOf (Ctr__AssignmentOp__11 p) = p
type CatchList = [Rule_54]
data ClassDeclaration = Anti_ClassDeclaration String |
                        Ctr__ClassDeclaration__0 RtkPos String TypeParameters Rule_16 Rule_17 FieldDeclarationList
                        deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ClassDeclaration where
    rtkPosOf (Anti_ClassDeclaration _) = rtkNoPos
    rtkPosOf (Ctr__ClassDeclaration__0 p _ _ _ _ _) = p
data CompilationUnit = Anti_CompilationUnit String |
                       Ctr__CompilationUnit__0 RtkPos Rule_1 ImportList Rule_2
                       deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf CompilationUnit where
    rtkPosOf (Anti_CompilationUnit _) = rtkNoPos
    rtkPosOf (Ctr__CompilationUnit__0 p _ _ _) = p
data CompoundName = Anti_CompoundName String |
                    Ctr__CompoundName__0 RtkPos String Rule_81
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf CompoundName where
    rtkPosOf (Anti_CompoundName _) = rtkNoPos
    rtkPosOf (Ctr__CompoundName__0 p _ _) = p
data CreationExpression = Anti_CreationExpression String |
                          Ctr__CreationExpression__0 RtkPos TypeSpecifier Rule_65
                          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf CreationExpression where
    rtkPosOf (Anti_CreationExpression _) = rtkNoPos
    rtkPosOf (Ctr__CreationExpression__0 p _ _) = p
type DimExprs = [Rule_67]
type Dims = [Rule_31]
data DoStatement = Anti_DoStatement String |
                   Ctr__DoStatement__0 RtkPos Statement Expression
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf DoStatement where
    rtkPosOf (Anti_DoStatement _) = rtkNoPos
    rtkPosOf (Ctr__DoStatement__0 p _ _) = p
data DocComment = Anti_DocComment String |
                  Ctr__DocComment__0 RtkPos String
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf DocComment where
    rtkPosOf (Anti_DocComment _) = rtkNoPos
    rtkPosOf (Ctr__DocComment__0 p _) = p
data EnumConstant = Anti_EnumConstant String |
                    Ctr__EnumConstant__0 RtkPos AnnotationList String Rule_20 Rule_22
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf EnumConstant where
    rtkPosOf (Anti_EnumConstant _) = rtkNoPos
    rtkPosOf (Ctr__EnumConstant__0 p _ _ _ _) = p
data EnumConstantList = Anti_EnumConstantList String |
                        Ctr__EnumConstantList__0 RtkPos EnumConstant Rule_24 Rule_26
                        deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf EnumConstantList where
    rtkPosOf (Anti_EnumConstantList _) = rtkNoPos
    rtkPosOf (Ctr__EnumConstantList__0 p _ _ _) = p
data EnumDeclaration = Anti_EnumDeclaration String |
                       Ctr__EnumDeclaration__0 RtkPos String Rule_27 EnumConstantList Rule_28
                       deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf EnumDeclaration where
    rtkPosOf (Anti_EnumDeclaration _) = rtkNoPos
    rtkPosOf (Ctr__EnumDeclaration__0 p _ _ _ _) = p
data EqualityOp = Anti_EqualityOp String |
                  Ctr__EqualityOp__0 RtkPos |
                  Ctr__EqualityOp__1 RtkPos
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf EqualityOp where
    rtkPosOf (Anti_EqualityOp _) = rtkNoPos
    rtkPosOf (Ctr__EqualityOp__0 p) = p
    rtkPosOf (Ctr__EqualityOp__1 p) = p
data Expression = Anti_Expression String |
                  Ctr__Expression__0 RtkPos Literal |
                  Ctr__Expression__1 RtkPos |
                  Ctr__Expression__2 RtkPos Expression |
                  Ctr__Expression__3 RtkPos CreationExpression |
                  Ctr__Expression__4 RtkPos CompoundName Rule_61 |
                  Ctr__Expression__5 RtkPos CompoundName Expression |
                  Ctr__Expression__6 RtkPos String Rule_63 |
                  Ctr__Expression__7 RtkPos Expression |
                  Ctr__Expression__8 RtkPos Expression PostfixOp |
                  Ctr__Expression__9 RtkPos Expression String |
                  Ctr__Expression__10 RtkPos Expression String Arglist |
                  Ctr__Expression__11 RtkPos Expression Expression |
                  Ctr__Expression__12 RtkPos Expression |
                  Ctr__Expression__13 RtkPos Expression |
                  Ctr__Expression__14 RtkPos Expression |
                  Ctr__Expression__15 RtkPos Expression |
                  Ctr__Expression__16 RtkPos PrefixOp Expression |
                  Ctr__Expression__17 RtkPos Expression |
                  Ctr__Expression__18 RtkPos PrimitiveTypeKeyword Dims Expression |
                  Ctr__Expression__19 RtkPos CompoundName NonEmptyTypeArguments Dims Expression |
                  Ctr__Expression__20 RtkPos CompoundName NonEmptyDims Expression |
                  Ctr__Expression__21 RtkPos Expression Expression |
                  Ctr__Expression__22 RtkPos Expression |
                  Ctr__Expression__23 RtkPos Expression MultiplicativeOp Expression |
                  Ctr__Expression__24 RtkPos Expression |
                  Ctr__Expression__25 RtkPos Expression AdditiveOp Expression |
                  Ctr__Expression__26 RtkPos Expression |
                  Ctr__Expression__27 RtkPos Expression ShiftOp Expression |
                  Ctr__Expression__28 RtkPos Expression |
                  Ctr__Expression__29 RtkPos Expression RelationalOp Expression |
                  Ctr__Expression__30 RtkPos Expression Type |
                  Ctr__Expression__31 RtkPos Expression |
                  Ctr__Expression__32 RtkPos Expression EqualityOp Expression |
                  Ctr__Expression__33 RtkPos Expression |
                  Ctr__Expression__34 RtkPos Expression Expression |
                  Ctr__Expression__35 RtkPos Expression |
                  Ctr__Expression__36 RtkPos Expression Expression |
                  Ctr__Expression__37 RtkPos Expression |
                  Ctr__Expression__38 RtkPos Expression Expression |
                  Ctr__Expression__39 RtkPos Expression |
                  Ctr__Expression__40 RtkPos Expression Expression |
                  Ctr__Expression__41 RtkPos Expression |
                  Ctr__Expression__42 RtkPos Expression Expression |
                  Ctr__Expression__43 RtkPos Expression |
                  Ctr__Expression__44 RtkPos Expression Expression Expression |
                  Ctr__Expression__45 RtkPos Expression Rule_59 |
                  Ctr__Expression__46 RtkPos Expression
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Expression where
    rtkPosOf (Anti_Expression _) = rtkNoPos
    rtkPosOf (Ctr__Expression__0 p _) = p
    rtkPosOf (Ctr__Expression__1 p) = p
    rtkPosOf (Ctr__Expression__2 p _) = p
    rtkPosOf (Ctr__Expression__3 p _) = p
    rtkPosOf (Ctr__Expression__4 p _ _) = p
    rtkPosOf (Ctr__Expression__5 p _ _) = p
    rtkPosOf (Ctr__Expression__6 p _ _) = p
    rtkPosOf (Ctr__Expression__7 p _) = p
    rtkPosOf (Ctr__Expression__8 p _ _) = p
    rtkPosOf (Ctr__Expression__9 p _ _) = p
    rtkPosOf (Ctr__Expression__10 p _ _ _) = p
    rtkPosOf (Ctr__Expression__11 p _ _) = p
    rtkPosOf (Ctr__Expression__12 p _) = p
    rtkPosOf (Ctr__Expression__13 p _) = p
    rtkPosOf (Ctr__Expression__14 p _) = p
    rtkPosOf (Ctr__Expression__15 p _) = p
    rtkPosOf (Ctr__Expression__16 p _ _) = p
    rtkPosOf (Ctr__Expression__17 p _) = p
    rtkPosOf (Ctr__Expression__18 p _ _ _) = p
    rtkPosOf (Ctr__Expression__19 p _ _ _ _) = p
    rtkPosOf (Ctr__Expression__20 p _ _ _) = p
    rtkPosOf (Ctr__Expression__21 p _ _) = p
    rtkPosOf (Ctr__Expression__22 p _) = p
    rtkPosOf (Ctr__Expression__23 p _ _ _) = p
    rtkPosOf (Ctr__Expression__24 p _) = p
    rtkPosOf (Ctr__Expression__25 p _ _ _) = p
    rtkPosOf (Ctr__Expression__26 p _) = p
    rtkPosOf (Ctr__Expression__27 p _ _ _) = p
    rtkPosOf (Ctr__Expression__28 p _) = p
    rtkPosOf (Ctr__Expression__29 p _ _ _) = p
    rtkPosOf (Ctr__Expression__30 p _ _) = p
    rtkPosOf (Ctr__Expression__31 p _) = p
    rtkPosOf (Ctr__Expression__32 p _ _ _) = p
    rtkPosOf (Ctr__Expression__33 p _) = p
    rtkPosOf (Ctr__Expression__34 p _ _) = p
    rtkPosOf (Ctr__Expression__35 p _) = p
    rtkPosOf (Ctr__Expression__36 p _ _) = p
    rtkPosOf (Ctr__Expression__37 p _) = p
    rtkPosOf (Ctr__Expression__38 p _ _) = p
    rtkPosOf (Ctr__Expression__39 p _) = p
    rtkPosOf (Ctr__Expression__40 p _ _) = p
    rtkPosOf (Ctr__Expression__41 p _) = p
    rtkPosOf (Ctr__Expression__42 p _ _) = p
    rtkPosOf (Ctr__Expression__43 p _) = p
    rtkPosOf (Ctr__Expression__44 p _ _ _) = p
    rtkPosOf (Ctr__Expression__45 p _ _) = p
    rtkPosOf (Ctr__Expression__46 p _) = p
data ExtendsList = Anti_ExtendsList String |
                   Ctr__ExtendsList__0 RtkPos CompoundName Rule_12
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ExtendsList where
    rtkPosOf (Anti_ExtendsList _) = rtkNoPos
    rtkPosOf (Ctr__ExtendsList__0 p _ _) = p
data FieldDeclaration = Anti_FieldDeclaration String |
                        Ctr__FieldDeclaration__0 RtkPos OptDocComment ModifierList Rule_30 |
                        Ctr__FieldDeclaration__1 RtkPos
                        deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf FieldDeclaration where
    rtkPosOf (Anti_FieldDeclaration _) = rtkNoPos
    rtkPosOf (Ctr__FieldDeclaration__0 p _ _ _) = p
    rtkPosOf (Ctr__FieldDeclaration__1 p) = p
type FieldDeclarationList = [FieldDeclaration]
data ForStatement = Anti_ForStatement String |
                    Ctr__ForStatement__0 RtkPos Rule_53 OptExpression OptExpression Statement
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ForStatement where
    rtkPosOf (Anti_ForStatement _) = rtkNoPos
    rtkPosOf (Ctr__ForStatement__0 p _ _ _ _) = p
data IfStatement = Anti_IfStatement String |
                   Ctr__IfStatement__0 RtkPos Expression Statement OptElsePart
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf IfStatement where
    rtkPosOf (Anti_IfStatement _) = rtkNoPos
    rtkPosOf (Ctr__IfStatement__0 p _ _ _) = p
data ImplementsList = Anti_ImplementsList String |
                      Ctr__ImplementsList__0 RtkPos Rule_14
                      deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ImplementsList where
    rtkPosOf (Anti_ImplementsList _) = rtkNoPos
    rtkPosOf (Ctr__ImplementsList__0 p _) = p
type ImportList = [ImportStatement]
data ImportStatement = Anti_ImportStatement String |
                       Ctr__ImportStatement__0 RtkPos Rule_3
                       deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ImportStatement where
    rtkPosOf (Anti_ImportStatement _) = rtkNoPos
    rtkPosOf (Ctr__ImportStatement__0 p _) = p
data InterfaceDeclaration = Anti_InterfaceDeclaration String |
                            Ctr__InterfaceDeclaration__0 RtkPos String TypeParameters Rule_18 FieldDeclarationList
                            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf InterfaceDeclaration where
    rtkPosOf (Anti_InterfaceDeclaration _) = rtkNoPos
    rtkPosOf (Ctr__InterfaceDeclaration__0 p _ _ _ _) = p
data Literal = Anti_Literal String |
               Ctr__Literal__0 RtkPos String |
               Ctr__Literal__1 RtkPos String |
               Ctr__Literal__2 RtkPos |
               Ctr__Literal__3 RtkPos |
               Ctr__Literal__4 RtkPos String |
               Ctr__Literal__5 RtkPos String |
               Ctr__Literal__6 RtkPos
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Literal where
    rtkPosOf (Anti_Literal _) = rtkNoPos
    rtkPosOf (Ctr__Literal__0 p _) = p
    rtkPosOf (Ctr__Literal__1 p _) = p
    rtkPosOf (Ctr__Literal__2 p) = p
    rtkPosOf (Ctr__Literal__3 p) = p
    rtkPosOf (Ctr__Literal__4 p _) = p
    rtkPosOf (Ctr__Literal__5 p _) = p
    rtkPosOf (Ctr__Literal__6 p) = p
data MemberAfterFirstId = Anti_MemberAfterFirstId String |
                          Ctr__MemberAfterFirstId__0 RtkPos Rule_35 StatementBlock |
                          Ctr__MemberAfterFirstId__1 RtkPos MoreTypeSpecifier String MemberRest
                          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf MemberAfterFirstId where
    rtkPosOf (Anti_MemberAfterFirstId _) = rtkNoPos
    rtkPosOf (Ctr__MemberAfterFirstId__0 p _ _) = p
    rtkPosOf (Ctr__MemberAfterFirstId__1 p _ _ _) = p
data MemberDeclaration = Anti_MemberDeclaration String |
                         Ctr__MemberDeclaration__0 RtkPos PrimitiveTypeKeyword Dims String MemberRest |
                         Ctr__MemberDeclaration__1 RtkPos TypeParameters String MoreTypeSpecifier String MemberRest |
                         Ctr__MemberDeclaration__2 RtkPos String MemberAfterFirstId
                         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf MemberDeclaration where
    rtkPosOf (Anti_MemberDeclaration _) = rtkNoPos
    rtkPosOf (Ctr__MemberDeclaration__0 p _ _ _ _) = p
    rtkPosOf (Ctr__MemberDeclaration__1 p _ _ _ _ _) = p
    rtkPosOf (Ctr__MemberDeclaration__2 p _ _) = p
data MemberRest = Anti_MemberRest String |
                  Ctr__MemberRest__0 RtkPos Rule_36 Dims Rule_37 |
                  Ctr__MemberRest__1 RtkPos Dims OptVariableInitializer MoreVariableDeclarators
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf MemberRest where
    rtkPosOf (Anti_MemberRest _) = rtkNoPos
    rtkPosOf (Ctr__MemberRest__0 p _ _ _) = p
    rtkPosOf (Ctr__MemberRest__1 p _ _ _) = p
data Modifier = Anti_Modifier String |
                Ctr__Modifier__0 RtkPos |
                Ctr__Modifier__1 RtkPos |
                Ctr__Modifier__2 RtkPos |
                Ctr__Modifier__3 RtkPos |
                Ctr__Modifier__4 RtkPos |
                Ctr__Modifier__5 RtkPos |
                Ctr__Modifier__6 RtkPos |
                Ctr__Modifier__7 RtkPos |
                Ctr__Modifier__8 RtkPos |
                Ctr__Modifier__9 RtkPos
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Modifier where
    rtkPosOf (Anti_Modifier _) = rtkNoPos
    rtkPosOf (Ctr__Modifier__0 p) = p
    rtkPosOf (Ctr__Modifier__1 p) = p
    rtkPosOf (Ctr__Modifier__2 p) = p
    rtkPosOf (Ctr__Modifier__3 p) = p
    rtkPosOf (Ctr__Modifier__4 p) = p
    rtkPosOf (Ctr__Modifier__5 p) = p
    rtkPosOf (Ctr__Modifier__6 p) = p
    rtkPosOf (Ctr__Modifier__7 p) = p
    rtkPosOf (Ctr__Modifier__8 p) = p
    rtkPosOf (Ctr__Modifier__9 p) = p
type ModifierList = [Rule_10]
data MoreTypeSpecifier = Anti_MoreTypeSpecifier String |
                         Ctr__MoreTypeSpecifier__0 RtkPos String MoreTypeSpecifier |
                         Ctr__MoreTypeSpecifier__1 RtkPos TypeArguments Dims
                         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf MoreTypeSpecifier where
    rtkPosOf (Anti_MoreTypeSpecifier _) = rtkNoPos
    rtkPosOf (Ctr__MoreTypeSpecifier__0 p _ _) = p
    rtkPosOf (Ctr__MoreTypeSpecifier__1 p _ _) = p
type MoreVariableDeclarators = [Rule_40]
data MultiplicativeOp = Anti_MultiplicativeOp String |
                        Ctr__MultiplicativeOp__0 RtkPos |
                        Ctr__MultiplicativeOp__1 RtkPos |
                        Ctr__MultiplicativeOp__2 RtkPos
                        deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf MultiplicativeOp where
    rtkPosOf (Anti_MultiplicativeOp _) = rtkNoPos
    rtkPosOf (Ctr__MultiplicativeOp__0 p) = p
    rtkPosOf (Ctr__MultiplicativeOp__1 p) = p
    rtkPosOf (Ctr__MultiplicativeOp__2 p) = p
type NonEmptyDims = [Rule_33]
data NonEmptyTypeArguments = Anti_NonEmptyTypeArguments String |
                             Ctr__NonEmptyTypeArguments__0 RtkPos TypeArgument Rule_72
                             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf NonEmptyTypeArguments where
    rtkPosOf (Anti_NonEmptyTypeArguments _) = rtkNoPos
    rtkPosOf (Ctr__NonEmptyTypeArguments__0 p _ _) = p
data OptDocComment = Anti_OptDocComment String |
                     Ctr__OptDocComment__0 RtkPos |
                     Ctr__OptDocComment__1 RtkPos DocComment
                     deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptDocComment where
    rtkPosOf (Anti_OptDocComment _) = rtkNoPos
    rtkPosOf (Ctr__OptDocComment__0 p) = p
    rtkPosOf (Ctr__OptDocComment__1 p _) = p
data OptElsePart = Anti_OptElsePart String |
                   Ctr__OptElsePart__0 RtkPos |
                   Ctr__OptElsePart__1 RtkPos Rule_52
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptElsePart where
    rtkPosOf (Anti_OptElsePart _) = rtkNoPos
    rtkPosOf (Ctr__OptElsePart__0 p) = p
    rtkPosOf (Ctr__OptElsePart__1 p _) = p
data OptExpression = Anti_OptExpression String |
                     Ctr__OptExpression__0 RtkPos |
                     Ctr__OptExpression__1 RtkPos Expression
                     deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptExpression where
    rtkPosOf (Anti_OptExpression _) = rtkNoPos
    rtkPosOf (Ctr__OptExpression__0 p) = p
    rtkPosOf (Ctr__OptExpression__1 p _) = p
data OptFinally = Anti_OptFinally String |
                  Ctr__OptFinally__0 RtkPos |
                  Ctr__OptFinally__1 RtkPos Rule_56
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptFinally where
    rtkPosOf (Anti_OptFinally _) = rtkNoPos
    rtkPosOf (Ctr__OptFinally__0 p) = p
    rtkPosOf (Ctr__OptFinally__1 p _) = p
data OptId = Anti_OptId String |
             Ctr__OptId__0 RtkPos |
             Ctr__OptId__1 RtkPos String
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptId where
    rtkPosOf (Anti_OptId _) = rtkNoPos
    rtkPosOf (Ctr__OptId__0 p) = p
    rtkPosOf (Ctr__OptId__1 p _) = p
data OptVariableInitializer = Anti_OptVariableInitializer String |
                              Ctr__OptVariableInitializer__0 RtkPos |
                              Ctr__OptVariableInitializer__1 RtkPos Rule_44
                              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptVariableInitializer where
    rtkPosOf (Anti_OptVariableInitializer _) = rtkNoPos
    rtkPosOf (Ctr__OptVariableInitializer__0 p) = p
    rtkPosOf (Ctr__OptVariableInitializer__1 p _) = p
data Package = Anti_Package String |
               Ctr__Package__0 RtkPos CompoundName
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Package where
    rtkPosOf (Anti_Package _) = rtkNoPos
    rtkPosOf (Ctr__Package__0 p _) = p
data Parameter = Anti_Parameter String |
                 Ctr__Parameter__0 RtkPos Type String Dims
                 deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Parameter where
    rtkPosOf (Anti_Parameter _) = rtkNoPos
    rtkPosOf (Ctr__Parameter__0 p _ _ _) = p
data ParameterList = Anti_ParameterList String |
                     Ctr__ParameterList__0 RtkPos Parameter Rule_49
                     deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ParameterList where
    rtkPosOf (Anti_ParameterList _) = rtkNoPos
    rtkPosOf (Ctr__ParameterList__0 p _ _) = p
data PostfixOp = Anti_PostfixOp String |
                 Ctr__PostfixOp__0 RtkPos |
                 Ctr__PostfixOp__1 RtkPos
                 deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf PostfixOp where
    rtkPosOf (Anti_PostfixOp _) = rtkNoPos
    rtkPosOf (Ctr__PostfixOp__0 p) = p
    rtkPosOf (Ctr__PostfixOp__1 p) = p
data PrefixOp = Anti_PrefixOp String |
                Ctr__PrefixOp__0 RtkPos |
                Ctr__PrefixOp__1 RtkPos |
                Ctr__PrefixOp__2 RtkPos |
                Ctr__PrefixOp__3 RtkPos
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf PrefixOp where
    rtkPosOf (Anti_PrefixOp _) = rtkNoPos
    rtkPosOf (Ctr__PrefixOp__0 p) = p
    rtkPosOf (Ctr__PrefixOp__1 p) = p
    rtkPosOf (Ctr__PrefixOp__2 p) = p
    rtkPosOf (Ctr__PrefixOp__3 p) = p
data PrimitiveTypeKeyword = Anti_PrimitiveTypeKeyword String |
                            Ctr__PrimitiveTypeKeyword__0 RtkPos |
                            Ctr__PrimitiveTypeKeyword__1 RtkPos |
                            Ctr__PrimitiveTypeKeyword__2 RtkPos |
                            Ctr__PrimitiveTypeKeyword__3 RtkPos |
                            Ctr__PrimitiveTypeKeyword__4 RtkPos |
                            Ctr__PrimitiveTypeKeyword__5 RtkPos |
                            Ctr__PrimitiveTypeKeyword__6 RtkPos |
                            Ctr__PrimitiveTypeKeyword__7 RtkPos |
                            Ctr__PrimitiveTypeKeyword__8 RtkPos
                            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf PrimitiveTypeKeyword where
    rtkPosOf (Anti_PrimitiveTypeKeyword _) = rtkNoPos
    rtkPosOf (Ctr__PrimitiveTypeKeyword__0 p) = p
    rtkPosOf (Ctr__PrimitiveTypeKeyword__1 p) = p
    rtkPosOf (Ctr__PrimitiveTypeKeyword__2 p) = p
    rtkPosOf (Ctr__PrimitiveTypeKeyword__3 p) = p
    rtkPosOf (Ctr__PrimitiveTypeKeyword__4 p) = p
    rtkPosOf (Ctr__PrimitiveTypeKeyword__5 p) = p
    rtkPosOf (Ctr__PrimitiveTypeKeyword__6 p) = p
    rtkPosOf (Ctr__PrimitiveTypeKeyword__7 p) = p
    rtkPosOf (Ctr__PrimitiveTypeKeyword__8 p) = p
data RelationalOp = Anti_RelationalOp String |
                    Ctr__RelationalOp__0 RtkPos |
                    Ctr__RelationalOp__1 RtkPos |
                    Ctr__RelationalOp__2 RtkPos |
                    Ctr__RelationalOp__3 RtkPos
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf RelationalOp where
    rtkPosOf (Anti_RelationalOp _) = rtkNoPos
    rtkPosOf (Ctr__RelationalOp__0 p) = p
    rtkPosOf (Ctr__RelationalOp__1 p) = p
    rtkPosOf (Ctr__RelationalOp__2 p) = p
    rtkPosOf (Ctr__RelationalOp__3 p) = p
data Rule_1 = Ctr__Rule_1__0 RtkPos |
              Ctr__Rule_1__1 RtkPos Package
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_1 where
    rtkPosOf (Ctr__Rule_1__0 p) = p
    rtkPosOf (Ctr__Rule_1__1 p _) = p
data Rule_10 = Anti_Rule_10 String |
               Ctr__Rule_10__1 RtkPos Modifier |
               Ctr__Rule_10__2 RtkPos Annotation
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_10 where
    rtkPosOf (Anti_Rule_10 _) = rtkNoPos
    rtkPosOf (Ctr__Rule_10__1 p _) = p
    rtkPosOf (Ctr__Rule_10__2 p _) = p
type Rule_12 = [Rule_13]
data Rule_13 = Ctr__Rule_13__0 RtkPos CompoundName
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_13 where
    rtkPosOf (Ctr__Rule_13__0 p _) = p
type Rule_14 = [CompoundName]
data Rule_16 = Ctr__Rule_16__0 RtkPos |
               Ctr__Rule_16__1 RtkPos ExtendsList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_16 where
    rtkPosOf (Ctr__Rule_16__0 p) = p
    rtkPosOf (Ctr__Rule_16__1 p _) = p
data Rule_17 = Ctr__Rule_17__0 RtkPos |
               Ctr__Rule_17__1 RtkPos ImplementsList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_17 where
    rtkPosOf (Ctr__Rule_17__0 p) = p
    rtkPosOf (Ctr__Rule_17__1 p _) = p
data Rule_18 = Ctr__Rule_18__0 RtkPos |
               Ctr__Rule_18__1 RtkPos ExtendsList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_18 where
    rtkPosOf (Ctr__Rule_18__0 p) = p
    rtkPosOf (Ctr__Rule_18__1 p _) = p
data Rule_2 = Ctr__Rule_2__0 RtkPos |
              Ctr__Rule_2__1 RtkPos TypeDeclaration
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_2 where
    rtkPosOf (Ctr__Rule_2__0 p) = p
    rtkPosOf (Ctr__Rule_2__1 p _) = p
data Rule_20 = Ctr__Rule_20__0 RtkPos |
               Ctr__Rule_20__1 RtkPos Rule_21
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_20 where
    rtkPosOf (Ctr__Rule_20__0 p) = p
    rtkPosOf (Ctr__Rule_20__1 p _) = p
data Rule_21 = Ctr__Rule_21__0 RtkPos Arglist
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_21 where
    rtkPosOf (Ctr__Rule_21__0 p _) = p
data Rule_22 = Ctr__Rule_22__0 RtkPos |
               Ctr__Rule_22__1 RtkPos Rule_23
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_22 where
    rtkPosOf (Ctr__Rule_22__0 p) = p
    rtkPosOf (Ctr__Rule_22__1 p _) = p
data Rule_23 = Ctr__Rule_23__0 RtkPos FieldDeclarationList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_23 where
    rtkPosOf (Ctr__Rule_23__0 p _) = p
type Rule_24 = [Rule_25]
data Rule_25 = Ctr__Rule_25__0 RtkPos EnumConstant
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_25 where
    rtkPosOf (Ctr__Rule_25__0 p _) = p
data Rule_26 = Ctr__Rule_26__0 RtkPos |
               Ctr__Rule_26__1 RtkPos
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_26 where
    rtkPosOf (Ctr__Rule_26__0 p) = p
    rtkPosOf (Ctr__Rule_26__1 p) = p
data Rule_27 = Ctr__Rule_27__0 RtkPos |
               Ctr__Rule_27__1 RtkPos ImplementsList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_27 where
    rtkPosOf (Ctr__Rule_27__0 p) = p
    rtkPosOf (Ctr__Rule_27__1 p _) = p
data Rule_28 = Ctr__Rule_28__0 RtkPos |
               Ctr__Rule_28__1 RtkPos Rule_29
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_28 where
    rtkPosOf (Ctr__Rule_28__0 p) = p
    rtkPosOf (Ctr__Rule_28__1 p _) = p
data Rule_29 = Ctr__Rule_29__0 RtkPos FieldDeclarationList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_29 where
    rtkPosOf (Ctr__Rule_29__0 p _) = p
data Rule_3 = Ctr__Rule_3__0 RtkPos CompoundName |
              Ctr__Rule_3__1 RtkPos CompoundName
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_3 where
    rtkPosOf (Ctr__Rule_3__0 p _) = p
    rtkPosOf (Ctr__Rule_3__1 p _) = p
data Rule_30 = Ctr__Rule_30__0 RtkPos MemberDeclaration |
               Ctr__Rule_30__1 RtkPos TypeDeclRest |
               Ctr__Rule_30__2 RtkPos StaticInitializer
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_30 where
    rtkPosOf (Ctr__Rule_30__0 p _) = p
    rtkPosOf (Ctr__Rule_30__1 p _) = p
    rtkPosOf (Ctr__Rule_30__2 p _) = p
data Rule_31 = Anti_Rule_31 String |
               Ctr__Rule_31__1 RtkPos
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_31 where
    rtkPosOf (Anti_Rule_31 _) = rtkNoPos
    rtkPosOf (Ctr__Rule_31__1 p) = p
data Rule_33 = Anti_Rule_33 String |
               Ctr__Rule_33__1 RtkPos
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_33 where
    rtkPosOf (Anti_Rule_33 _) = rtkNoPos
    rtkPosOf (Ctr__Rule_33__1 p) = p
data Rule_35 = Ctr__Rule_35__0 RtkPos |
               Ctr__Rule_35__1 RtkPos ParameterList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_35 where
    rtkPosOf (Ctr__Rule_35__0 p) = p
    rtkPosOf (Ctr__Rule_35__1 p _) = p
data Rule_36 = Ctr__Rule_36__0 RtkPos |
               Ctr__Rule_36__1 RtkPos ParameterList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_36 where
    rtkPosOf (Ctr__Rule_36__0 p) = p
    rtkPosOf (Ctr__Rule_36__1 p _) = p
data Rule_37 = Ctr__Rule_37__0 RtkPos StatementBlock |
               Ctr__Rule_37__1 RtkPos Rule_38
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_37 where
    rtkPosOf (Ctr__Rule_37__0 p _) = p
    rtkPosOf (Ctr__Rule_37__1 p _) = p
data Rule_38 = Ctr__Rule_38__0 RtkPos |
               Ctr__Rule_38__1 RtkPos Rule_39
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_38 where
    rtkPosOf (Ctr__Rule_38__0 p) = p
    rtkPosOf (Ctr__Rule_38__1 p _) = p
data Rule_39 = Ctr__Rule_39__0 RtkPos Expression
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_39 where
    rtkPosOf (Ctr__Rule_39__0 p _) = p
data Rule_4 = Ctr__Rule_4__0 RtkPos |
              Ctr__Rule_4__1 RtkPos Rule_5
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_4 where
    rtkPosOf (Ctr__Rule_4__0 p) = p
    rtkPosOf (Ctr__Rule_4__1 p _) = p
data Rule_40 = Anti_Rule_40 String |
               Ctr__Rule_40__1 RtkPos VariableDeclarator
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_40 where
    rtkPosOf (Anti_Rule_40 _) = rtkNoPos
    rtkPosOf (Ctr__Rule_40__1 p _) = p
type Rule_42 = [Rule_43]
data Rule_43 = Ctr__Rule_43__0 RtkPos VariableDeclarator
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_43 where
    rtkPosOf (Ctr__Rule_43__0 p _) = p
data Rule_44 = Ctr__Rule_44__0 RtkPos VariableInitializer
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_44 where
    rtkPosOf (Ctr__Rule_44__0 p _) = p
data Rule_45 = Ctr__Rule_45__0 RtkPos VariableInitializer Rule_46 Rule_48
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_45 where
    rtkPosOf (Ctr__Rule_45__0 p _ _ _) = p
type Rule_46 = [Rule_47]
data Rule_47 = Ctr__Rule_47__0 RtkPos VariableInitializer
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_47 where
    rtkPosOf (Ctr__Rule_47__0 p _) = p
data Rule_48 = Ctr__Rule_48__0 RtkPos |
               Ctr__Rule_48__1 RtkPos
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_48 where
    rtkPosOf (Ctr__Rule_48__0 p) = p
    rtkPosOf (Ctr__Rule_48__1 p) = p
type Rule_49 = [Rule_50]
data Rule_5 = Ctr__Rule_5__0 RtkPos Rule_6
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_5 where
    rtkPosOf (Ctr__Rule_5__0 p _) = p
data Rule_50 = Ctr__Rule_50__0 RtkPos Parameter
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_50 where
    rtkPosOf (Ctr__Rule_50__0 p _) = p
data Rule_52 = Ctr__Rule_52__0 RtkPos Statement
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_52 where
    rtkPosOf (Ctr__Rule_52__0 p _) = p
data Rule_53 = Ctr__Rule_53__0 RtkPos VariableDeclaration |
               Ctr__Rule_53__1 RtkPos Expression |
               Ctr__Rule_53__2 RtkPos
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_53 where
    rtkPosOf (Ctr__Rule_53__0 p _) = p
    rtkPosOf (Ctr__Rule_53__1 p _) = p
    rtkPosOf (Ctr__Rule_53__2 p) = p
data Rule_54 = Anti_Rule_54 String |
               Ctr__Rule_54__1 RtkPos Parameter Statement
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_54 where
    rtkPosOf (Anti_Rule_54 _) = rtkNoPos
    rtkPosOf (Ctr__Rule_54__1 p _ _) = p
data Rule_56 = Ctr__Rule_56__0 RtkPos Statement
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_56 where
    rtkPosOf (Ctr__Rule_56__0 p _) = p
data Rule_57 = Anti_Rule_57 String |
               Ctr__Rule_57__1 RtkPos Expression |
               Ctr__Rule_57__2 RtkPos |
               Ctr__Rule_57__3 RtkPos Statement
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_57 where
    rtkPosOf (Anti_Rule_57 _) = rtkNoPos
    rtkPosOf (Ctr__Rule_57__1 p _) = p
    rtkPosOf (Ctr__Rule_57__2 p) = p
    rtkPosOf (Ctr__Rule_57__3 p _) = p
data Rule_59 = Ctr__Rule_59__0 RtkPos |
               Ctr__Rule_59__1 RtkPos Rule_60
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_59 where
    rtkPosOf (Ctr__Rule_59__0 p) = p
    rtkPosOf (Ctr__Rule_59__1 p _) = p
data Rule_6 = Ctr__Rule_6__0 RtkPos |
              Ctr__Rule_6__1 RtkPos AnnotationArguments
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_6 where
    rtkPosOf (Ctr__Rule_6__0 p) = p
    rtkPosOf (Ctr__Rule_6__1 p _) = p
data Rule_60 = Ctr__Rule_60__0 RtkPos AssignmentOp Expression
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_60 where
    rtkPosOf (Ctr__Rule_60__0 p _ _) = p
data Rule_61 = Ctr__Rule_61__0 RtkPos |
               Ctr__Rule_61__1 RtkPos Rule_62
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_61 where
    rtkPosOf (Ctr__Rule_61__0 p) = p
    rtkPosOf (Ctr__Rule_61__1 p _) = p
data Rule_62 = Ctr__Rule_62__0 RtkPos Arglist
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_62 where
    rtkPosOf (Ctr__Rule_62__0 p _) = p
data Rule_63 = Ctr__Rule_63__0 RtkPos |
               Ctr__Rule_63__1 RtkPos Rule_64
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_63 where
    rtkPosOf (Ctr__Rule_63__0 p) = p
    rtkPosOf (Ctr__Rule_63__1 p _) = p
data Rule_64 = Ctr__Rule_64__0 RtkPos Arglist
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_64 where
    rtkPosOf (Ctr__Rule_64__0 p _) = p
data Rule_65 = Ctr__Rule_65__0 RtkPos Arglist |
               Ctr__Rule_65__1 RtkPos DimExprs Rule_66
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_65 where
    rtkPosOf (Ctr__Rule_65__0 p _) = p
    rtkPosOf (Ctr__Rule_65__1 p _ _) = p
data Rule_66 = Ctr__Rule_66__0 RtkPos |
               Ctr__Rule_66__1 RtkPos NonEmptyDims
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_66 where
    rtkPosOf (Ctr__Rule_66__0 p) = p
    rtkPosOf (Ctr__Rule_66__1 p _) = p
data Rule_67 = Anti_Rule_67 String |
               Ctr__Rule_67__1 RtkPos Expression
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_67 where
    rtkPosOf (Anti_Rule_67 _) = rtkNoPos
    rtkPosOf (Ctr__Rule_67__1 p _) = p
data Rule_69 = Ctr__Rule_69__0 RtkPos Expression Rule_70
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_69 where
    rtkPosOf (Ctr__Rule_69__0 p _ _) = p
type Rule_7 = [Rule_8]
type Rule_70 = [Rule_71]
data Rule_71 = Ctr__Rule_71__0 RtkPos Expression
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_71 where
    rtkPosOf (Ctr__Rule_71__0 p _) = p
type Rule_72 = [Rule_73]
data Rule_73 = Ctr__Rule_73__0 RtkPos TypeArgument
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_73 where
    rtkPosOf (Ctr__Rule_73__0 p _) = p
data Rule_74 = Ctr__Rule_74__0 RtkPos TypeParameter Rule_75
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_74 where
    rtkPosOf (Ctr__Rule_74__0 p _ _) = p
type Rule_75 = [Rule_76]
data Rule_76 = Ctr__Rule_76__0 RtkPos TypeParameter
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_76 where
    rtkPosOf (Ctr__Rule_76__0 p _) = p
data Rule_77 = Ctr__Rule_77__0 RtkPos |
               Ctr__Rule_77__1 RtkPos Rule_78
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_77 where
    rtkPosOf (Ctr__Rule_77__0 p) = p
    rtkPosOf (Ctr__Rule_77__1 p _) = p
data Rule_78 = Ctr__Rule_78__0 RtkPos Type Rule_79
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_78 where
    rtkPosOf (Ctr__Rule_78__0 p _ _) = p
type Rule_79 = [Rule_80]
data Rule_8 = Ctr__Rule_8__0 RtkPos AnnotationElement
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_8 where
    rtkPosOf (Ctr__Rule_8__0 p _) = p
data Rule_80 = Ctr__Rule_80__0 RtkPos Type
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_80 where
    rtkPosOf (Ctr__Rule_80__0 p _) = p
type Rule_81 = [Rule_82]
data Rule_82 = Ctr__Rule_82__0 RtkPos String
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_82 where
    rtkPosOf (Ctr__Rule_82__0 p _) = p
data ShiftOp = Anti_ShiftOp String |
               Ctr__ShiftOp__0 RtkPos |
               Ctr__ShiftOp__1 RtkPos |
               Ctr__ShiftOp__2 RtkPos
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ShiftOp where
    rtkPosOf (Anti_ShiftOp _) = rtkNoPos
    rtkPosOf (Ctr__ShiftOp__0 p) = p
    rtkPosOf (Ctr__ShiftOp__1 p) = p
    rtkPosOf (Ctr__ShiftOp__2 p) = p
data Statement = Anti_Statement String |
                 Ctr__Statement__0 RtkPos VariableDeclaration |
                 Ctr__Statement__1 RtkPos OptExpression |
                 Ctr__Statement__2 RtkPos Expression |
                 Ctr__Statement__3 RtkPos StatementBlock |
                 Ctr__Statement__4 RtkPos IfStatement |
                 Ctr__Statement__5 RtkPos DoStatement |
                 Ctr__Statement__6 RtkPos WhileStatement |
                 Ctr__Statement__7 RtkPos ForStatement |
                 Ctr__Statement__8 RtkPos TryStatement |
                 Ctr__Statement__9 RtkPos SwitchStatement |
                 Ctr__Statement__10 RtkPos Expression Statement |
                 Ctr__Statement__11 RtkPos Expression |
                 Ctr__Statement__12 RtkPos String Statement |
                 Ctr__Statement__13 RtkPos OptId |
                 Ctr__Statement__14 RtkPos OptId |
                 Ctr__Statement__15 RtkPos
                 deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Statement where
    rtkPosOf (Anti_Statement _) = rtkNoPos
    rtkPosOf (Ctr__Statement__0 p _) = p
    rtkPosOf (Ctr__Statement__1 p _) = p
    rtkPosOf (Ctr__Statement__2 p _) = p
    rtkPosOf (Ctr__Statement__3 p _) = p
    rtkPosOf (Ctr__Statement__4 p _) = p
    rtkPosOf (Ctr__Statement__5 p _) = p
    rtkPosOf (Ctr__Statement__6 p _) = p
    rtkPosOf (Ctr__Statement__7 p _) = p
    rtkPosOf (Ctr__Statement__8 p _) = p
    rtkPosOf (Ctr__Statement__9 p _) = p
    rtkPosOf (Ctr__Statement__10 p _ _) = p
    rtkPosOf (Ctr__Statement__11 p _) = p
    rtkPosOf (Ctr__Statement__12 p _ _) = p
    rtkPosOf (Ctr__Statement__13 p _) = p
    rtkPosOf (Ctr__Statement__14 p _) = p
    rtkPosOf (Ctr__Statement__15 p) = p
data StatementBlock = Anti_StatementBlock String |
                      Ctr__StatementBlock__0 RtkPos StatementList
                      deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf StatementBlock where
    rtkPosOf (Anti_StatementBlock _) = rtkNoPos
    rtkPosOf (Ctr__StatementBlock__0 p _) = p
type StatementList = [Statement]
data StaticInitializer = Anti_StaticInitializer String |
                         Ctr__StaticInitializer__0 RtkPos StatementBlock
                         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf StaticInitializer where
    rtkPosOf (Anti_StaticInitializer _) = rtkNoPos
    rtkPosOf (Ctr__StaticInitializer__0 p _) = p
type SwitchCaseList = [Rule_57]
data SwitchStatement = Anti_SwitchStatement String |
                       Ctr__SwitchStatement__0 RtkPos Expression SwitchCaseList
                       deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf SwitchStatement where
    rtkPosOf (Anti_SwitchStatement _) = rtkNoPos
    rtkPosOf (Ctr__SwitchStatement__0 p _ _) = p
data TryStatement = Anti_TryStatement String |
                    Ctr__TryStatement__0 RtkPos Statement CatchList OptFinally
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TryStatement where
    rtkPosOf (Anti_TryStatement _) = rtkNoPos
    rtkPosOf (Ctr__TryStatement__0 p _ _ _) = p
data Type = Anti_Type String |
            Ctr__Type__0 RtkPos PrimitiveTypeKeyword Dims |
            Ctr__Type__1 RtkPos CompoundName NonEmptyTypeArguments Dims |
            Ctr__Type__2 RtkPos CompoundName NonEmptyDims |
            Ctr__Type__3 RtkPos CompoundName
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Type where
    rtkPosOf (Anti_Type _) = rtkNoPos
    rtkPosOf (Ctr__Type__0 p _ _) = p
    rtkPosOf (Ctr__Type__1 p _ _ _) = p
    rtkPosOf (Ctr__Type__2 p _ _) = p
    rtkPosOf (Ctr__Type__3 p _) = p
data TypeArgument = Anti_TypeArgument String |
                    Ctr__TypeArgument__0 RtkPos Type |
                    Ctr__TypeArgument__1 RtkPos WildcardType
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TypeArgument where
    rtkPosOf (Anti_TypeArgument _) = rtkNoPos
    rtkPosOf (Ctr__TypeArgument__0 p _) = p
    rtkPosOf (Ctr__TypeArgument__1 p _) = p
data TypeArguments = Anti_TypeArguments String |
                     Ctr__TypeArguments__0 RtkPos |
                     Ctr__TypeArguments__1 RtkPos NonEmptyTypeArguments
                     deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TypeArguments where
    rtkPosOf (Anti_TypeArguments _) = rtkNoPos
    rtkPosOf (Ctr__TypeArguments__0 p) = p
    rtkPosOf (Ctr__TypeArguments__1 p _) = p
data TypeDeclRest = Anti_TypeDeclRest String |
                    Ctr__TypeDeclRest__0 RtkPos ClassDeclaration |
                    Ctr__TypeDeclRest__1 RtkPos InterfaceDeclaration |
                    Ctr__TypeDeclRest__2 RtkPos EnumDeclaration |
                    Ctr__TypeDeclRest__3 RtkPos AnnotationDeclaration
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TypeDeclRest where
    rtkPosOf (Anti_TypeDeclRest _) = rtkNoPos
    rtkPosOf (Ctr__TypeDeclRest__0 p _) = p
    rtkPosOf (Ctr__TypeDeclRest__1 p _) = p
    rtkPosOf (Ctr__TypeDeclRest__2 p _) = p
    rtkPosOf (Ctr__TypeDeclRest__3 p _) = p
data TypeDeclaration = Anti_TypeDeclaration String |
                       Ctr__TypeDeclaration__0 RtkPos OptDocComment ModifierList TypeDeclRest
                       deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TypeDeclaration where
    rtkPosOf (Anti_TypeDeclaration _) = rtkNoPos
    rtkPosOf (Ctr__TypeDeclaration__0 p _ _ _) = p
data TypeParameter = Anti_TypeParameter String |
                     Ctr__TypeParameter__0 RtkPos String Rule_77
                     deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TypeParameter where
    rtkPosOf (Anti_TypeParameter _) = rtkNoPos
    rtkPosOf (Ctr__TypeParameter__0 p _ _) = p
data TypeParameters = Anti_TypeParameters String |
                      Ctr__TypeParameters__0 RtkPos |
                      Ctr__TypeParameters__1 RtkPos Rule_74
                      deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TypeParameters where
    rtkPosOf (Anti_TypeParameters _) = rtkNoPos
    rtkPosOf (Ctr__TypeParameters__0 p) = p
    rtkPosOf (Ctr__TypeParameters__1 p _) = p
data TypeSpecifier = Anti_TypeSpecifier String |
                     Ctr__TypeSpecifier__0 RtkPos |
                     Ctr__TypeSpecifier__1 RtkPos |
                     Ctr__TypeSpecifier__2 RtkPos |
                     Ctr__TypeSpecifier__3 RtkPos |
                     Ctr__TypeSpecifier__4 RtkPos |
                     Ctr__TypeSpecifier__5 RtkPos |
                     Ctr__TypeSpecifier__6 RtkPos |
                     Ctr__TypeSpecifier__7 RtkPos |
                     Ctr__TypeSpecifier__8 RtkPos |
                     Ctr__TypeSpecifier__9 RtkPos CompoundName TypeArguments
                     deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf TypeSpecifier where
    rtkPosOf (Anti_TypeSpecifier _) = rtkNoPos
    rtkPosOf (Ctr__TypeSpecifier__0 p) = p
    rtkPosOf (Ctr__TypeSpecifier__1 p) = p
    rtkPosOf (Ctr__TypeSpecifier__2 p) = p
    rtkPosOf (Ctr__TypeSpecifier__3 p) = p
    rtkPosOf (Ctr__TypeSpecifier__4 p) = p
    rtkPosOf (Ctr__TypeSpecifier__5 p) = p
    rtkPosOf (Ctr__TypeSpecifier__6 p) = p
    rtkPosOf (Ctr__TypeSpecifier__7 p) = p
    rtkPosOf (Ctr__TypeSpecifier__8 p) = p
    rtkPosOf (Ctr__TypeSpecifier__9 p _ _) = p
data VariableDeclaration = Anti_VariableDeclaration String |
                           Ctr__VariableDeclaration__0 RtkPos Type VariableDeclaratorList
                           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf VariableDeclaration where
    rtkPosOf (Anti_VariableDeclaration _) = rtkNoPos
    rtkPosOf (Ctr__VariableDeclaration__0 p _ _) = p
data VariableDeclarator = Anti_VariableDeclarator String |
                          Ctr__VariableDeclarator__0 RtkPos String Dims OptVariableInitializer
                          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf VariableDeclarator where
    rtkPosOf (Anti_VariableDeclarator _) = rtkNoPos
    rtkPosOf (Ctr__VariableDeclarator__0 p _ _ _) = p
data VariableDeclaratorList = Anti_VariableDeclaratorList String |
                              Ctr__VariableDeclaratorList__0 RtkPos VariableDeclarator Rule_42
                              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf VariableDeclaratorList where
    rtkPosOf (Anti_VariableDeclaratorList _) = rtkNoPos
    rtkPosOf (Ctr__VariableDeclaratorList__0 p _ _) = p
data VariableInitializer = Anti_VariableInitializer String |
                           Ctr__VariableInitializer__0 RtkPos Expression |
                           Ctr__VariableInitializer__1 RtkPos VariableInitializerList
                           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf VariableInitializer where
    rtkPosOf (Anti_VariableInitializer _) = rtkNoPos
    rtkPosOf (Ctr__VariableInitializer__0 p _) = p
    rtkPosOf (Ctr__VariableInitializer__1 p _) = p
data VariableInitializerList = Anti_VariableInitializerList String |
                               Ctr__VariableInitializerList__0 RtkPos |
                               Ctr__VariableInitializerList__1 RtkPos Rule_45
                               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf VariableInitializerList where
    rtkPosOf (Anti_VariableInitializerList _) = rtkNoPos
    rtkPosOf (Ctr__VariableInitializerList__0 p) = p
    rtkPosOf (Ctr__VariableInitializerList__1 p _) = p
data WhileStatement = Anti_WhileStatement String |
                      Ctr__WhileStatement__0 RtkPos Expression Statement
                      deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf WhileStatement where
    rtkPosOf (Anti_WhileStatement _) = rtkNoPos
    rtkPosOf (Ctr__WhileStatement__0 p _ _) = p
data WildcardType = Anti_WildcardType String |
                    Ctr__WildcardType__0 RtkPos |
                    Ctr__WildcardType__1 RtkPos Type |
                    Ctr__WildcardType__2 RtkPos Type
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf WildcardType where
    rtkPosOf (Anti_WildcardType _) = rtkNoPos
    rtkPosOf (Ctr__WildcardType__0 p) = p
    rtkPosOf (Ctr__WildcardType__1 p _) = p
    rtkPosOf (Ctr__WildcardType__2 p _) = p
}