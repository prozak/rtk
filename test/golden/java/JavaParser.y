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
tok_AdditiveOp_dummy_169 { L.PosToken _ L.Tk__tok_AdditiveOp_dummy_169 }
tok_Annotation_dummy_168 { L.PosToken _ L.Tk__tok_Annotation_dummy_168 }
tok_AnnotationArguments_dummy_167 { L.PosToken _ L.Tk__tok_AnnotationArguments_dummy_167 }
tok_AnnotationDeclaration_dummy_166 { L.PosToken _ L.Tk__tok_AnnotationDeclaration_dummy_166 }
tok_AnnotationElement_dummy_165 { L.PosToken _ L.Tk__tok_AnnotationElement_dummy_165 }
tok_AnnotationList_dummy_164 { L.PosToken _ L.Tk__tok_AnnotationList_dummy_164 }
tok_AnnotationTypeElement_dummy_163 { L.PosToken _ L.Tk__tok_AnnotationTypeElement_dummy_163 }
tok_AnnotationTypeElementList_dummy_162 { L.PosToken _ L.Tk__tok_AnnotationTypeElementList_dummy_162 }
tok_Arglist_dummy_161 { L.PosToken _ L.Tk__tok_Arglist_dummy_161 }
tok_AssignmentOp_dummy_160 { L.PosToken _ L.Tk__tok_AssignmentOp_dummy_160 }
tok_CatchList_dummy_159 { L.PosToken _ L.Tk__tok_CatchList_dummy_159 }
tok_ClassDeclaration_dummy_158 { L.PosToken _ L.Tk__tok_ClassDeclaration_dummy_158 }
tok_CompilationUnit_dummy_157 { L.PosToken _ L.Tk__tok_CompilationUnit_dummy_157 }
tok_CompoundName_dummy_156 { L.PosToken _ L.Tk__tok_CompoundName_dummy_156 }
tok_CreationExpression_dummy_155 { L.PosToken _ L.Tk__tok_CreationExpression_dummy_155 }
tok_DoStatement_dummy_154 { L.PosToken _ L.Tk__tok_DoStatement_dummy_154 }
tok_DocComment_dummy_153 { L.PosToken _ L.Tk__tok_DocComment_dummy_153 }
tok_EnumConstant_dummy_152 { L.PosToken _ L.Tk__tok_EnumConstant_dummy_152 }
tok_EnumConstantList_dummy_151 { L.PosToken _ L.Tk__tok_EnumConstantList_dummy_151 }
tok_EnumDeclaration_dummy_150 { L.PosToken _ L.Tk__tok_EnumDeclaration_dummy_150 }
tok_EqualityOp_dummy_149 { L.PosToken _ L.Tk__tok_EqualityOp_dummy_149 }
tok_Expression_dummy_148 { L.PosToken _ L.Tk__tok_Expression_dummy_148 }
tok_ExtendsList_dummy_147 { L.PosToken _ L.Tk__tok_ExtendsList_dummy_147 }
tok_FieldDeclaration_dummy_146 { L.PosToken _ L.Tk__tok_FieldDeclaration_dummy_146 }
tok_FieldDeclarationList_dummy_145 { L.PosToken _ L.Tk__tok_FieldDeclarationList_dummy_145 }
tok_ForStatement_dummy_144 { L.PosToken _ L.Tk__tok_ForStatement_dummy_144 }
tok_IfStatement_dummy_143 { L.PosToken _ L.Tk__tok_IfStatement_dummy_143 }
tok_ImplementsList_dummy_142 { L.PosToken _ L.Tk__tok_ImplementsList_dummy_142 }
tok_ImportList_dummy_141 { L.PosToken _ L.Tk__tok_ImportList_dummy_141 }
tok_ImportStatement_dummy_140 { L.PosToken _ L.Tk__tok_ImportStatement_dummy_140 }
tok_InterfaceDeclaration_dummy_139 { L.PosToken _ L.Tk__tok_InterfaceDeclaration_dummy_139 }
tok_Java_dummy_170 { L.PosToken _ L.Tk__tok_Java_dummy_170 }
tok_Literal_dummy_138 { L.PosToken _ L.Tk__tok_Literal_dummy_138 }
tok_MemberAfterFirstId_dummy_137 { L.PosToken _ L.Tk__tok_MemberAfterFirstId_dummy_137 }
tok_MemberDeclaration_dummy_136 { L.PosToken _ L.Tk__tok_MemberDeclaration_dummy_136 }
tok_MemberRest_dummy_135 { L.PosToken _ L.Tk__tok_MemberRest_dummy_135 }
tok_Modifier_dummy_134 { L.PosToken _ L.Tk__tok_Modifier_dummy_134 }
tok_ModifierList_dummy_133 { L.PosToken _ L.Tk__tok_ModifierList_dummy_133 }
tok_MoreTypeSpecifier_dummy_132 { L.PosToken _ L.Tk__tok_MoreTypeSpecifier_dummy_132 }
tok_MoreVariableDeclarators_dummy_131 { L.PosToken _ L.Tk__tok_MoreVariableDeclarators_dummy_131 }
tok_MultiplicativeOp_dummy_130 { L.PosToken _ L.Tk__tok_MultiplicativeOp_dummy_130 }
tok_NestedTypeDeclaration_dummy_129 { L.PosToken _ L.Tk__tok_NestedTypeDeclaration_dummy_129 }
tok_OptDocComment_dummy_128 { L.PosToken _ L.Tk__tok_OptDocComment_dummy_128 }
tok_OptElsePart_dummy_127 { L.PosToken _ L.Tk__tok_OptElsePart_dummy_127 }
tok_OptExpression_dummy_126 { L.PosToken _ L.Tk__tok_OptExpression_dummy_126 }
tok_OptFinally_dummy_125 { L.PosToken _ L.Tk__tok_OptFinally_dummy_125 }
tok_OptId_dummy_124 { L.PosToken _ L.Tk__tok_OptId_dummy_124 }
tok_OptVariableInitializer_dummy_123 { L.PosToken _ L.Tk__tok_OptVariableInitializer_dummy_123 }
tok_Package_dummy_122 { L.PosToken _ L.Tk__tok_Package_dummy_122 }
tok_Parameter_dummy_121 { L.PosToken _ L.Tk__tok_Parameter_dummy_121 }
tok_ParameterList_dummy_120 { L.PosToken _ L.Tk__tok_ParameterList_dummy_120 }
tok_PostfixOp_dummy_119 { L.PosToken _ L.Tk__tok_PostfixOp_dummy_119 }
tok_PrefixOp_dummy_118 { L.PosToken _ L.Tk__tok_PrefixOp_dummy_118 }
tok_PrimitiveTypeKeyword_dummy_117 { L.PosToken _ L.Tk__tok_PrimitiveTypeKeyword_dummy_117 }
tok_RelationalOp_dummy_116 { L.PosToken _ L.Tk__tok_RelationalOp_dummy_116 }
tok_ShiftOp_dummy_115 { L.PosToken _ L.Tk__tok_ShiftOp_dummy_115 }
tok_SquareBracketsList_dummy_114 { L.PosToken _ L.Tk__tok_SquareBracketsList_dummy_114 }
tok_Statement_dummy_113 { L.PosToken _ L.Tk__tok_Statement_dummy_113 }
tok_StatementBlock_dummy_112 { L.PosToken _ L.Tk__tok_StatementBlock_dummy_112 }
tok_StatementList_dummy_111 { L.PosToken _ L.Tk__tok_StatementList_dummy_111 }
tok_StatementWithoutIf_dummy_110 { L.PosToken _ L.Tk__tok_StatementWithoutIf_dummy_110 }
tok_StaticInitializer_dummy_109 { L.PosToken _ L.Tk__tok_StaticInitializer_dummy_109 }
tok_SwitchCaseList_dummy_108 { L.PosToken _ L.Tk__tok_SwitchCaseList_dummy_108 }
tok_SwitchStatement_dummy_107 { L.PosToken _ L.Tk__tok_SwitchStatement_dummy_107 }
tok_TryStatement_dummy_106 { L.PosToken _ L.Tk__tok_TryStatement_dummy_106 }
tok_Type_dummy_105 { L.PosToken _ L.Tk__tok_Type_dummy_105 }
tok_TypeArgument_dummy_104 { L.PosToken _ L.Tk__tok_TypeArgument_dummy_104 }
tok_TypeArguments_dummy_103 { L.PosToken _ L.Tk__tok_TypeArguments_dummy_103 }
tok_TypeDeclaration_dummy_102 { L.PosToken _ L.Tk__tok_TypeDeclaration_dummy_102 }
tok_TypeParameter_dummy_101 { L.PosToken _ L.Tk__tok_TypeParameter_dummy_101 }
tok_TypeParameters_dummy_100 { L.PosToken _ L.Tk__tok_TypeParameters_dummy_100 }
tok_TypeSpecifier_dummy_99 { L.PosToken _ L.Tk__tok_TypeSpecifier_dummy_99 }
tok_VariableDeclaration_dummy_98 { L.PosToken _ L.Tk__tok_VariableDeclaration_dummy_98 }
tok_VariableDeclarator_dummy_97 { L.PosToken _ L.Tk__tok_VariableDeclarator_dummy_97 }
tok_VariableDeclaratorList_dummy_96 { L.PosToken _ L.Tk__tok_VariableDeclaratorList_dummy_96 }
tok_VariableInitializer_dummy_95 { L.PosToken _ L.Tk__tok_VariableInitializer_dummy_95 }
tok_VariableInitializerList_dummy_94 { L.PosToken _ L.Tk__tok_VariableInitializerList_dummy_94 }
tok_WhileStatement_dummy_93 { L.PosToken _ L.Tk__tok_WhileStatement_dummy_93 }
tok_WildcardType_dummy_92 { L.PosToken _ L.Tk__tok_WildcardType_dummy_92 }
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
doccomment { L.PosToken _ (L.Tk__doccomment $$) }
id { L.PosToken _ (L.Tk__id $$) }
string { L.PosToken _ (L.Tk__string $$) }
char { L.PosToken _ (L.Tk__char $$) }
floatTypeSuffix { L.PosToken _ (L.Tk__floatTypeSuffix $$) }
exponentPart { L.PosToken _ (L.Tk__exponentPart $$) }
floatLiteral { L.PosToken _ (L.Tk__floatLiteral $$) }
integerLiteral { L.PosToken _ (L.Tk__integerLiteral $$) }
qq_CompoundName { L.PosToken _ (L.Tk__qq_CompoundName $$) }
qq_Modifier { L.PosToken _ (L.Tk__qq_Modifier $$) }
qq_TypeSpecifier { L.PosToken _ (L.Tk__qq_TypeSpecifier $$) }
qq_Type { L.PosToken _ (L.Tk__qq_Type $$) }
qq_TypeParameter { L.PosToken _ (L.Tk__qq_TypeParameter $$) }
qq_TypeParameters { L.PosToken _ (L.Tk__qq_TypeParameters $$) }
qq_WildcardType { L.PosToken _ (L.Tk__qq_WildcardType $$) }
qq_TypeArgument { L.PosToken _ (L.Tk__qq_TypeArgument $$) }
qq_TypeArguments { L.PosToken _ (L.Tk__qq_TypeArguments $$) }
qq_Arglist { L.PosToken _ (L.Tk__qq_Arglist $$) }
qq_Literal { L.PosToken _ (L.Tk__qq_Literal $$) }
qq_CreationExpression { L.PosToken _ (L.Tk__qq_CreationExpression $$) }
qq_PostfixOp { L.PosToken _ (L.Tk__qq_PostfixOp $$) }
qq_PrefixOp { L.PosToken _ (L.Tk__qq_PrefixOp $$) }
qq_MultiplicativeOp { L.PosToken _ (L.Tk__qq_MultiplicativeOp $$) }
qq_AdditiveOp { L.PosToken _ (L.Tk__qq_AdditiveOp $$) }
qq_ShiftOp { L.PosToken _ (L.Tk__qq_ShiftOp $$) }
qq_RelationalOp { L.PosToken _ (L.Tk__qq_RelationalOp $$) }
qq_EqualityOp { L.PosToken _ (L.Tk__qq_EqualityOp $$) }
qq_AssignmentOp { L.PosToken _ (L.Tk__qq_AssignmentOp $$) }
qq_Expression { L.PosToken _ (L.Tk__qq_Expression $$) }
qq_SwitchStatement { L.PosToken _ (L.Tk__qq_SwitchStatement $$) }
qq_SwitchCaseList { L.PosToken _ (L.Tk__qq_SwitchCaseList $$) }
qq_TryStatement { L.PosToken _ (L.Tk__qq_TryStatement $$) }
qq_OptFinally { L.PosToken _ (L.Tk__qq_OptFinally $$) }
qq_CatchList { L.PosToken _ (L.Tk__qq_CatchList $$) }
qq_ForStatement { L.PosToken _ (L.Tk__qq_ForStatement $$) }
qq_WhileStatement { L.PosToken _ (L.Tk__qq_WhileStatement $$) }
qq_DoStatement { L.PosToken _ (L.Tk__qq_DoStatement $$) }
qq_IfStatement { L.PosToken _ (L.Tk__qq_IfStatement $$) }
qq_OptElsePart { L.PosToken _ (L.Tk__qq_OptElsePart $$) }
qq_StatementWithoutIf { L.PosToken _ (L.Tk__qq_StatementWithoutIf $$) }
qq_Statement { L.PosToken _ (L.Tk__qq_Statement $$) }
qq_OptId { L.PosToken _ (L.Tk__qq_OptId $$) }
qq_OptExpression { L.PosToken _ (L.Tk__qq_OptExpression $$) }
qq_StatementList { L.PosToken _ (L.Tk__qq_StatementList $$) }
qq_Parameter { L.PosToken _ (L.Tk__qq_Parameter $$) }
qq_ParameterList { L.PosToken _ (L.Tk__qq_ParameterList $$) }
qq_StaticInitializer { L.PosToken _ (L.Tk__qq_StaticInitializer $$) }
qq_VariableInitializer { L.PosToken _ (L.Tk__qq_VariableInitializer $$) }
qq_VariableInitializerList { L.PosToken _ (L.Tk__qq_VariableInitializerList $$) }
qq_VariableDeclarator { L.PosToken _ (L.Tk__qq_VariableDeclarator $$) }
qq_OptVariableInitializer { L.PosToken _ (L.Tk__qq_OptVariableInitializer $$) }
qq_VariableDeclaration { L.PosToken _ (L.Tk__qq_VariableDeclaration $$) }
qq_VariableDeclaratorList { L.PosToken _ (L.Tk__qq_VariableDeclaratorList $$) }
qq_StatementBlock { L.PosToken _ (L.Tk__qq_StatementBlock $$) }
qq_MoreVariableDeclarators { L.PosToken _ (L.Tk__qq_MoreVariableDeclarators $$) }
qq_MemberRest { L.PosToken _ (L.Tk__qq_MemberRest $$) }
qq_MoreTypeSpecifier { L.PosToken _ (L.Tk__qq_MoreTypeSpecifier $$) }
qq_MemberAfterFirstId { L.PosToken _ (L.Tk__qq_MemberAfterFirstId $$) }
qq_PrimitiveTypeKeyword { L.PosToken _ (L.Tk__qq_PrimitiveTypeKeyword $$) }
qq_MemberDeclaration { L.PosToken _ (L.Tk__qq_MemberDeclaration $$) }
qq_SquareBracketsList { L.PosToken _ (L.Tk__qq_SquareBracketsList $$) }
qq_FieldDeclaration { L.PosToken _ (L.Tk__qq_FieldDeclaration $$) }
qq_NestedTypeDeclaration { L.PosToken _ (L.Tk__qq_NestedTypeDeclaration $$) }
qq_EnumDeclaration { L.PosToken _ (L.Tk__qq_EnumDeclaration $$) }
qq_EnumConstantList { L.PosToken _ (L.Tk__qq_EnumConstantList $$) }
qq_EnumConstant { L.PosToken _ (L.Tk__qq_EnumConstant $$) }
qq_AnnotationTypeElement { L.PosToken _ (L.Tk__qq_AnnotationTypeElement $$) }
qq_AnnotationTypeElementList { L.PosToken _ (L.Tk__qq_AnnotationTypeElementList $$) }
qq_AnnotationDeclaration { L.PosToken _ (L.Tk__qq_AnnotationDeclaration $$) }
qq_InterfaceDeclaration { L.PosToken _ (L.Tk__qq_InterfaceDeclaration $$) }
qq_ClassDeclaration { L.PosToken _ (L.Tk__qq_ClassDeclaration $$) }
qq_FieldDeclarationList { L.PosToken _ (L.Tk__qq_FieldDeclarationList $$) }
qq_ImplementsList { L.PosToken _ (L.Tk__qq_ImplementsList $$) }
qq_ExtendsList { L.PosToken _ (L.Tk__qq_ExtendsList $$) }
qq_ModifierList { L.PosToken _ (L.Tk__qq_ModifierList $$) }
qq_AnnotationList { L.PosToken _ (L.Tk__qq_AnnotationList $$) }
qq_AnnotationElement { L.PosToken _ (L.Tk__qq_AnnotationElement $$) }
qq_AnnotationArguments { L.PosToken _ (L.Tk__qq_AnnotationArguments $$) }
qq_Annotation { L.PosToken _ (L.Tk__qq_Annotation $$) }
qq_DocComment { L.PosToken _ (L.Tk__qq_DocComment $$) }
qq_ImportStatement { L.PosToken _ (L.Tk__qq_ImportStatement $$) }
qq_Package { L.PosToken _ (L.Tk__qq_Package $$) }
qq_CompilationUnit { L.PosToken _ (L.Tk__qq_CompilationUnit $$) }
qq_ImportList { L.PosToken _ (L.Tk__qq_ImportList $$) }
qq_TypeDeclaration { L.PosToken _ (L.Tk__qq_TypeDeclaration $$) }
qq_OptDocComment { L.PosToken _ (L.Tk__qq_OptDocComment $$) }
qq_Java { L.PosToken _ (L.Tk__qq_Java $$) }

%%

Java__top : Java rtk__eof { $1 }

Java : tok_Java_dummy_170 Java tok_Java_dummy_170 { Ctr__Java__0 $2 } |
       tok_AdditiveOp_dummy_169 AdditiveOp tok_AdditiveOp_dummy_169 { Ctr__Java__1 $2 } |
       tok_Annotation_dummy_168 Annotation tok_Annotation_dummy_168 { Ctr__Java__2 $2 } |
       tok_AnnotationArguments_dummy_167 AnnotationArguments tok_AnnotationArguments_dummy_167 { Ctr__Java__3 $2 } |
       tok_AnnotationDeclaration_dummy_166 AnnotationDeclaration tok_AnnotationDeclaration_dummy_166 { Ctr__Java__4 $2 } |
       tok_AnnotationElement_dummy_165 AnnotationElement tok_AnnotationElement_dummy_165 { Ctr__Java__5 $2 } |
       tok_AnnotationList_dummy_164 AnnotationList tok_AnnotationList_dummy_164 { Ctr__Java__6 (reverse $2) } |
       tok_AnnotationTypeElement_dummy_163 AnnotationTypeElement tok_AnnotationTypeElement_dummy_163 { Ctr__Java__7 $2 } |
       tok_AnnotationTypeElementList_dummy_162 AnnotationTypeElementList tok_AnnotationTypeElementList_dummy_162 { Ctr__Java__8 (reverse $2) } |
       tok_Arglist_dummy_161 Arglist tok_Arglist_dummy_161 { Ctr__Java__9 $2 } |
       tok_AssignmentOp_dummy_160 AssignmentOp tok_AssignmentOp_dummy_160 { Ctr__Java__10 $2 } |
       tok_CatchList_dummy_159 CatchList tok_CatchList_dummy_159 { Ctr__Java__11 (reverse $2) } |
       tok_ClassDeclaration_dummy_158 ClassDeclaration tok_ClassDeclaration_dummy_158 { Ctr__Java__12 $2 } |
       tok_CompilationUnit_dummy_157 CompilationUnit tok_CompilationUnit_dummy_157 { Ctr__Java__13 $2 } |
       tok_CompoundName_dummy_156 CompoundName tok_CompoundName_dummy_156 { Ctr__Java__14 $2 } |
       tok_CreationExpression_dummy_155 CreationExpression tok_CreationExpression_dummy_155 { Ctr__Java__15 $2 } |
       tok_DoStatement_dummy_154 DoStatement tok_DoStatement_dummy_154 { Ctr__Java__16 $2 } |
       tok_DocComment_dummy_153 DocComment tok_DocComment_dummy_153 { Ctr__Java__17 $2 } |
       tok_EnumConstant_dummy_152 EnumConstant tok_EnumConstant_dummy_152 { Ctr__Java__18 $2 } |
       tok_EnumConstantList_dummy_151 EnumConstantList tok_EnumConstantList_dummy_151 { Ctr__Java__19 $2 } |
       tok_EnumDeclaration_dummy_150 EnumDeclaration tok_EnumDeclaration_dummy_150 { Ctr__Java__20 $2 } |
       tok_EqualityOp_dummy_149 EqualityOp tok_EqualityOp_dummy_149 { Ctr__Java__21 $2 } |
       tok_Expression_dummy_148 Expression tok_Expression_dummy_148 { Ctr__Java__22 $2 } |
       tok_ExtendsList_dummy_147 ExtendsList tok_ExtendsList_dummy_147 { Ctr__Java__23 $2 } |
       tok_FieldDeclaration_dummy_146 FieldDeclaration tok_FieldDeclaration_dummy_146 { Ctr__Java__24 $2 } |
       tok_FieldDeclarationList_dummy_145 FieldDeclarationList tok_FieldDeclarationList_dummy_145 { Ctr__Java__25 (reverse $2) } |
       tok_ForStatement_dummy_144 ForStatement tok_ForStatement_dummy_144 { Ctr__Java__26 $2 } |
       tok_IfStatement_dummy_143 IfStatement tok_IfStatement_dummy_143 { Ctr__Java__27 $2 } |
       tok_ImplementsList_dummy_142 ImplementsList tok_ImplementsList_dummy_142 { Ctr__Java__28 $2 } |
       tok_ImportList_dummy_141 ImportList tok_ImportList_dummy_141 { Ctr__Java__29 (reverse $2) } |
       tok_ImportStatement_dummy_140 ImportStatement tok_ImportStatement_dummy_140 { Ctr__Java__30 $2 } |
       tok_InterfaceDeclaration_dummy_139 InterfaceDeclaration tok_InterfaceDeclaration_dummy_139 { Ctr__Java__31 $2 } |
       tok_Literal_dummy_138 Literal tok_Literal_dummy_138 { Ctr__Java__32 $2 } |
       tok_MemberAfterFirstId_dummy_137 MemberAfterFirstId tok_MemberAfterFirstId_dummy_137 { Ctr__Java__33 $2 } |
       tok_MemberDeclaration_dummy_136 MemberDeclaration tok_MemberDeclaration_dummy_136 { Ctr__Java__34 $2 } |
       tok_MemberRest_dummy_135 MemberRest tok_MemberRest_dummy_135 { Ctr__Java__35 $2 } |
       tok_Modifier_dummy_134 Modifier tok_Modifier_dummy_134 { Ctr__Java__36 $2 } |
       tok_ModifierList_dummy_133 ModifierList tok_ModifierList_dummy_133 { Ctr__Java__37 (reverse $2) } |
       tok_MoreTypeSpecifier_dummy_132 MoreTypeSpecifier tok_MoreTypeSpecifier_dummy_132 { Ctr__Java__38 $2 } |
       tok_MoreVariableDeclarators_dummy_131 MoreVariableDeclarators tok_MoreVariableDeclarators_dummy_131 { Ctr__Java__39 (reverse $2) } |
       tok_MultiplicativeOp_dummy_130 MultiplicativeOp tok_MultiplicativeOp_dummy_130 { Ctr__Java__40 $2 } |
       tok_NestedTypeDeclaration_dummy_129 NestedTypeDeclaration tok_NestedTypeDeclaration_dummy_129 { Ctr__Java__41 $2 } |
       tok_OptDocComment_dummy_128 OptDocComment tok_OptDocComment_dummy_128 { Ctr__Java__42 $2 } |
       tok_OptElsePart_dummy_127 OptElsePart tok_OptElsePart_dummy_127 { Ctr__Java__43 $2 } |
       tok_OptExpression_dummy_126 OptExpression tok_OptExpression_dummy_126 { Ctr__Java__44 $2 } |
       tok_OptFinally_dummy_125 OptFinally tok_OptFinally_dummy_125 { Ctr__Java__45 $2 } |
       tok_OptId_dummy_124 OptId tok_OptId_dummy_124 { Ctr__Java__46 $2 } |
       tok_OptVariableInitializer_dummy_123 OptVariableInitializer tok_OptVariableInitializer_dummy_123 { Ctr__Java__47 $2 } |
       tok_Package_dummy_122 Package tok_Package_dummy_122 { Ctr__Java__48 $2 } |
       tok_Parameter_dummy_121 Parameter tok_Parameter_dummy_121 { Ctr__Java__49 $2 } |
       tok_ParameterList_dummy_120 ParameterList tok_ParameterList_dummy_120 { Ctr__Java__50 $2 } |
       tok_PostfixOp_dummy_119 PostfixOp tok_PostfixOp_dummy_119 { Ctr__Java__51 $2 } |
       tok_PrefixOp_dummy_118 PrefixOp tok_PrefixOp_dummy_118 { Ctr__Java__52 $2 } |
       tok_PrimitiveTypeKeyword_dummy_117 PrimitiveTypeKeyword tok_PrimitiveTypeKeyword_dummy_117 { Ctr__Java__53 $2 } |
       tok_RelationalOp_dummy_116 RelationalOp tok_RelationalOp_dummy_116 { Ctr__Java__54 $2 } |
       tok_ShiftOp_dummy_115 ShiftOp tok_ShiftOp_dummy_115 { Ctr__Java__55 $2 } |
       tok_SquareBracketsList_dummy_114 SquareBracketsList tok_SquareBracketsList_dummy_114 { Ctr__Java__56 (reverse $2) } |
       tok_Statement_dummy_113 Statement tok_Statement_dummy_113 { Ctr__Java__57 $2 } |
       tok_StatementBlock_dummy_112 StatementBlock tok_StatementBlock_dummy_112 { Ctr__Java__58 $2 } |
       tok_StatementList_dummy_111 StatementList tok_StatementList_dummy_111 { Ctr__Java__59 (reverse $2) } |
       tok_StatementWithoutIf_dummy_110 StatementWithoutIf tok_StatementWithoutIf_dummy_110 { Ctr__Java__60 $2 } |
       tok_StaticInitializer_dummy_109 StaticInitializer tok_StaticInitializer_dummy_109 { Ctr__Java__61 $2 } |
       tok_SwitchCaseList_dummy_108 SwitchCaseList tok_SwitchCaseList_dummy_108 { Ctr__Java__62 (reverse $2) } |
       tok_SwitchStatement_dummy_107 SwitchStatement tok_SwitchStatement_dummy_107 { Ctr__Java__63 $2 } |
       tok_TryStatement_dummy_106 TryStatement tok_TryStatement_dummy_106 { Ctr__Java__64 $2 } |
       tok_Type_dummy_105 Type tok_Type_dummy_105 { Ctr__Java__65 $2 } |
       tok_TypeArgument_dummy_104 TypeArgument tok_TypeArgument_dummy_104 { Ctr__Java__66 $2 } |
       tok_TypeArguments_dummy_103 TypeArguments tok_TypeArguments_dummy_103 { Ctr__Java__67 $2 } |
       tok_TypeDeclaration_dummy_102 TypeDeclaration tok_TypeDeclaration_dummy_102 { Ctr__Java__68 $2 } |
       tok_TypeParameter_dummy_101 TypeParameter tok_TypeParameter_dummy_101 { Ctr__Java__69 $2 } |
       tok_TypeParameters_dummy_100 TypeParameters tok_TypeParameters_dummy_100 { Ctr__Java__70 $2 } |
       tok_TypeSpecifier_dummy_99 TypeSpecifier tok_TypeSpecifier_dummy_99 { Ctr__Java__71 $2 } |
       tok_VariableDeclaration_dummy_98 VariableDeclaration tok_VariableDeclaration_dummy_98 { Ctr__Java__72 $2 } |
       tok_VariableDeclarator_dummy_97 VariableDeclarator tok_VariableDeclarator_dummy_97 { Ctr__Java__73 $2 } |
       tok_VariableDeclaratorList_dummy_96 VariableDeclaratorList tok_VariableDeclaratorList_dummy_96 { Ctr__Java__74 $2 } |
       tok_VariableInitializer_dummy_95 VariableInitializer tok_VariableInitializer_dummy_95 { Ctr__Java__75 $2 } |
       tok_VariableInitializerList_dummy_94 VariableInitializerList tok_VariableInitializerList_dummy_94 { Ctr__Java__76 $2 } |
       tok_WhileStatement_dummy_93 WhileStatement tok_WhileStatement_dummy_93 { Ctr__Java__77 $2 } |
       tok_WildcardType_dummy_92 WildcardType tok_WildcardType_dummy_92 { Ctr__Java__78 $2 }

Java : qq_Java { Anti_Java $1 } |
       CompilationUnit { Ctr__Java__79 $1 }

AdditiveOp : qq_AdditiveOp { Anti_AdditiveOp $1 } |
             tok__plus__72 { Ctr__AdditiveOp__0 } |
             tok__minus__73 { Ctr__AdditiveOp__1 }

ListElem_AnnotationList15 : qq_AnnotationList { Anti_Annotation $1 } |
                            Annotation { $1 }

Annotation : qq_Annotation { Anti_Annotation $1 } |
             tok__symbol__5 CompoundName Rule_10 { Ctr__Annotation__1 $2 $3 }

AnnotationArguments : qq_AnnotationArguments { Anti_AnnotationArguments $1 } |
                      AnnotationElement Rule_13 { Ctr__AnnotationArguments__0 $1 (reverse $2) }

AnnotationDeclaration : qq_AnnotationDeclaration { Anti_AnnotationDeclaration $1 } |
                        ModifierList tok__symbol__5 tok_interface_15 id tok__symbol__13 AnnotationTypeElementList tok__symbol__14 { Ctr__AnnotationDeclaration__0 (reverse $1) $4 (reverse $6) }

AnnotationElement : qq_AnnotationElement { Anti_AnnotationElement $1 } |
                    id tok__eql__9 ConditionalExpression { Ctr__AnnotationElement__0 $1 $3 } |
                    ConditionalExpression { Ctr__AnnotationElement__1 $1 }

AnnotationList : {- empty -} { [] } |
                 AnnotationList ListElem_AnnotationList15 { $2 : $1 }

AnnotationTypeElement : qq_AnnotationTypeElement { Anti_AnnotationTypeElement $1 } |
                        FieldDeclaration { Ctr__AnnotationTypeElement__0 $1 }

ListElem_AnnotationTypeElementList25 : qq_AnnotationTypeElementList { Anti_AnnotationTypeElement $1 } |
                                       AnnotationTypeElement { $1 }

AnnotationTypeElementList : {- empty -} { [] } |
                            AnnotationTypeElementList ListElem_AnnotationTypeElementList25 { $2 : $1 }

Arglist : qq_Arglist { Anti_Arglist $1 } |
          { Ctr__Arglist__0 } |
          Rule_77 { Ctr__Arglist__1 $1 }

AssignmentOp : qq_AssignmentOp { Anti_AssignmentOp $1 } |
               tok__eql__9 { Ctr__AssignmentOp__0 } |
               tok__plus__eql__45 { Ctr__AssignmentOp__1 } |
               tok__minus__eql__46 { Ctr__AssignmentOp__2 } |
               tok__star__eql__47 { Ctr__AssignmentOp__3 } |
               tok__symbol__eql__48 { Ctr__AssignmentOp__4 } |
               tok__pipe__eql__49 { Ctr__AssignmentOp__5 } |
               tok__symbol__eql__50 { Ctr__AssignmentOp__6 } |
               tok__symbol__eql__51 { Ctr__AssignmentOp__7 } |
               tok__symbol__eql__52 { Ctr__AssignmentOp__8 } |
               tok__symbol__symbol__eql__53 { Ctr__AssignmentOp__9 } |
               tok__symbol__symbol__eql__54 { Ctr__AssignmentOp__10 } |
               tok__symbol__symbol__symbol__eql__55 { Ctr__AssignmentOp__11 }

CatchList : {- empty -} { [] } |
            CatchList ListElem_CatchList64 { $2 : $1 }

ClassDeclaration : qq_ClassDeclaration { Anti_ClassDeclaration $1 } |
                   ModifierList tok_class_12 id TypeParameters Rule_22 Rule_23 tok__symbol__13 FieldDeclarationList tok__symbol__14 { Ctr__ClassDeclaration__0 (reverse $1) $3 $4 $5 $6 (reverse $8) }

CompilationUnit : qq_CompilationUnit { Anti_CompilationUnit $1 } |
                  Rule_4 ImportList Rule_6 { Ctr__CompilationUnit__0 $1 (reverse $2) $3 }

CompoundName : qq_CompoundName { Anti_CompoundName $1 } |
               id Rule_90 { Ctr__CompoundName__0 $1 (reverse $2) }

CreationExpression : qq_CreationExpression { Anti_CreationExpression $1 } |
                     tok_new_82 Type Rule_76 { Ctr__CreationExpression__0 $2 $3 }

DoStatement : qq_DoStatement { Anti_DoStatement $1 } |
              tok_do_37 Statement tok_while_38 tok__lparen__6 Expression tok__rparen__7 tok__semi__1 { Ctr__DoStatement__0 $2 $5 }

DocComment : qq_DocComment { Anti_DocComment $1 } |
             doccomment { Ctr__DocComment__0 $1 }

EnumConstant : qq_EnumConstant { Anti_EnumConstant $1 } |
               AnnotationList id Rule_26 Rule_28 { Ctr__EnumConstant__0 (reverse $1) $2 $3 $4 }

EnumConstantList : qq_EnumConstantList { Anti_EnumConstantList $1 } |
                   EnumConstant Rule_30 Rule_32 { Ctr__EnumConstantList__0 $1 (reverse $2) $3 }

EnumDeclaration : qq_EnumDeclaration { Anti_EnumDeclaration $1 } |
                  ModifierList tok_enum_16 id Rule_34 tok__symbol__13 EnumConstantList Rule_35 tok__symbol__14 { Ctr__EnumDeclaration__0 (reverse $1) $3 $4 $6 $7 }

EqualityOp : qq_EqualityOp { Anti_EqualityOp $1 } |
             tok__eql__eql__62 { Ctr__EqualityOp__0 } |
             tok__exclamation__eql__63 { Ctr__EqualityOp__1 }

PrimaryNoPostfix : qq_Expression { Anti_Expression $1 } |
                   Literal { Ctr__Expression__0 $1 } |
                   tok_this_80 { Ctr__Expression__1 } |
                   tok__lparen__6 Expression tok__rparen__7 { Ctr__Expression__2 $2 } |
                   CreationExpression { Ctr__Expression__3 $1 } |
                   CompoundName Rule_72 { Ctr__Expression__4 $1 $2 } |
                   tok_super_81 tok__dot__3 id Rule_74 { Ctr__Expression__5 $3 $4 }

PostfixExpression : PrimaryNoPostfix { Ctr__Expression__6 $1 } |
                    PostfixExpression PostfixOp { Ctr__Expression__7 $1 $2 } |
                    PostfixExpression tok__dot__3 id { Ctr__Expression__8 $1 $3 } |
                    PostfixExpression tok__dot__3 id tok__lparen__6 Arglist tok__rparen__7 { Ctr__Expression__9 $1 $3 $5 } |
                    PostfixExpression tok__sq_bkt_l__17 Expression tok__sq_bkt_r__18 { Ctr__Expression__10 $1 $3 }

UnaryExpressionNotPlusMinus : PostfixExpression { Ctr__Expression__11 $1 } |
                              tok__tilde__78 UnaryExpression { Ctr__Expression__12 $2 } |
                              tok__exclamation__79 UnaryExpression { Ctr__Expression__13 $2 } |
                              CastExpression { Ctr__Expression__14 $1 }

UnaryExpression : PrefixOp UnaryExpression { Ctr__Expression__15 $1 $2 } |
                  UnaryExpressionNotPlusMinus { Ctr__Expression__16 $1 }

CastExpression : tok__lparen__6 Type tok__rparen__7 UnaryExpression { Ctr__Expression__17 $2 $4 }

MultiplicativeExpression : UnaryExpression { Ctr__Expression__18 $1 } |
                           MultiplicativeExpression MultiplicativeOp UnaryExpression { Ctr__Expression__19 $1 $2 $3 }

AdditiveExpression : MultiplicativeExpression { Ctr__Expression__20 $1 } |
                     AdditiveExpression AdditiveOp MultiplicativeExpression { Ctr__Expression__21 $1 $2 $3 }

ShiftExpression : AdditiveExpression { Ctr__Expression__22 $1 } |
                  ShiftExpression ShiftOp AdditiveExpression { Ctr__Expression__23 $1 $2 $3 }

RelationalExpression : ShiftExpression { Ctr__Expression__24 $1 } |
                       RelationalExpression RelationalOp ShiftExpression { Ctr__Expression__25 $1 $2 $3 } |
                       RelationalExpression tok_instanceof_68 Type { Ctr__Expression__26 $1 $3 }

EqualityExpression : RelationalExpression { Ctr__Expression__27 $1 } |
                     EqualityExpression EqualityOp RelationalExpression { Ctr__Expression__28 $1 $2 $3 }

AndExpression : EqualityExpression { Ctr__Expression__29 $1 } |
                AndExpression tok__symbol__61 EqualityExpression { Ctr__Expression__30 $1 $3 }

ExclusiveOrExpression : AndExpression { Ctr__Expression__31 $1 } |
                        ExclusiveOrExpression tok__symbol__60 AndExpression { Ctr__Expression__32 $1 $3 }

InclusiveOrEpression : ExclusiveOrExpression { Ctr__Expression__33 $1 } |
                       InclusiveOrEpression tok__pipe__59 ExclusiveOrExpression { Ctr__Expression__34 $1 $3 }

ConditionalAndExpression : InclusiveOrEpression { Ctr__Expression__35 $1 } |
                           ConditionalAndExpression tok__symbol__symbol__58 InclusiveOrEpression { Ctr__Expression__36 $1 $3 }

ConditionalOrExpression : ConditionalAndExpression { Ctr__Expression__37 $1 } |
                          ConditionalOrExpression tok__pipe__pipe__57 ConditionalAndExpression { Ctr__Expression__38 $1 $3 }

ConditionalExpression : ConditionalOrExpression { Ctr__Expression__39 $1 } |
                        ConditionalOrExpression tok__symbol__56 Expression tok__colon__32 ConditionalExpression { Ctr__Expression__40 $1 $3 $5 }

AssignmentExpression : ConditionalExpression Rule_70 { Ctr__Expression__41 $1 $2 }

Expression : AssignmentExpression { Ctr__Expression__42 $1 }

ExtendsList : qq_ExtendsList { Anti_ExtendsList $1 } |
              tok_extends_10 CompoundName Rule_18 { Ctr__ExtendsList__0 $2 (reverse $3) }

FieldDeclaration : qq_FieldDeclaration { Anti_FieldDeclaration $1 } |
                   OptDocComment Rule_37 { Ctr__FieldDeclaration__0 $1 $2 } |
                   tok__semi__1 { Ctr__FieldDeclaration__1 }

ListElem_FieldDeclarationList21 : qq_FieldDeclarationList { Anti_FieldDeclaration $1 } |
                                  FieldDeclaration { $1 }

FieldDeclarationList : {- empty -} { [] } |
                       FieldDeclarationList ListElem_FieldDeclarationList21 { $2 : $1 }

ForStatement : qq_ForStatement { Anti_ForStatement $1 } |
               tok_for_39 tok__lparen__6 Rule_61 OptExpression tok__semi__1 OptExpression tok__rparen__7 Statement { Ctr__ForStatement__0 $3 $4 $6 $8 }

IfStatement : qq_IfStatement { Anti_IfStatement $1 } |
              tok_if_36 tok__lparen__6 Expression tok__rparen__7 StatementWithoutIf OptElsePart { Ctr__IfStatement__0 $3 $5 $6 }

ImplementsList : qq_ImplementsList { Anti_ImplementsList $1 } |
                 tok_implements_11 Rule_20 { Ctr__ImplementsList__0 (reverse $2) }

ImportList : {- empty -} { [] } |
             ImportList ListElem_ImportList3 { $2 : $1 }

ImportStatement : qq_ImportStatement { Anti_ImportStatement $1 } |
                  tok_import_2 Rule_8 tok__semi__1 { Ctr__ImportStatement__0 $2 }

InterfaceDeclaration : qq_InterfaceDeclaration { Anti_InterfaceDeclaration $1 } |
                       ModifierList tok_interface_15 id TypeParameters Rule_24 tok__symbol__13 FieldDeclarationList tok__symbol__14 { Ctr__InterfaceDeclaration__0 (reverse $1) $3 $4 $5 (reverse $7) }

Literal : qq_Literal { Anti_Literal $1 } |
          integerLiteral { Ctr__Literal__0 $1 } |
          floatLiteral { Ctr__Literal__1 $1 } |
          tok_true_83 { Ctr__Literal__2 } |
          tok_false_84 { Ctr__Literal__3 } |
          char { Ctr__Literal__4 $1 } |
          string { Ctr__Literal__5 $1 } |
          tok_null_85 { Ctr__Literal__6 }

MemberAfterFirstId : qq_MemberAfterFirstId { Anti_MemberAfterFirstId $1 } |
                     tok__lparen__6 Rule_42 tok__rparen__7 StatementBlock { Ctr__MemberAfterFirstId__0 $2 $4 } |
                     MoreTypeSpecifier id MemberRest { Ctr__MemberAfterFirstId__1 $1 $2 $3 }

MemberDeclaration : qq_MemberDeclaration { Anti_MemberDeclaration $1 } |
                    PrimitiveTypeKeyword SquareBracketsList id MemberRest { Ctr__MemberDeclaration__0 $1 (reverse $2) $3 $4 } |
                    TypeParameters id MoreTypeSpecifier id MemberRest { Ctr__MemberDeclaration__1 $1 $2 $3 $4 $5 } |
                    id MemberAfterFirstId { Ctr__MemberDeclaration__2 $1 $2 }

MemberRest : qq_MemberRest { Anti_MemberRest $1 } |
             tok__lparen__6 Rule_43 tok__rparen__7 SquareBracketsList Rule_44 { Ctr__MemberRest__0 $2 (reverse $4) $5 } |
             SquareBracketsList OptVariableInitializer MoreVariableDeclarators tok__semi__1 { Ctr__MemberRest__1 (reverse $1) $2 (reverse $3) }

Modifier : qq_Modifier { Anti_Modifier $1 } |
           tok_public_86 { Ctr__Modifier__0 } |
           tok_private_87 { Ctr__Modifier__1 } |
           tok_protected_88 { Ctr__Modifier__2 } |
           tok_static_89 { Ctr__Modifier__3 } |
           tok_final_90 { Ctr__Modifier__4 } |
           tok_native_91 { Ctr__Modifier__5 } |
           tok_synchronized_30 { Ctr__Modifier__6 } |
           tok_abstract_92 { Ctr__Modifier__7 } |
           tok_threadsafe_93 { Ctr__Modifier__8 } |
           tok_transient_94 { Ctr__Modifier__9 }

ModifierList : {- empty -} { [] } |
               ModifierList ListElem_ModifierList17 { $2 : $1 }

MoreTypeSpecifier : qq_MoreTypeSpecifier { Anti_MoreTypeSpecifier $1 } |
                    tok__dot__3 id MoreTypeSpecifier { Ctr__MoreTypeSpecifier__0 $2 $3 } |
                    TypeArguments SquareBracketsList { Ctr__MoreTypeSpecifier__1 $1 (reverse $2) }

MoreVariableDeclarators : {- empty -} { [] } |
                          MoreVariableDeclarators ListElem_MoreVariableDeclarators48 { $2 : $1 }

MultiplicativeOp : qq_MultiplicativeOp { Anti_MultiplicativeOp $1 } |
                   tok__star__4 { Ctr__MultiplicativeOp__0 } |
                   tok__symbol__74 { Ctr__MultiplicativeOp__1 } |
                   tok__symbol__75 { Ctr__MultiplicativeOp__2 }

NestedTypeDeclaration : qq_NestedTypeDeclaration { Anti_NestedTypeDeclaration $1 } |
                        ClassDeclaration { Ctr__NestedTypeDeclaration__0 $1 } |
                        InterfaceDeclaration { Ctr__NestedTypeDeclaration__1 $1 } |
                        EnumDeclaration { Ctr__NestedTypeDeclaration__2 $1 } |
                        AnnotationDeclaration { Ctr__NestedTypeDeclaration__3 $1 }

OptDocComment : qq_OptDocComment { Anti_OptDocComment $1 } |
                { Ctr__OptDocComment__0 } |
                Rule_0 { Ctr__OptDocComment__1 $1 }

OptElsePart : qq_OptElsePart { Anti_OptElsePart $1 } |
              { Ctr__OptElsePart__0 } |
              Rule_60 { Ctr__OptElsePart__1 $1 }

OptExpression : qq_OptExpression { Anti_OptExpression $1 } |
                { Ctr__OptExpression__0 } |
                Expression { Ctr__OptExpression__1 $1 }

OptFinally : qq_OptFinally { Anti_OptFinally $1 } |
             { Ctr__OptFinally__0 } |
             Rule_65 { Ctr__OptFinally__1 $1 }

OptId : qq_OptId { Anti_OptId $1 } |
        { Ctr__OptId__0 } |
        id { Ctr__OptId__1 $1 }

OptVariableInitializer : qq_OptVariableInitializer { Anti_OptVariableInitializer $1 } |
                         { Ctr__OptVariableInitializer__0 } |
                         Rule_51 { Ctr__OptVariableInitializer__1 $1 }

Package : qq_Package { Anti_Package $1 } |
          tok_package_0 CompoundName tok__semi__1 { Ctr__Package__0 $2 }

Parameter : qq_Parameter { Anti_Parameter $1 } |
            Type id SquareBracketsList { Ctr__Parameter__0 $1 $2 (reverse $3) }

ParameterList : qq_ParameterList { Anti_ParameterList $1 } |
                Parameter Rule_57 { Ctr__ParameterList__0 $1 (reverse $2) }

PostfixOp : qq_PostfixOp { Anti_PostfixOp $1 } |
            tok__plus__plus__76 { Ctr__PostfixOp__0 } |
            tok__minus__minus__77 { Ctr__PostfixOp__1 }

PrefixOp : qq_PrefixOp { Anti_PrefixOp $1 } |
           tok__plus__plus__76 { Ctr__PrefixOp__0 } |
           tok__minus__minus__77 { Ctr__PrefixOp__1 } |
           tok__plus__72 { Ctr__PrefixOp__2 } |
           tok__minus__73 { Ctr__PrefixOp__3 }

PrimitiveTypeKeyword : qq_PrimitiveTypeKeyword { Anti_PrimitiveTypeKeyword $1 } |
                       tok_boolean_19 { Ctr__PrimitiveTypeKeyword__0 } |
                       tok_byte_20 { Ctr__PrimitiveTypeKeyword__1 } |
                       tok_char_21 { Ctr__PrimitiveTypeKeyword__2 } |
                       tok_short_22 { Ctr__PrimitiveTypeKeyword__3 } |
                       tok_int_23 { Ctr__PrimitiveTypeKeyword__4 } |
                       tok_float_24 { Ctr__PrimitiveTypeKeyword__5 } |
                       tok_long_25 { Ctr__PrimitiveTypeKeyword__6 } |
                       tok_double_26 { Ctr__PrimitiveTypeKeyword__7 } |
                       tok_void_27 { Ctr__PrimitiveTypeKeyword__8 }

RelationalOp : qq_RelationalOp { Anti_RelationalOp $1 } |
               tok__symbol__64 { Ctr__RelationalOp__0 } |
               tok__symbol__65 { Ctr__RelationalOp__1 } |
               tok__symbol__eql__66 { Ctr__RelationalOp__2 } |
               tok__symbol__eql__67 { Ctr__RelationalOp__3 }

Rule_0 : DocComment { Ctr__Rule_0__0 $1 }

Rule_1 : ClassDeclaration { Ctr__Rule_1__0 $1 } |
         InterfaceDeclaration { Ctr__Rule_1__1 $1 } |
         EnumDeclaration { Ctr__Rule_1__2 $1 } |
         AnnotationDeclaration { Ctr__Rule_1__3 $1 }

Rule_10 : { Ctr__Rule_10__0 } |
          Rule_11 { Ctr__Rule_10__1 $1 }

Rule_11 : tok__lparen__6 Rule_12 tok__rparen__7 { Ctr__Rule_11__0 $2 }

Rule_12 : { Ctr__Rule_12__0 } |
          AnnotationArguments { Ctr__Rule_12__1 $1 }

Rule_13 : {- empty -} { [] } |
          Rule_13 Rule_14 { $2 : $1 }

Rule_14 : tok__coma__8 AnnotationElement { Ctr__Rule_14__0 $2 }

ListElem_ModifierList17 : qq_ModifierList { Anti_Rule_16 $1 } |
                          Rule_16 { $1 }

Rule_16 : Modifier { Ctr__Rule_16__1 $1 } |
          Annotation { Ctr__Rule_16__2 $1 }

Rule_18 : {- empty -} { [] } |
          Rule_18 Rule_19 { $2 : $1 }

Rule_19 : tok__coma__8 CompoundName { Ctr__Rule_19__0 $2 }

ListElem_ImportList3 : qq_ImportList { Anti_Rule_2 $1 } |
                       Rule_2 { $1 }

Rule_2 : ImportStatement { Ctr__Rule_2__1 $1 }

Rule_20 : CompoundName { [$1] } |
          Rule_20 tok__coma__8 CompoundName { $3 : $1 }

Rule_22 : { Ctr__Rule_22__0 } |
          ExtendsList { Ctr__Rule_22__1 $1 }

Rule_23 : { Ctr__Rule_23__0 } |
          ImplementsList { Ctr__Rule_23__1 $1 }

Rule_24 : { Ctr__Rule_24__0 } |
          ExtendsList { Ctr__Rule_24__1 $1 }

Rule_26 : { Ctr__Rule_26__0 } |
          Rule_27 { Ctr__Rule_26__1 $1 }

Rule_27 : tok__lparen__6 Arglist tok__rparen__7 { Ctr__Rule_27__0 $2 }

Rule_28 : { Ctr__Rule_28__0 } |
          Rule_29 { Ctr__Rule_28__1 $1 }

Rule_29 : tok__symbol__13 FieldDeclarationList tok__symbol__14 { Ctr__Rule_29__0 (reverse $2) }

Rule_30 : {- empty -} { [] } |
          Rule_30 Rule_31 { $2 : $1 }

Rule_31 : tok__coma__8 EnumConstant { Ctr__Rule_31__0 $2 }

Rule_32 : { Ctr__Rule_32__0 } |
          Rule_33 { Ctr__Rule_32__1 $1 }

Rule_33 : tok__coma__8 { Ctr__Rule_33__0 }

Rule_34 : { Ctr__Rule_34__0 } |
          ImplementsList { Ctr__Rule_34__1 $1 }

Rule_35 : { Ctr__Rule_35__0 } |
          Rule_36 { Ctr__Rule_35__1 $1 }

Rule_36 : tok__semi__1 FieldDeclarationList { Ctr__Rule_36__0 (reverse $2) }

Rule_37 : ModifierList Rule_38 { Ctr__Rule_37__0 (reverse $1) $2 }

Rule_38 : Rule_39 { Ctr__Rule_38__0 $1 } |
          StaticInitializer { Ctr__Rule_38__1 $1 }

Rule_39 : MemberDeclaration { Ctr__Rule_39__0 $1 } |
          NestedTypeDeclaration { Ctr__Rule_39__1 $1 }

Rule_4 : { Ctr__Rule_4__0 } |
         Rule_5 { Ctr__Rule_4__1 $1 }

ListElem_SquareBracketsList41 : qq_SquareBracketsList { Anti_Rule_40 $1 } |
                                Rule_40 { $1 }

Rule_40 : tok__sq_bkt_l__17 OptExpression tok__sq_bkt_r__18 { Ctr__Rule_40__1 $2 }

Rule_42 : { Ctr__Rule_42__0 } |
          ParameterList { Ctr__Rule_42__1 $1 }

Rule_43 : { Ctr__Rule_43__0 } |
          ParameterList { Ctr__Rule_43__1 $1 }

Rule_44 : StatementBlock { Ctr__Rule_44__0 $1 } |
          Rule_45 tok__semi__1 { Ctr__Rule_44__1 $1 }

Rule_45 : { Ctr__Rule_45__0 } |
          Rule_46 { Ctr__Rule_45__1 $1 }

Rule_46 : tok_default_28 Expression { Ctr__Rule_46__0 $2 }

ListElem_MoreVariableDeclarators48 : qq_MoreVariableDeclarators { Anti_Rule_47 $1 } |
                                     Rule_47 { $1 }

Rule_47 : tok__coma__8 VariableDeclarator { Ctr__Rule_47__1 $2 }

Rule_49 : {- empty -} { [] } |
          Rule_49 Rule_50 { $2 : $1 }

Rule_5 : Package { Ctr__Rule_5__0 $1 }

Rule_50 : tok__coma__8 VariableDeclarator { Ctr__Rule_50__0 $2 }

Rule_51 : tok__eql__9 VariableInitializer { Ctr__Rule_51__0 $2 }

Rule_52 : VariableInitializer Rule_53 Rule_55 { Ctr__Rule_52__0 $1 (reverse $2) $3 }

Rule_53 : {- empty -} { [] } |
          Rule_53 Rule_54 { $2 : $1 }

Rule_54 : tok__coma__8 VariableInitializer { Ctr__Rule_54__0 $2 }

Rule_55 : { Ctr__Rule_55__0 } |
          Rule_56 { Ctr__Rule_55__1 $1 }

Rule_56 : tok__coma__8 { Ctr__Rule_56__0 }

Rule_57 : {- empty -} { [] } |
          Rule_57 Rule_58 { $2 : $1 }

Rule_58 : tok__coma__8 Parameter { Ctr__Rule_58__0 $2 }

Rule_6 : { Ctr__Rule_6__0 } |
         Rule_7 { Ctr__Rule_6__1 $1 }

Rule_60 : tok_else_35 Statement { Ctr__Rule_60__0 $2 }

Rule_61 : VariableDeclaration { Ctr__Rule_61__0 $1 } |
          Rule_62 { Ctr__Rule_61__1 $1 } |
          tok__semi__1 { Ctr__Rule_61__2 }

Rule_62 : Expression tok__semi__1 { Ctr__Rule_62__0 $1 }

ListElem_CatchList64 : qq_CatchList { Anti_Rule_63 $1 } |
                       Rule_63 { $1 }

Rule_63 : tok_catch_40 tok__lparen__6 Parameter tok__rparen__7 Statement { Ctr__Rule_63__1 $3 $5 }

Rule_65 : tok_finally_41 Statement { Ctr__Rule_65__0 $2 }

ListElem_SwitchCaseList69 : qq_SwitchCaseList { Anti_Rule_66 $1 } |
                            Rule_66 { $1 }

Rule_66 : Rule_67 { Ctr__Rule_66__1 $1 } |
          Rule_68 { Ctr__Rule_66__2 $1 } |
          Statement { Ctr__Rule_66__3 $1 }

Rule_67 : tok_case_43 Expression tok__colon__32 { Ctr__Rule_67__0 $2 }

Rule_68 : tok_default_28 tok__colon__32 { Ctr__Rule_68__0 }

Rule_7 : TypeDeclaration { Ctr__Rule_7__0 $1 }

Rule_70 : { Ctr__Rule_70__0 } |
          Rule_71 { Ctr__Rule_70__1 $1 }

Rule_71 : AssignmentOp AssignmentExpression { Ctr__Rule_71__0 $1 $2 }

Rule_72 : { Ctr__Rule_72__0 } |
          Rule_73 { Ctr__Rule_72__1 $1 }

Rule_73 : tok__lparen__6 Arglist tok__rparen__7 { Ctr__Rule_73__0 $2 }

Rule_74 : { Ctr__Rule_74__0 } |
          Rule_75 { Ctr__Rule_74__1 $1 }

Rule_75 : tok__lparen__6 Arglist tok__rparen__7 { Ctr__Rule_75__0 $2 }

Rule_76 : tok__lparen__6 Arglist tok__rparen__7 { Ctr__Rule_76__0 $2 } |
          SquareBracketsList { Ctr__Rule_76__1 (reverse $1) }

Rule_77 : Expression Rule_78 { Ctr__Rule_77__0 $1 (reverse $2) }

Rule_78 : {- empty -} { [] } |
          Rule_78 Rule_79 { $2 : $1 }

Rule_79 : tok__coma__8 Expression { Ctr__Rule_79__0 $2 }

Rule_8 : Rule_9 { Ctr__Rule_8__0 $1 } |
         CompoundName { Ctr__Rule_8__1 $1 }

Rule_80 : tok__symbol__64 TypeArgument Rule_81 tok__symbol__65 { Ctr__Rule_80__0 $2 (reverse $3) }

Rule_81 : {- empty -} { [] } |
          Rule_81 Rule_82 { $2 : $1 }

Rule_82 : tok__coma__8 TypeArgument { Ctr__Rule_82__0 $2 }

Rule_83 : tok__symbol__64 TypeParameter Rule_84 tok__symbol__65 { Ctr__Rule_83__0 $2 (reverse $3) }

Rule_84 : {- empty -} { [] } |
          Rule_84 Rule_85 { $2 : $1 }

Rule_85 : tok__coma__8 TypeParameter { Ctr__Rule_85__0 $2 }

Rule_86 : { Ctr__Rule_86__0 } |
          Rule_87 { Ctr__Rule_86__1 $1 }

Rule_87 : tok_extends_10 Type Rule_88 { Ctr__Rule_87__0 $2 (reverse $3) }

Rule_88 : {- empty -} { [] } |
          Rule_88 Rule_89 { $2 : $1 }

Rule_89 : tok__symbol__61 Type { Ctr__Rule_89__0 $2 }

Rule_9 : CompoundName tok__dot__3 tok__star__4 { Ctr__Rule_9__0 $1 }

Rule_90 : {- empty -} { [] } |
          Rule_90 Rule_91 { $2 : $1 }

Rule_91 : tok__dot__3 id { Ctr__Rule_91__0 $2 }

ShiftOp : qq_ShiftOp { Anti_ShiftOp $1 } |
          tok__symbol__symbol__69 { Ctr__ShiftOp__0 } |
          tok__symbol__symbol__70 { Ctr__ShiftOp__1 } |
          tok__symbol__symbol__symbol__71 { Ctr__ShiftOp__2 }

SquareBracketsList : {- empty -} { [] } |
                     SquareBracketsList ListElem_SquareBracketsList41 { $2 : $1 }

Statement : qq_Statement { Anti_Statement $1 } |
            StatementWithoutIf { Ctr__Statement__0 $1 } |
            IfStatement { Ctr__Statement__1 $1 }

ListElem_StatementList59 : qq_StatementList { Anti_Statement $1 } |
                           Statement { $1 }

StatementBlock : qq_StatementBlock { Anti_StatementBlock $1 } |
                 tok__symbol__13 StatementList tok__symbol__14 { Ctr__StatementBlock__0 (reverse $2) }

StatementList : {- empty -} { [] } |
                StatementList ListElem_StatementList59 { $2 : $1 }

StatementWithoutIf : qq_StatementWithoutIf { Anti_StatementWithoutIf $1 } |
                     VariableDeclaration { Ctr__StatementWithoutIf__0 $1 } |
                     tok_return_29 OptExpression tok__semi__1 { Ctr__StatementWithoutIf__1 $2 } |
                     Expression tok__semi__1 { Ctr__StatementWithoutIf__2 $1 } |
                     StatementBlock { Ctr__StatementWithoutIf__3 $1 } |
                     DoStatement { Ctr__StatementWithoutIf__4 $1 } |
                     WhileStatement { Ctr__StatementWithoutIf__5 $1 } |
                     ForStatement { Ctr__StatementWithoutIf__6 $1 } |
                     TryStatement { Ctr__StatementWithoutIf__7 $1 } |
                     SwitchStatement { Ctr__StatementWithoutIf__8 $1 } |
                     tok_synchronized_30 tok__lparen__6 Expression tok__rparen__7 Statement { Ctr__StatementWithoutIf__9 $3 $5 } |
                     tok_throw_31 Expression tok__semi__1 { Ctr__StatementWithoutIf__10 $2 } |
                     id tok__colon__32 Statement { Ctr__StatementWithoutIf__11 $1 $3 } |
                     tok_break_33 OptId tok__semi__1 { Ctr__StatementWithoutIf__12 $2 } |
                     tok_continue_34 OptId tok__semi__1 { Ctr__StatementWithoutIf__13 $2 } |
                     tok__semi__1 { Ctr__StatementWithoutIf__14 }

StaticInitializer : qq_StaticInitializer { Anti_StaticInitializer $1 } |
                    StatementBlock { Ctr__StaticInitializer__0 $1 }

SwitchCaseList : {- empty -} { [] } |
                 SwitchCaseList ListElem_SwitchCaseList69 { $2 : $1 }

SwitchStatement : qq_SwitchStatement { Anti_SwitchStatement $1 } |
                  tok_switch_44 tok__lparen__6 Expression tok__rparen__7 tok__symbol__13 SwitchCaseList tok__symbol__14 { Ctr__SwitchStatement__0 $3 (reverse $6) }

TryStatement : qq_TryStatement { Anti_TryStatement $1 } |
               tok_try_42 Statement CatchList OptFinally { Ctr__TryStatement__0 $2 (reverse $3) $4 }

Type : qq_Type { Anti_Type $1 } |
       TypeSpecifier SquareBracketsList { Ctr__Type__0 $1 (reverse $2) }

TypeArgument : qq_TypeArgument { Anti_TypeArgument $1 } |
               Type { Ctr__TypeArgument__0 $1 } |
               WildcardType { Ctr__TypeArgument__1 $1 }

TypeArguments : qq_TypeArguments { Anti_TypeArguments $1 } |
                { Ctr__TypeArguments__0 } |
                Rule_80 { Ctr__TypeArguments__1 $1 }

TypeDeclaration : qq_TypeDeclaration { Anti_TypeDeclaration $1 } |
                  OptDocComment Rule_1 { Ctr__TypeDeclaration__0 $1 $2 }

TypeParameter : qq_TypeParameter { Anti_TypeParameter $1 } |
                id Rule_86 { Ctr__TypeParameter__0 $1 $2 }

TypeParameters : qq_TypeParameters { Anti_TypeParameters $1 } |
                 { Ctr__TypeParameters__0 } |
                 Rule_83 { Ctr__TypeParameters__1 $1 }

TypeSpecifier : qq_TypeSpecifier { Anti_TypeSpecifier $1 } |
                tok_boolean_19 { Ctr__TypeSpecifier__0 } |
                tok_byte_20 { Ctr__TypeSpecifier__1 } |
                tok_char_21 { Ctr__TypeSpecifier__2 } |
                tok_short_22 { Ctr__TypeSpecifier__3 } |
                tok_int_23 { Ctr__TypeSpecifier__4 } |
                tok_float_24 { Ctr__TypeSpecifier__5 } |
                tok_long_25 { Ctr__TypeSpecifier__6 } |
                tok_double_26 { Ctr__TypeSpecifier__7 } |
                tok_void_27 { Ctr__TypeSpecifier__8 } |
                CompoundName TypeArguments { Ctr__TypeSpecifier__9 $1 $2 }

VariableDeclaration : qq_VariableDeclaration { Anti_VariableDeclaration $1 } |
                      Type VariableDeclaratorList tok__semi__1 { Ctr__VariableDeclaration__0 $1 $2 }

VariableDeclarator : qq_VariableDeclarator { Anti_VariableDeclarator $1 } |
                     id SquareBracketsList OptVariableInitializer { Ctr__VariableDeclarator__0 $1 (reverse $2) $3 }

VariableDeclaratorList : qq_VariableDeclaratorList { Anti_VariableDeclaratorList $1 } |
                         VariableDeclarator Rule_49 { Ctr__VariableDeclaratorList__0 $1 (reverse $2) }

VariableInitializer : qq_VariableInitializer { Anti_VariableInitializer $1 } |
                      Expression { Ctr__VariableInitializer__0 $1 } |
                      tok__symbol__13 VariableInitializerList tok__symbol__14 { Ctr__VariableInitializer__1 $2 }

VariableInitializerList : qq_VariableInitializerList { Anti_VariableInitializerList $1 } |
                          { Ctr__VariableInitializerList__0 } |
                          Rule_52 { Ctr__VariableInitializerList__1 $1 }

WhileStatement : qq_WhileStatement { Anti_WhileStatement $1 } |
                 tok_while_38 tok__lparen__6 Expression tok__rparen__7 Statement { Ctr__WhileStatement__0 $3 $5 }

WildcardType : qq_WildcardType { Anti_WildcardType $1 } |
               tok__symbol__56 { Ctr__WildcardType__0 } |
               tok__symbol__56 tok_extends_10 Type { Ctr__WildcardType__1 $3 } |
               tok__symbol__56 tok_super_81 Type { Ctr__WildcardType__2 $3 }


{
parseError :: [L.PosToken] -> Either String a
parseError [] = Left "Parse error: unexpected end of input"
parseError (L.PosToken (L.AlexPn _ line col) tok : _) =
    Left $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ ": unexpected " ++ showRtkToken tok

-- Render a token the way it appears in the source, for error messages
showRtkToken :: L.Token -> String
showRtkToken L.EndOfFile = "end of input"
showRtkToken L.Tk__tok_AdditiveOp_dummy_169 = "'tok_AdditiveOp_dummy_169'"
showRtkToken L.Tk__tok_Annotation_dummy_168 = "'tok_Annotation_dummy_168'"
showRtkToken L.Tk__tok_AnnotationArguments_dummy_167 = "'tok_AnnotationArguments_dummy_167'"
showRtkToken L.Tk__tok_AnnotationDeclaration_dummy_166 = "'tok_AnnotationDeclaration_dummy_166'"
showRtkToken L.Tk__tok_AnnotationElement_dummy_165 = "'tok_AnnotationElement_dummy_165'"
showRtkToken L.Tk__tok_AnnotationList_dummy_164 = "'tok_AnnotationList_dummy_164'"
showRtkToken L.Tk__tok_AnnotationTypeElement_dummy_163 = "'tok_AnnotationTypeElement_dummy_163'"
showRtkToken L.Tk__tok_AnnotationTypeElementList_dummy_162 = "'tok_AnnotationTypeElementList_dummy_162'"
showRtkToken L.Tk__tok_Arglist_dummy_161 = "'tok_Arglist_dummy_161'"
showRtkToken L.Tk__tok_AssignmentOp_dummy_160 = "'tok_AssignmentOp_dummy_160'"
showRtkToken L.Tk__tok_CatchList_dummy_159 = "'tok_CatchList_dummy_159'"
showRtkToken L.Tk__tok_ClassDeclaration_dummy_158 = "'tok_ClassDeclaration_dummy_158'"
showRtkToken L.Tk__tok_CompilationUnit_dummy_157 = "'tok_CompilationUnit_dummy_157'"
showRtkToken L.Tk__tok_CompoundName_dummy_156 = "'tok_CompoundName_dummy_156'"
showRtkToken L.Tk__tok_CreationExpression_dummy_155 = "'tok_CreationExpression_dummy_155'"
showRtkToken L.Tk__tok_DoStatement_dummy_154 = "'tok_DoStatement_dummy_154'"
showRtkToken L.Tk__tok_DocComment_dummy_153 = "'tok_DocComment_dummy_153'"
showRtkToken L.Tk__tok_EnumConstant_dummy_152 = "'tok_EnumConstant_dummy_152'"
showRtkToken L.Tk__tok_EnumConstantList_dummy_151 = "'tok_EnumConstantList_dummy_151'"
showRtkToken L.Tk__tok_EnumDeclaration_dummy_150 = "'tok_EnumDeclaration_dummy_150'"
showRtkToken L.Tk__tok_EqualityOp_dummy_149 = "'tok_EqualityOp_dummy_149'"
showRtkToken L.Tk__tok_Expression_dummy_148 = "'tok_Expression_dummy_148'"
showRtkToken L.Tk__tok_ExtendsList_dummy_147 = "'tok_ExtendsList_dummy_147'"
showRtkToken L.Tk__tok_FieldDeclaration_dummy_146 = "'tok_FieldDeclaration_dummy_146'"
showRtkToken L.Tk__tok_FieldDeclarationList_dummy_145 = "'tok_FieldDeclarationList_dummy_145'"
showRtkToken L.Tk__tok_ForStatement_dummy_144 = "'tok_ForStatement_dummy_144'"
showRtkToken L.Tk__tok_IfStatement_dummy_143 = "'tok_IfStatement_dummy_143'"
showRtkToken L.Tk__tok_ImplementsList_dummy_142 = "'tok_ImplementsList_dummy_142'"
showRtkToken L.Tk__tok_ImportList_dummy_141 = "'tok_ImportList_dummy_141'"
showRtkToken L.Tk__tok_ImportStatement_dummy_140 = "'tok_ImportStatement_dummy_140'"
showRtkToken L.Tk__tok_InterfaceDeclaration_dummy_139 = "'tok_InterfaceDeclaration_dummy_139'"
showRtkToken L.Tk__tok_Java_dummy_170 = "'tok_Java_dummy_170'"
showRtkToken L.Tk__tok_Literal_dummy_138 = "'tok_Literal_dummy_138'"
showRtkToken L.Tk__tok_MemberAfterFirstId_dummy_137 = "'tok_MemberAfterFirstId_dummy_137'"
showRtkToken L.Tk__tok_MemberDeclaration_dummy_136 = "'tok_MemberDeclaration_dummy_136'"
showRtkToken L.Tk__tok_MemberRest_dummy_135 = "'tok_MemberRest_dummy_135'"
showRtkToken L.Tk__tok_Modifier_dummy_134 = "'tok_Modifier_dummy_134'"
showRtkToken L.Tk__tok_ModifierList_dummy_133 = "'tok_ModifierList_dummy_133'"
showRtkToken L.Tk__tok_MoreTypeSpecifier_dummy_132 = "'tok_MoreTypeSpecifier_dummy_132'"
showRtkToken L.Tk__tok_MoreVariableDeclarators_dummy_131 = "'tok_MoreVariableDeclarators_dummy_131'"
showRtkToken L.Tk__tok_MultiplicativeOp_dummy_130 = "'tok_MultiplicativeOp_dummy_130'"
showRtkToken L.Tk__tok_NestedTypeDeclaration_dummy_129 = "'tok_NestedTypeDeclaration_dummy_129'"
showRtkToken L.Tk__tok_OptDocComment_dummy_128 = "'tok_OptDocComment_dummy_128'"
showRtkToken L.Tk__tok_OptElsePart_dummy_127 = "'tok_OptElsePart_dummy_127'"
showRtkToken L.Tk__tok_OptExpression_dummy_126 = "'tok_OptExpression_dummy_126'"
showRtkToken L.Tk__tok_OptFinally_dummy_125 = "'tok_OptFinally_dummy_125'"
showRtkToken L.Tk__tok_OptId_dummy_124 = "'tok_OptId_dummy_124'"
showRtkToken L.Tk__tok_OptVariableInitializer_dummy_123 = "'tok_OptVariableInitializer_dummy_123'"
showRtkToken L.Tk__tok_Package_dummy_122 = "'tok_Package_dummy_122'"
showRtkToken L.Tk__tok_Parameter_dummy_121 = "'tok_Parameter_dummy_121'"
showRtkToken L.Tk__tok_ParameterList_dummy_120 = "'tok_ParameterList_dummy_120'"
showRtkToken L.Tk__tok_PostfixOp_dummy_119 = "'tok_PostfixOp_dummy_119'"
showRtkToken L.Tk__tok_PrefixOp_dummy_118 = "'tok_PrefixOp_dummy_118'"
showRtkToken L.Tk__tok_PrimitiveTypeKeyword_dummy_117 = "'tok_PrimitiveTypeKeyword_dummy_117'"
showRtkToken L.Tk__tok_RelationalOp_dummy_116 = "'tok_RelationalOp_dummy_116'"
showRtkToken L.Tk__tok_ShiftOp_dummy_115 = "'tok_ShiftOp_dummy_115'"
showRtkToken L.Tk__tok_SquareBracketsList_dummy_114 = "'tok_SquareBracketsList_dummy_114'"
showRtkToken L.Tk__tok_Statement_dummy_113 = "'tok_Statement_dummy_113'"
showRtkToken L.Tk__tok_StatementBlock_dummy_112 = "'tok_StatementBlock_dummy_112'"
showRtkToken L.Tk__tok_StatementList_dummy_111 = "'tok_StatementList_dummy_111'"
showRtkToken L.Tk__tok_StatementWithoutIf_dummy_110 = "'tok_StatementWithoutIf_dummy_110'"
showRtkToken L.Tk__tok_StaticInitializer_dummy_109 = "'tok_StaticInitializer_dummy_109'"
showRtkToken L.Tk__tok_SwitchCaseList_dummy_108 = "'tok_SwitchCaseList_dummy_108'"
showRtkToken L.Tk__tok_SwitchStatement_dummy_107 = "'tok_SwitchStatement_dummy_107'"
showRtkToken L.Tk__tok_TryStatement_dummy_106 = "'tok_TryStatement_dummy_106'"
showRtkToken L.Tk__tok_Type_dummy_105 = "'tok_Type_dummy_105'"
showRtkToken L.Tk__tok_TypeArgument_dummy_104 = "'tok_TypeArgument_dummy_104'"
showRtkToken L.Tk__tok_TypeArguments_dummy_103 = "'tok_TypeArguments_dummy_103'"
showRtkToken L.Tk__tok_TypeDeclaration_dummy_102 = "'tok_TypeDeclaration_dummy_102'"
showRtkToken L.Tk__tok_TypeParameter_dummy_101 = "'tok_TypeParameter_dummy_101'"
showRtkToken L.Tk__tok_TypeParameters_dummy_100 = "'tok_TypeParameters_dummy_100'"
showRtkToken L.Tk__tok_TypeSpecifier_dummy_99 = "'tok_TypeSpecifier_dummy_99'"
showRtkToken L.Tk__tok_VariableDeclaration_dummy_98 = "'tok_VariableDeclaration_dummy_98'"
showRtkToken L.Tk__tok_VariableDeclarator_dummy_97 = "'tok_VariableDeclarator_dummy_97'"
showRtkToken L.Tk__tok_VariableDeclaratorList_dummy_96 = "'tok_VariableDeclaratorList_dummy_96'"
showRtkToken L.Tk__tok_VariableInitializer_dummy_95 = "'tok_VariableInitializer_dummy_95'"
showRtkToken L.Tk__tok_VariableInitializerList_dummy_94 = "'tok_VariableInitializerList_dummy_94'"
showRtkToken L.Tk__tok_WhileStatement_dummy_93 = "'tok_WhileStatement_dummy_93'"
showRtkToken L.Tk__tok_WildcardType_dummy_92 = "'tok_WildcardType_dummy_92'"
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
showRtkToken (L.Tk__qq_TypeArguments v) = "qq_TypeArguments " ++ show v
showRtkToken (L.Tk__qq_Arglist v) = "qq_Arglist " ++ show v
showRtkToken (L.Tk__qq_Literal v) = "qq_Literal " ++ show v
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
showRtkToken (L.Tk__qq_StatementWithoutIf v) = "qq_StatementWithoutIf " ++ show v
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
showRtkToken (L.Tk__qq_SquareBracketsList v) = "qq_SquareBracketsList " ++ show v
showRtkToken (L.Tk__qq_FieldDeclaration v) = "qq_FieldDeclaration " ++ show v
showRtkToken (L.Tk__qq_NestedTypeDeclaration v) = "qq_NestedTypeDeclaration " ++ show v
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

data Java = Ctr__Java__0 Java |
            Ctr__Java__1 AdditiveOp |
            Ctr__Java__2 Annotation |
            Ctr__Java__3 AnnotationArguments |
            Ctr__Java__4 AnnotationDeclaration |
            Ctr__Java__5 AnnotationElement |
            Ctr__Java__6 AnnotationList |
            Ctr__Java__7 AnnotationTypeElement |
            Ctr__Java__8 AnnotationTypeElementList |
            Ctr__Java__9 Arglist |
            Ctr__Java__10 AssignmentOp |
            Ctr__Java__11 CatchList |
            Ctr__Java__12 ClassDeclaration |
            Ctr__Java__13 CompilationUnit |
            Ctr__Java__14 CompoundName |
            Ctr__Java__15 CreationExpression |
            Ctr__Java__16 DoStatement |
            Ctr__Java__17 DocComment |
            Ctr__Java__18 EnumConstant |
            Ctr__Java__19 EnumConstantList |
            Ctr__Java__20 EnumDeclaration |
            Ctr__Java__21 EqualityOp |
            Ctr__Java__22 Expression |
            Ctr__Java__23 ExtendsList |
            Ctr__Java__24 FieldDeclaration |
            Ctr__Java__25 FieldDeclarationList |
            Ctr__Java__26 ForStatement |
            Ctr__Java__27 IfStatement |
            Ctr__Java__28 ImplementsList |
            Ctr__Java__29 ImportList |
            Ctr__Java__30 ImportStatement |
            Ctr__Java__31 InterfaceDeclaration |
            Ctr__Java__32 Literal |
            Ctr__Java__33 MemberAfterFirstId |
            Ctr__Java__34 MemberDeclaration |
            Ctr__Java__35 MemberRest |
            Ctr__Java__36 Modifier |
            Ctr__Java__37 ModifierList |
            Ctr__Java__38 MoreTypeSpecifier |
            Ctr__Java__39 MoreVariableDeclarators |
            Ctr__Java__40 MultiplicativeOp |
            Ctr__Java__41 NestedTypeDeclaration |
            Ctr__Java__42 OptDocComment |
            Ctr__Java__43 OptElsePart |
            Ctr__Java__44 OptExpression |
            Ctr__Java__45 OptFinally |
            Ctr__Java__46 OptId |
            Ctr__Java__47 OptVariableInitializer |
            Ctr__Java__48 Package |
            Ctr__Java__49 Parameter |
            Ctr__Java__50 ParameterList |
            Ctr__Java__51 PostfixOp |
            Ctr__Java__52 PrefixOp |
            Ctr__Java__53 PrimitiveTypeKeyword |
            Ctr__Java__54 RelationalOp |
            Ctr__Java__55 ShiftOp |
            Ctr__Java__56 SquareBracketsList |
            Ctr__Java__57 Statement |
            Ctr__Java__58 StatementBlock |
            Ctr__Java__59 StatementList |
            Ctr__Java__60 StatementWithoutIf |
            Ctr__Java__61 StaticInitializer |
            Ctr__Java__62 SwitchCaseList |
            Ctr__Java__63 SwitchStatement |
            Ctr__Java__64 TryStatement |
            Ctr__Java__65 Type |
            Ctr__Java__66 TypeArgument |
            Ctr__Java__67 TypeArguments |
            Ctr__Java__68 TypeDeclaration |
            Ctr__Java__69 TypeParameter |
            Ctr__Java__70 TypeParameters |
            Ctr__Java__71 TypeSpecifier |
            Ctr__Java__72 VariableDeclaration |
            Ctr__Java__73 VariableDeclarator |
            Ctr__Java__74 VariableDeclaratorList |
            Ctr__Java__75 VariableInitializer |
            Ctr__Java__76 VariableInitializerList |
            Ctr__Java__77 WhileStatement |
            Ctr__Java__78 WildcardType |
            Anti_Java String |
            Ctr__Java__79 CompilationUnit
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data AdditiveOp = Anti_AdditiveOp String |
                  Ctr__AdditiveOp__0 |
                  Ctr__AdditiveOp__1
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Annotation = Anti_Annotation String |
                  Ctr__Annotation__1 CompoundName Rule_10
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data AnnotationArguments = Anti_AnnotationArguments String |
                           Ctr__AnnotationArguments__0 AnnotationElement Rule_13
                           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data AnnotationDeclaration = Anti_AnnotationDeclaration String |
                             Ctr__AnnotationDeclaration__0 ModifierList String AnnotationTypeElementList
                             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data AnnotationElement = Anti_AnnotationElement String |
                         Ctr__AnnotationElement__0 String Expression |
                         Ctr__AnnotationElement__1 Expression
                         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type AnnotationList = [Annotation]
data AnnotationTypeElement = Anti_AnnotationTypeElement String |
                             Ctr__AnnotationTypeElement__0 FieldDeclaration
                             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type AnnotationTypeElementList = [AnnotationTypeElement]
data Arglist = Anti_Arglist String |
               Ctr__Arglist__0 |
               Ctr__Arglist__1 Rule_77
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data AssignmentOp = Anti_AssignmentOp String |
                    Ctr__AssignmentOp__0 |
                    Ctr__AssignmentOp__1 |
                    Ctr__AssignmentOp__2 |
                    Ctr__AssignmentOp__3 |
                    Ctr__AssignmentOp__4 |
                    Ctr__AssignmentOp__5 |
                    Ctr__AssignmentOp__6 |
                    Ctr__AssignmentOp__7 |
                    Ctr__AssignmentOp__8 |
                    Ctr__AssignmentOp__9 |
                    Ctr__AssignmentOp__10 |
                    Ctr__AssignmentOp__11
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type CatchList = [Rule_63]
data ClassDeclaration = Anti_ClassDeclaration String |
                        Ctr__ClassDeclaration__0 ModifierList String TypeParameters Rule_22 Rule_23 FieldDeclarationList
                        deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data CompilationUnit = Anti_CompilationUnit String |
                       Ctr__CompilationUnit__0 Rule_4 ImportList Rule_6
                       deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data CompoundName = Anti_CompoundName String |
                    Ctr__CompoundName__0 String Rule_90
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data CreationExpression = Anti_CreationExpression String |
                          Ctr__CreationExpression__0 Type Rule_76
                          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data DoStatement = Anti_DoStatement String |
                   Ctr__DoStatement__0 Statement Expression
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data DocComment = Anti_DocComment String |
                  Ctr__DocComment__0 String
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data EnumConstant = Anti_EnumConstant String |
                    Ctr__EnumConstant__0 AnnotationList String Rule_26 Rule_28
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data EnumConstantList = Anti_EnumConstantList String |
                        Ctr__EnumConstantList__0 EnumConstant Rule_30 Rule_32
                        deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data EnumDeclaration = Anti_EnumDeclaration String |
                       Ctr__EnumDeclaration__0 ModifierList String Rule_34 EnumConstantList Rule_35
                       deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data EqualityOp = Anti_EqualityOp String |
                  Ctr__EqualityOp__0 |
                  Ctr__EqualityOp__1
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Expression = Anti_Expression String |
                  Ctr__Expression__0 Literal |
                  Ctr__Expression__1 |
                  Ctr__Expression__2 Expression |
                  Ctr__Expression__3 CreationExpression |
                  Ctr__Expression__4 CompoundName Rule_72 |
                  Ctr__Expression__5 String Rule_74 |
                  Ctr__Expression__6 Expression |
                  Ctr__Expression__7 Expression PostfixOp |
                  Ctr__Expression__8 Expression String |
                  Ctr__Expression__9 Expression String Arglist |
                  Ctr__Expression__10 Expression Expression |
                  Ctr__Expression__11 Expression |
                  Ctr__Expression__12 Expression |
                  Ctr__Expression__13 Expression |
                  Ctr__Expression__14 Expression |
                  Ctr__Expression__15 PrefixOp Expression |
                  Ctr__Expression__16 Expression |
                  Ctr__Expression__17 Type Expression |
                  Ctr__Expression__18 Expression |
                  Ctr__Expression__19 Expression MultiplicativeOp Expression |
                  Ctr__Expression__20 Expression |
                  Ctr__Expression__21 Expression AdditiveOp Expression |
                  Ctr__Expression__22 Expression |
                  Ctr__Expression__23 Expression ShiftOp Expression |
                  Ctr__Expression__24 Expression |
                  Ctr__Expression__25 Expression RelationalOp Expression |
                  Ctr__Expression__26 Expression Type |
                  Ctr__Expression__27 Expression |
                  Ctr__Expression__28 Expression EqualityOp Expression |
                  Ctr__Expression__29 Expression |
                  Ctr__Expression__30 Expression Expression |
                  Ctr__Expression__31 Expression |
                  Ctr__Expression__32 Expression Expression |
                  Ctr__Expression__33 Expression |
                  Ctr__Expression__34 Expression Expression |
                  Ctr__Expression__35 Expression |
                  Ctr__Expression__36 Expression Expression |
                  Ctr__Expression__37 Expression |
                  Ctr__Expression__38 Expression Expression |
                  Ctr__Expression__39 Expression |
                  Ctr__Expression__40 Expression Expression Expression |
                  Ctr__Expression__41 Expression Rule_70 |
                  Ctr__Expression__42 Expression
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data ExtendsList = Anti_ExtendsList String |
                   Ctr__ExtendsList__0 CompoundName Rule_18
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data FieldDeclaration = Anti_FieldDeclaration String |
                        Ctr__FieldDeclaration__0 OptDocComment Rule_37 |
                        Ctr__FieldDeclaration__1
                        deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type FieldDeclarationList = [FieldDeclaration]
data ForStatement = Anti_ForStatement String |
                    Ctr__ForStatement__0 Rule_61 OptExpression OptExpression Statement
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data IfStatement = Anti_IfStatement String |
                   Ctr__IfStatement__0 Expression StatementWithoutIf OptElsePart
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data ImplementsList = Anti_ImplementsList String |
                      Ctr__ImplementsList__0 Rule_20
                      deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type ImportList = [Rule_2]
data ImportStatement = Anti_ImportStatement String |
                       Ctr__ImportStatement__0 Rule_8
                       deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data InterfaceDeclaration = Anti_InterfaceDeclaration String |
                            Ctr__InterfaceDeclaration__0 ModifierList String TypeParameters Rule_24 FieldDeclarationList
                            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Literal = Anti_Literal String |
               Ctr__Literal__0 String |
               Ctr__Literal__1 String |
               Ctr__Literal__2 |
               Ctr__Literal__3 |
               Ctr__Literal__4 String |
               Ctr__Literal__5 String |
               Ctr__Literal__6
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data MemberAfterFirstId = Anti_MemberAfterFirstId String |
                          Ctr__MemberAfterFirstId__0 Rule_42 StatementBlock |
                          Ctr__MemberAfterFirstId__1 MoreTypeSpecifier String MemberRest
                          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data MemberDeclaration = Anti_MemberDeclaration String |
                         Ctr__MemberDeclaration__0 PrimitiveTypeKeyword SquareBracketsList String MemberRest |
                         Ctr__MemberDeclaration__1 TypeParameters String MoreTypeSpecifier String MemberRest |
                         Ctr__MemberDeclaration__2 String MemberAfterFirstId
                         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data MemberRest = Anti_MemberRest String |
                  Ctr__MemberRest__0 Rule_43 SquareBracketsList Rule_44 |
                  Ctr__MemberRest__1 SquareBracketsList OptVariableInitializer MoreVariableDeclarators
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Modifier = Anti_Modifier String |
                Ctr__Modifier__0 |
                Ctr__Modifier__1 |
                Ctr__Modifier__2 |
                Ctr__Modifier__3 |
                Ctr__Modifier__4 |
                Ctr__Modifier__5 |
                Ctr__Modifier__6 |
                Ctr__Modifier__7 |
                Ctr__Modifier__8 |
                Ctr__Modifier__9
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type ModifierList = [Rule_16]
data MoreTypeSpecifier = Anti_MoreTypeSpecifier String |
                         Ctr__MoreTypeSpecifier__0 String MoreTypeSpecifier |
                         Ctr__MoreTypeSpecifier__1 TypeArguments SquareBracketsList
                         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type MoreVariableDeclarators = [Rule_47]
data MultiplicativeOp = Anti_MultiplicativeOp String |
                        Ctr__MultiplicativeOp__0 |
                        Ctr__MultiplicativeOp__1 |
                        Ctr__MultiplicativeOp__2
                        deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data NestedTypeDeclaration = Anti_NestedTypeDeclaration String |
                             Ctr__NestedTypeDeclaration__0 ClassDeclaration |
                             Ctr__NestedTypeDeclaration__1 InterfaceDeclaration |
                             Ctr__NestedTypeDeclaration__2 EnumDeclaration |
                             Ctr__NestedTypeDeclaration__3 AnnotationDeclaration
                             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptDocComment = Anti_OptDocComment String |
                     Ctr__OptDocComment__0 |
                     Ctr__OptDocComment__1 Rule_0
                     deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptElsePart = Anti_OptElsePart String |
                   Ctr__OptElsePart__0 |
                   Ctr__OptElsePart__1 Rule_60
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptExpression = Anti_OptExpression String |
                     Ctr__OptExpression__0 |
                     Ctr__OptExpression__1 Expression
                     deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptFinally = Anti_OptFinally String |
                  Ctr__OptFinally__0 |
                  Ctr__OptFinally__1 Rule_65
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptId = Anti_OptId String |
             Ctr__OptId__0 |
             Ctr__OptId__1 String
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptVariableInitializer = Anti_OptVariableInitializer String |
                              Ctr__OptVariableInitializer__0 |
                              Ctr__OptVariableInitializer__1 Rule_51
                              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Package = Anti_Package String |
               Ctr__Package__0 CompoundName
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Parameter = Anti_Parameter String |
                 Ctr__Parameter__0 Type String SquareBracketsList
                 deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data ParameterList = Anti_ParameterList String |
                     Ctr__ParameterList__0 Parameter Rule_57
                     deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data PostfixOp = Anti_PostfixOp String |
                 Ctr__PostfixOp__0 |
                 Ctr__PostfixOp__1
                 deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data PrefixOp = Anti_PrefixOp String |
                Ctr__PrefixOp__0 |
                Ctr__PrefixOp__1 |
                Ctr__PrefixOp__2 |
                Ctr__PrefixOp__3
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data PrimitiveTypeKeyword = Anti_PrimitiveTypeKeyword String |
                            Ctr__PrimitiveTypeKeyword__0 |
                            Ctr__PrimitiveTypeKeyword__1 |
                            Ctr__PrimitiveTypeKeyword__2 |
                            Ctr__PrimitiveTypeKeyword__3 |
                            Ctr__PrimitiveTypeKeyword__4 |
                            Ctr__PrimitiveTypeKeyword__5 |
                            Ctr__PrimitiveTypeKeyword__6 |
                            Ctr__PrimitiveTypeKeyword__7 |
                            Ctr__PrimitiveTypeKeyword__8
                            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data RelationalOp = Anti_RelationalOp String |
                    Ctr__RelationalOp__0 |
                    Ctr__RelationalOp__1 |
                    Ctr__RelationalOp__2 |
                    Ctr__RelationalOp__3
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_0 = Ctr__Rule_0__0 DocComment
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_1 = Ctr__Rule_1__0 ClassDeclaration |
              Ctr__Rule_1__1 InterfaceDeclaration |
              Ctr__Rule_1__2 EnumDeclaration |
              Ctr__Rule_1__3 AnnotationDeclaration
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_10 = Ctr__Rule_10__0 |
               Ctr__Rule_10__1 Rule_11
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_11 = Ctr__Rule_11__0 Rule_12
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_12 = Ctr__Rule_12__0 |
               Ctr__Rule_12__1 AnnotationArguments
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_13 = [Rule_14]
data Rule_14 = Ctr__Rule_14__0 AnnotationElement
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_16 = Anti_Rule_16 String |
               Ctr__Rule_16__1 Modifier |
               Ctr__Rule_16__2 Annotation
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_18 = [Rule_19]
data Rule_19 = Ctr__Rule_19__0 CompoundName
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_2 = Anti_Rule_2 String |
              Ctr__Rule_2__1 ImportStatement
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_20 = [CompoundName]
data Rule_22 = Ctr__Rule_22__0 |
               Ctr__Rule_22__1 ExtendsList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_23 = Ctr__Rule_23__0 |
               Ctr__Rule_23__1 ImplementsList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_24 = Ctr__Rule_24__0 |
               Ctr__Rule_24__1 ExtendsList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_26 = Ctr__Rule_26__0 |
               Ctr__Rule_26__1 Rule_27
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_27 = Ctr__Rule_27__0 Arglist
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_28 = Ctr__Rule_28__0 |
               Ctr__Rule_28__1 Rule_29
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_29 = Ctr__Rule_29__0 FieldDeclarationList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_30 = [Rule_31]
data Rule_31 = Ctr__Rule_31__0 EnumConstant
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_32 = Ctr__Rule_32__0 |
               Ctr__Rule_32__1 Rule_33
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_33 = Ctr__Rule_33__0
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_34 = Ctr__Rule_34__0 |
               Ctr__Rule_34__1 ImplementsList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_35 = Ctr__Rule_35__0 |
               Ctr__Rule_35__1 Rule_36
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_36 = Ctr__Rule_36__0 FieldDeclarationList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_37 = Ctr__Rule_37__0 ModifierList Rule_38
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_38 = Ctr__Rule_38__0 Rule_39 |
               Ctr__Rule_38__1 StaticInitializer
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_39 = Ctr__Rule_39__0 MemberDeclaration |
               Ctr__Rule_39__1 NestedTypeDeclaration
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_4 = Ctr__Rule_4__0 |
              Ctr__Rule_4__1 Rule_5
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_40 = Anti_Rule_40 String |
               Ctr__Rule_40__1 OptExpression
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_42 = Ctr__Rule_42__0 |
               Ctr__Rule_42__1 ParameterList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_43 = Ctr__Rule_43__0 |
               Ctr__Rule_43__1 ParameterList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_44 = Ctr__Rule_44__0 StatementBlock |
               Ctr__Rule_44__1 Rule_45
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_45 = Ctr__Rule_45__0 |
               Ctr__Rule_45__1 Rule_46
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_46 = Ctr__Rule_46__0 Expression
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_47 = Anti_Rule_47 String |
               Ctr__Rule_47__1 VariableDeclarator
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_49 = [Rule_50]
data Rule_5 = Ctr__Rule_5__0 Package
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_50 = Ctr__Rule_50__0 VariableDeclarator
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_51 = Ctr__Rule_51__0 VariableInitializer
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_52 = Ctr__Rule_52__0 VariableInitializer Rule_53 Rule_55
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_53 = [Rule_54]
data Rule_54 = Ctr__Rule_54__0 VariableInitializer
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_55 = Ctr__Rule_55__0 |
               Ctr__Rule_55__1 Rule_56
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_56 = Ctr__Rule_56__0
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_57 = [Rule_58]
data Rule_58 = Ctr__Rule_58__0 Parameter
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_6 = Ctr__Rule_6__0 |
              Ctr__Rule_6__1 Rule_7
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_60 = Ctr__Rule_60__0 Statement
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_61 = Ctr__Rule_61__0 VariableDeclaration |
               Ctr__Rule_61__1 Rule_62 |
               Ctr__Rule_61__2
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_62 = Ctr__Rule_62__0 Expression
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_63 = Anti_Rule_63 String |
               Ctr__Rule_63__1 Parameter Statement
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_65 = Ctr__Rule_65__0 Statement
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_66 = Anti_Rule_66 String |
               Ctr__Rule_66__1 Rule_67 |
               Ctr__Rule_66__2 Rule_68 |
               Ctr__Rule_66__3 Statement
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_67 = Ctr__Rule_67__0 Expression
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_68 = Ctr__Rule_68__0
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_7 = Ctr__Rule_7__0 TypeDeclaration
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_70 = Ctr__Rule_70__0 |
               Ctr__Rule_70__1 Rule_71
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_71 = Ctr__Rule_71__0 AssignmentOp Expression
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_72 = Ctr__Rule_72__0 |
               Ctr__Rule_72__1 Rule_73
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_73 = Ctr__Rule_73__0 Arglist
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_74 = Ctr__Rule_74__0 |
               Ctr__Rule_74__1 Rule_75
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_75 = Ctr__Rule_75__0 Arglist
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_76 = Ctr__Rule_76__0 Arglist |
               Ctr__Rule_76__1 SquareBracketsList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_77 = Ctr__Rule_77__0 Expression Rule_78
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_78 = [Rule_79]
data Rule_79 = Ctr__Rule_79__0 Expression
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_8 = Ctr__Rule_8__0 Rule_9 |
              Ctr__Rule_8__1 CompoundName
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_80 = Ctr__Rule_80__0 TypeArgument Rule_81
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_81 = [Rule_82]
data Rule_82 = Ctr__Rule_82__0 TypeArgument
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_83 = Ctr__Rule_83__0 TypeParameter Rule_84
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_84 = [Rule_85]
data Rule_85 = Ctr__Rule_85__0 TypeParameter
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_86 = Ctr__Rule_86__0 |
               Ctr__Rule_86__1 Rule_87
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_87 = Ctr__Rule_87__0 Type Rule_88
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_88 = [Rule_89]
data Rule_89 = Ctr__Rule_89__0 Type
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_9 = Ctr__Rule_9__0 CompoundName
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_90 = [Rule_91]
data Rule_91 = Ctr__Rule_91__0 String
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data ShiftOp = Anti_ShiftOp String |
               Ctr__ShiftOp__0 |
               Ctr__ShiftOp__1 |
               Ctr__ShiftOp__2
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type SquareBracketsList = [Rule_40]
data Statement = Anti_Statement String |
                 Ctr__Statement__0 StatementWithoutIf |
                 Ctr__Statement__1 IfStatement
                 deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data StatementBlock = Anti_StatementBlock String |
                      Ctr__StatementBlock__0 StatementList
                      deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type StatementList = [Statement]
data StatementWithoutIf = Anti_StatementWithoutIf String |
                          Ctr__StatementWithoutIf__0 VariableDeclaration |
                          Ctr__StatementWithoutIf__1 OptExpression |
                          Ctr__StatementWithoutIf__2 Expression |
                          Ctr__StatementWithoutIf__3 StatementBlock |
                          Ctr__StatementWithoutIf__4 DoStatement |
                          Ctr__StatementWithoutIf__5 WhileStatement |
                          Ctr__StatementWithoutIf__6 ForStatement |
                          Ctr__StatementWithoutIf__7 TryStatement |
                          Ctr__StatementWithoutIf__8 SwitchStatement |
                          Ctr__StatementWithoutIf__9 Expression Statement |
                          Ctr__StatementWithoutIf__10 Expression |
                          Ctr__StatementWithoutIf__11 String Statement |
                          Ctr__StatementWithoutIf__12 OptId |
                          Ctr__StatementWithoutIf__13 OptId |
                          Ctr__StatementWithoutIf__14
                          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data StaticInitializer = Anti_StaticInitializer String |
                         Ctr__StaticInitializer__0 StatementBlock
                         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type SwitchCaseList = [Rule_66]
data SwitchStatement = Anti_SwitchStatement String |
                       Ctr__SwitchStatement__0 Expression SwitchCaseList
                       deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data TryStatement = Anti_TryStatement String |
                    Ctr__TryStatement__0 Statement CatchList OptFinally
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Type = Anti_Type String |
            Ctr__Type__0 TypeSpecifier SquareBracketsList
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data TypeArgument = Anti_TypeArgument String |
                    Ctr__TypeArgument__0 Type |
                    Ctr__TypeArgument__1 WildcardType
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data TypeArguments = Anti_TypeArguments String |
                     Ctr__TypeArguments__0 |
                     Ctr__TypeArguments__1 Rule_80
                     deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data TypeDeclaration = Anti_TypeDeclaration String |
                       Ctr__TypeDeclaration__0 OptDocComment Rule_1
                       deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data TypeParameter = Anti_TypeParameter String |
                     Ctr__TypeParameter__0 String Rule_86
                     deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data TypeParameters = Anti_TypeParameters String |
                      Ctr__TypeParameters__0 |
                      Ctr__TypeParameters__1 Rule_83
                      deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data TypeSpecifier = Anti_TypeSpecifier String |
                     Ctr__TypeSpecifier__0 |
                     Ctr__TypeSpecifier__1 |
                     Ctr__TypeSpecifier__2 |
                     Ctr__TypeSpecifier__3 |
                     Ctr__TypeSpecifier__4 |
                     Ctr__TypeSpecifier__5 |
                     Ctr__TypeSpecifier__6 |
                     Ctr__TypeSpecifier__7 |
                     Ctr__TypeSpecifier__8 |
                     Ctr__TypeSpecifier__9 CompoundName TypeArguments
                     deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data VariableDeclaration = Anti_VariableDeclaration String |
                           Ctr__VariableDeclaration__0 Type VariableDeclaratorList
                           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data VariableDeclarator = Anti_VariableDeclarator String |
                          Ctr__VariableDeclarator__0 String SquareBracketsList OptVariableInitializer
                          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data VariableDeclaratorList = Anti_VariableDeclaratorList String |
                              Ctr__VariableDeclaratorList__0 VariableDeclarator Rule_49
                              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data VariableInitializer = Anti_VariableInitializer String |
                           Ctr__VariableInitializer__0 Expression |
                           Ctr__VariableInitializer__1 VariableInitializerList
                           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data VariableInitializerList = Anti_VariableInitializerList String |
                               Ctr__VariableInitializerList__0 |
                               Ctr__VariableInitializerList__1 Rule_52
                               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data WhileStatement = Anti_WhileStatement String |
                      Ctr__WhileStatement__0 Expression Statement
                      deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data WildcardType = Anti_WildcardType String |
                    Ctr__WildcardType__0 |
                    Ctr__WildcardType__1 Type |
                    Ctr__WildcardType__2 Type
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
}