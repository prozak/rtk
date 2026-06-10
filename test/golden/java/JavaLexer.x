{
module JavaLexer(alexScanTokens, Token(..), PosToken(..), AlexPosn(..))
where

 }
%wrapper "monad"

@exponentPart = ([eE]  ("+"| "-") ?  [0-9]  ([0-9_]*  [0-9]) ?)
@floatTypeSuffix = ([fFdD])

tokens :- "tok_AdditiveOp_dummy_171" { simple Tk__tok_AdditiveOp_dummy_171 }
          "tok_Annotation_dummy_170" { simple Tk__tok_Annotation_dummy_170 }
          "tok_AnnotationArguments_dummy_169" { simple Tk__tok_AnnotationArguments_dummy_169 }
          "tok_AnnotationDeclaration_dummy_168" { simple Tk__tok_AnnotationDeclaration_dummy_168 }
          "tok_AnnotationElement_dummy_167" { simple Tk__tok_AnnotationElement_dummy_167 }
          "tok_AnnotationList_dummy_166" { simple Tk__tok_AnnotationList_dummy_166 }
          "tok_AnnotationTypeElement_dummy_165" { simple Tk__tok_AnnotationTypeElement_dummy_165 }
          "tok_AnnotationTypeElementList_dummy_164" { simple Tk__tok_AnnotationTypeElementList_dummy_164 }
          "tok_Arglist_dummy_163" { simple Tk__tok_Arglist_dummy_163 }
          "tok_AssignmentOp_dummy_162" { simple Tk__tok_AssignmentOp_dummy_162 }
          "tok_CatchList_dummy_161" { simple Tk__tok_CatchList_dummy_161 }
          "tok_ClassDeclaration_dummy_160" { simple Tk__tok_ClassDeclaration_dummy_160 }
          "tok_CompilationUnit_dummy_159" { simple Tk__tok_CompilationUnit_dummy_159 }
          "tok_CompoundName_dummy_158" { simple Tk__tok_CompoundName_dummy_158 }
          "tok_CreationExpression_dummy_157" { simple Tk__tok_CreationExpression_dummy_157 }
          "tok_DoStatement_dummy_156" { simple Tk__tok_DoStatement_dummy_156 }
          "tok_DocComment_dummy_155" { simple Tk__tok_DocComment_dummy_155 }
          "tok_EnumConstant_dummy_154" { simple Tk__tok_EnumConstant_dummy_154 }
          "tok_EnumConstantList_dummy_153" { simple Tk__tok_EnumConstantList_dummy_153 }
          "tok_EnumDeclaration_dummy_152" { simple Tk__tok_EnumDeclaration_dummy_152 }
          "tok_EqualityOp_dummy_151" { simple Tk__tok_EqualityOp_dummy_151 }
          "tok_Expression_dummy_150" { simple Tk__tok_Expression_dummy_150 }
          "tok_ExtendsList_dummy_149" { simple Tk__tok_ExtendsList_dummy_149 }
          "tok_FieldDeclaration_dummy_148" { simple Tk__tok_FieldDeclaration_dummy_148 }
          "tok_FieldDeclarationList_dummy_147" { simple Tk__tok_FieldDeclarationList_dummy_147 }
          "tok_ForStatement_dummy_146" { simple Tk__tok_ForStatement_dummy_146 }
          "tok_IfStatement_dummy_145" { simple Tk__tok_IfStatement_dummy_145 }
          "tok_ImplementsList_dummy_144" { simple Tk__tok_ImplementsList_dummy_144 }
          "tok_ImportList_dummy_143" { simple Tk__tok_ImportList_dummy_143 }
          "tok_ImportStatement_dummy_142" { simple Tk__tok_ImportStatement_dummy_142 }
          "tok_InterfaceDeclaration_dummy_141" { simple Tk__tok_InterfaceDeclaration_dummy_141 }
          "tok_Java_dummy_172" { simple Tk__tok_Java_dummy_172 }
          "tok_Literal_dummy_140" { simple Tk__tok_Literal_dummy_140 }
          "tok_MemberAfterFirstId_dummy_139" { simple Tk__tok_MemberAfterFirstId_dummy_139 }
          "tok_MemberDeclaration_dummy_138" { simple Tk__tok_MemberDeclaration_dummy_138 }
          "tok_MemberRest_dummy_137" { simple Tk__tok_MemberRest_dummy_137 }
          "tok_Modifier_dummy_136" { simple Tk__tok_Modifier_dummy_136 }
          "tok_ModifierList_dummy_135" { simple Tk__tok_ModifierList_dummy_135 }
          "tok_MoreTypeSpecifier_dummy_134" { simple Tk__tok_MoreTypeSpecifier_dummy_134 }
          "tok_MoreVariableDeclarators_dummy_133" { simple Tk__tok_MoreVariableDeclarators_dummy_133 }
          "tok_MultiplicativeOp_dummy_132" { simple Tk__tok_MultiplicativeOp_dummy_132 }
          "tok_NestedTypeDeclaration_dummy_131" { simple Tk__tok_NestedTypeDeclaration_dummy_131 }
          "tok_OptDocComment_dummy_130" { simple Tk__tok_OptDocComment_dummy_130 }
          "tok_OptElsePart_dummy_129" { simple Tk__tok_OptElsePart_dummy_129 }
          "tok_OptExpression_dummy_128" { simple Tk__tok_OptExpression_dummy_128 }
          "tok_OptFinally_dummy_127" { simple Tk__tok_OptFinally_dummy_127 }
          "tok_OptId_dummy_126" { simple Tk__tok_OptId_dummy_126 }
          "tok_OptVariableInitializer_dummy_125" { simple Tk__tok_OptVariableInitializer_dummy_125 }
          "tok_Package_dummy_124" { simple Tk__tok_Package_dummy_124 }
          "tok_Parameter_dummy_123" { simple Tk__tok_Parameter_dummy_123 }
          "tok_ParameterList_dummy_122" { simple Tk__tok_ParameterList_dummy_122 }
          "tok_PostfixOp_dummy_121" { simple Tk__tok_PostfixOp_dummy_121 }
          "tok_PrefixOp_dummy_120" { simple Tk__tok_PrefixOp_dummy_120 }
          "tok_PrimitiveTypeKeyword_dummy_119" { simple Tk__tok_PrimitiveTypeKeyword_dummy_119 }
          "tok_RelationalOp_dummy_118" { simple Tk__tok_RelationalOp_dummy_118 }
          "tok_ShiftOp_dummy_117" { simple Tk__tok_ShiftOp_dummy_117 }
          "tok_SquareBracketsList_dummy_116" { simple Tk__tok_SquareBracketsList_dummy_116 }
          "tok_Statement_dummy_115" { simple Tk__tok_Statement_dummy_115 }
          "tok_StatementBlock_dummy_114" { simple Tk__tok_StatementBlock_dummy_114 }
          "tok_StatementList_dummy_113" { simple Tk__tok_StatementList_dummy_113 }
          "tok_StatementWithoutIf_dummy_112" { simple Tk__tok_StatementWithoutIf_dummy_112 }
          "tok_StaticInitializer_dummy_111" { simple Tk__tok_StaticInitializer_dummy_111 }
          "tok_SwitchCaseList_dummy_110" { simple Tk__tok_SwitchCaseList_dummy_110 }
          "tok_SwitchStatement_dummy_109" { simple Tk__tok_SwitchStatement_dummy_109 }
          "tok_TryStatement_dummy_108" { simple Tk__tok_TryStatement_dummy_108 }
          "tok_Type_dummy_107" { simple Tk__tok_Type_dummy_107 }
          "tok_TypeArgument_dummy_106" { simple Tk__tok_TypeArgument_dummy_106 }
          "tok_TypeArguments_dummy_105" { simple Tk__tok_TypeArguments_dummy_105 }
          "tok_TypeDeclaration_dummy_104" { simple Tk__tok_TypeDeclaration_dummy_104 }
          "tok_TypeParameter_dummy_103" { simple Tk__tok_TypeParameter_dummy_103 }
          "tok_TypeParameters_dummy_102" { simple Tk__tok_TypeParameters_dummy_102 }
          "tok_TypeSpecifier_dummy_101" { simple Tk__tok_TypeSpecifier_dummy_101 }
          "tok_VariableDeclaration_dummy_100" { simple Tk__tok_VariableDeclaration_dummy_100 }
          "tok_VariableDeclarator_dummy_99" { simple Tk__tok_VariableDeclarator_dummy_99 }
          "tok_VariableDeclaratorList_dummy_98" { simple Tk__tok_VariableDeclaratorList_dummy_98 }
          "tok_VariableInitializer_dummy_97" { simple Tk__tok_VariableInitializer_dummy_97 }
          "tok_VariableInitializerList_dummy_96" { simple Tk__tok_VariableInitializerList_dummy_96 }
          "tok_WhileStatement_dummy_95" { simple Tk__tok_WhileStatement_dummy_95 }
          "tok_WildcardType_dummy_94" { simple Tk__tok_WildcardType_dummy_94 }
          "~" { simple Tk__tok__tilde__78 }
          "}" { simple Tk__tok__symbol__14 }
          "||" { simple Tk__tok__pipe__pipe__57 }
          "|=" { simple Tk__tok__pipe__eql__49 }
          "|" { simple Tk__tok__pipe__59 }
          "{" { simple Tk__tok__symbol__13 }
          "while" { simple Tk__tok_while_38 }
          "void" { simple Tk__tok_void_28 }
          "try" { simple Tk__tok_try_42 }
          "true" { simple Tk__tok_true_83 }
          "transient" { simple Tk__tok_transient_94 }
          "throw" { simple Tk__tok_throw_31 }
          "threadsafe" { simple Tk__tok_threadsafe_93 }
          "this" { simple Tk__tok_this_80 }
          "synchronized" { simple Tk__tok_synchronized_30 }
          "switch" { simple Tk__tok_switch_44 }
          "super" { simple Tk__tok_super_81 }
          "static" { simple Tk__tok_static_89 }
          "short" { simple Tk__tok_short_23 }
          "return" { simple Tk__tok_return_29 }
          "public" { simple Tk__tok_public_86 }
          "protected" { simple Tk__tok_protected_88 }
          "private" { simple Tk__tok_private_87 }
          "package" { simple Tk__tok_package_0 }
          "null" { simple Tk__tok_null_85 }
          "new" { simple Tk__tok_new_82 }
          "native" { simple Tk__tok_native_91 }
          "long" { simple Tk__tok_long_26 }
          "interface" { simple Tk__tok_interface_15 }
          "int" { simple Tk__tok_int_24 }
          "instanceof" { simple Tk__tok_instanceof_68 }
          "import" { simple Tk__tok_import_2 }
          "implements" { simple Tk__tok_implements_11 }
          "if" { simple Tk__tok_if_36 }
          "for" { simple Tk__tok_for_39 }
          "float" { simple Tk__tok_float_25 }
          "finally" { simple Tk__tok_finally_41 }
          "final" { simple Tk__tok_final_90 }
          "false" { simple Tk__tok_false_84 }
          "extends" { simple Tk__tok_extends_10 }
          "enum" { simple Tk__tok_enum_17 }
          "else" { simple Tk__tok_else_35 }
          "double" { simple Tk__tok_double_27 }
          "do" { simple Tk__tok_do_37 }
          "default" { simple Tk__tok_default_16 }
          "continue" { simple Tk__tok_continue_34 }
          "class" { simple Tk__tok_class_12 }
          "char" { simple Tk__tok_char_22 }
          "catch" { simple Tk__tok_catch_40 }
          "case" { simple Tk__tok_case_43 }
          "byte" { simple Tk__tok_byte_21 }
          "break" { simple Tk__tok_break_33 }
          "boolean" { simple Tk__tok_boolean_20 }
          "abstract" { simple Tk__tok_abstract_92 }
          "^=" { simple Tk__tok__symbol__eql__51 }
          "^" { simple Tk__tok__symbol__60 }
          "]" { simple Tk__tok__sq_bkt_r__19 }
          "[" { simple Tk__tok__sq_bkt_l__18 }
          "@" { simple Tk__tok__symbol__5 }
          "?" { simple Tk__tok__symbol__56 }
          ">>>=" { simple Tk__tok__symbol__symbol__symbol__eql__55 }
          ">>>" { simple Tk__tok__symbol__symbol__symbol__71 }
          ">>=" { simple Tk__tok__symbol__symbol__eql__54 }
          ">>" { simple Tk__tok__symbol__symbol__69 }
          ">=" { simple Tk__tok__symbol__eql__67 }
          ">" { simple Tk__tok__symbol__65 }
          "==" { simple Tk__tok__eql__eql__62 }
          "=" { simple Tk__tok__eql__9 }
          "<=" { simple Tk__tok__symbol__eql__66 }
          "<<=" { simple Tk__tok__symbol__symbol__eql__53 }
          "<<" { simple Tk__tok__symbol__symbol__70 }
          "<" { simple Tk__tok__symbol__64 }
          ";" { simple Tk__tok__semi__1 }
          ":" { simple Tk__tok__colon__32 }
          "/=" { simple Tk__tok__symbol__eql__48 }
          "/" { simple Tk__tok__symbol__74 }
          "." { simple Tk__tok__dot__3 }
          "-=" { simple Tk__tok__minus__eql__46 }
          "--" { simple Tk__tok__minus__minus__77 }
          "-" { simple Tk__tok__minus__73 }
          "," { simple Tk__tok__coma__8 }
          "+=" { simple Tk__tok__plus__eql__45 }
          "++" { simple Tk__tok__plus__plus__76 }
          "+" { simple Tk__tok__plus__72 }
          "*=" { simple Tk__tok__star__eql__47 }
          "*" { simple Tk__tok__star__4 }
          ")" { simple Tk__tok__rparen__7 }
          "(" { simple Tk__tok__lparen__6 }
          "&=" { simple Tk__tok__symbol__eql__50 }
          "&&" { simple Tk__tok__symbol__symbol__58 }
          "&" { simple Tk__tok__symbol__61 }
          "%=" { simple Tk__tok__symbol__eql__52 }
          "%" { simple Tk__tok__symbol__75 }
          "!=" { simple Tk__tok__exclamation__eql__63 }
          "!" { simple Tk__tok__exclamation__79 }
          ("/*"  ([^\*\n]| [\n])  ([\n]| [^\*\n]| [\*]  [^\/\n]| [\*]  [\n])*  "*/") ;
          ("//"  [^\n]*  \n) ;
          ([\ \t\n\r]+) ;
          ("/**"  ([\n]| [^\*\n]| [\*]  [^\/\n]| [\*]  [\n])*  "*/") { simple1 $  Tk__doccomment . (id) }
          ([a-zA-Z\$_]  [a-zA-Z\$_0-9]*) { simple1 $  Tk__id . (id) }
          ([\"]  ([^\x22\x5C\x0A\x0D]| [\x5C]  .)*  [\"]) { simple1 $  Tk__string . (id) }
          ("'"  .  "'"| "'"  [\x5C]  .  "'"| "'"  [\x5C]  [0-7]  [0-7]  "'"| "'"  [\x5C]  [0-3]  [0-7]  [0-7]  "'"| "'"  [\x5C]  "u"  [0-9a-fA-F]  [0-9a-fA-F]  [0-9a-fA-F]  [0-9a-fA-F]  "'") { simple1 $  Tk__char . (id) }
          ([fFdD]) { simple1 $  Tk__floatTypeSuffix . (id) }
          ([eE]  ("+"| "-") ?  [0-9]  ([0-9_]*  [0-9]) ?) { simple1 $  Tk__exponentPart . (id) }
          ([0-9]  ([0-9_]*  [0-9]) ?  "."  ([0-9]  ([0-9_]*  [0-9]) ?) ?  @exponentPart ?  @floatTypeSuffix ?| "."  [0-9]  ([0-9_]*  [0-9]) ?  @exponentPart ?  @floatTypeSuffix ?| [0-9]  ([0-9_]*  [0-9]) ?  @exponentPart  @floatTypeSuffix ?| [0-9]  ([0-9_]*  [0-9]) ?  @floatTypeSuffix) { simple1 $  Tk__floatLiteral . (id) }
          ((([0-9]  ([0-9_]*  [0-9]) ?)| ("0"  [xX]  [0-9a-fA-F]  ([0-9a-fA-F_]*  [0-9a-fA-F]) ?)| ("0"  [bB]  [01]  ([01_]*  [01]) ?))  [lL] ?) { simple1 $  Tk__integerLiteral . (id) }
          ("$"  "CompoundName"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_CompoundName . ((tail . dropWhile (/= ':'))) }
          ("$"  "Modifier"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Modifier . ((tail . dropWhile (/= ':'))) }
          ("$"  "TypeSpecifier"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TypeSpecifier . ((tail . dropWhile (/= ':'))) }
          ("$"  "Type"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Type . ((tail . dropWhile (/= ':'))) }
          ("$"  "TypeParameter"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TypeParameter . ((tail . dropWhile (/= ':'))) }
          ("$"  "TypeParameters"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TypeParameters . ((tail . dropWhile (/= ':'))) }
          ("$"  "WildcardType"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_WildcardType . ((tail . dropWhile (/= ':'))) }
          ("$"  "TypeArgument"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TypeArgument . ((tail . dropWhile (/= ':'))) }
          ("$"  "TypeArguments"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TypeArguments . ((tail . dropWhile (/= ':'))) }
          ("$"  "Arglist"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Arglist . ((tail . dropWhile (/= ':'))) }
          ("$"  "Literal"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Literal . ((tail . dropWhile (/= ':'))) }
          ("$"  "CreationExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_CreationExpression . ((tail . dropWhile (/= ':'))) }
          ("$"  "PostfixOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_PostfixOp . ((tail . dropWhile (/= ':'))) }
          ("$"  "PrefixOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_PrefixOp . ((tail . dropWhile (/= ':'))) }
          ("$"  "MultiplicativeOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_MultiplicativeOp . ((tail . dropWhile (/= ':'))) }
          ("$"  "AdditiveOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AdditiveOp . ((tail . dropWhile (/= ':'))) }
          ("$"  "ShiftOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ShiftOp . ((tail . dropWhile (/= ':'))) }
          ("$"  "RelationalOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_RelationalOp . ((tail . dropWhile (/= ':'))) }
          ("$"  "EqualityOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_EqualityOp . ((tail . dropWhile (/= ':'))) }
          ("$"  "AssignmentOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AssignmentOp . ((tail . dropWhile (/= ':'))) }
          ("$"  "Expression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Expression . ((tail . dropWhile (/= ':'))) }
          ("$"  "SwitchStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_SwitchStatement . ((tail . dropWhile (/= ':'))) }
          ("$"  "SwitchCaseList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_SwitchCaseList . ((tail . dropWhile (/= ':'))) }
          ("$"  "TryStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TryStatement . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptFinally"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptFinally . ((tail . dropWhile (/= ':'))) }
          ("$"  "CatchList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_CatchList . ((tail . dropWhile (/= ':'))) }
          ("$"  "ForStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ForStatement . ((tail . dropWhile (/= ':'))) }
          ("$"  "WhileStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_WhileStatement . ((tail . dropWhile (/= ':'))) }
          ("$"  "DoStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_DoStatement . ((tail . dropWhile (/= ':'))) }
          ("$"  "IfStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_IfStatement . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptElsePart"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptElsePart . ((tail . dropWhile (/= ':'))) }
          ("$"  "StatementWithoutIf"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_StatementWithoutIf . ((tail . dropWhile (/= ':'))) }
          ("$"  "Statement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Statement . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptId"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptId . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptExpression . ((tail . dropWhile (/= ':'))) }
          ("$"  "StatementList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_StatementList . ((tail . dropWhile (/= ':'))) }
          ("$"  "Parameter"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Parameter . ((tail . dropWhile (/= ':'))) }
          ("$"  "ParameterList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ParameterList . ((tail . dropWhile (/= ':'))) }
          ("$"  "StaticInitializer"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_StaticInitializer . ((tail . dropWhile (/= ':'))) }
          ("$"  "VariableInitializer"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_VariableInitializer . ((tail . dropWhile (/= ':'))) }
          ("$"  "VariableInitializerList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_VariableInitializerList . ((tail . dropWhile (/= ':'))) }
          ("$"  "VariableDeclarator"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_VariableDeclarator . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptVariableInitializer"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptVariableInitializer . ((tail . dropWhile (/= ':'))) }
          ("$"  "VariableDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_VariableDeclaration . ((tail . dropWhile (/= ':'))) }
          ("$"  "VariableDeclaratorList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_VariableDeclaratorList . ((tail . dropWhile (/= ':'))) }
          ("$"  "StatementBlock"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_StatementBlock . ((tail . dropWhile (/= ':'))) }
          ("$"  "MoreVariableDeclarators"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_MoreVariableDeclarators . ((tail . dropWhile (/= ':'))) }
          ("$"  "MemberRest"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_MemberRest . ((tail . dropWhile (/= ':'))) }
          ("$"  "MoreTypeSpecifier"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_MoreTypeSpecifier . ((tail . dropWhile (/= ':'))) }
          ("$"  "MemberAfterFirstId"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_MemberAfterFirstId . ((tail . dropWhile (/= ':'))) }
          ("$"  "PrimitiveTypeKeyword"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_PrimitiveTypeKeyword . ((tail . dropWhile (/= ':'))) }
          ("$"  "MemberDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_MemberDeclaration . ((tail . dropWhile (/= ':'))) }
          ("$"  "SquareBracketsList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_SquareBracketsList . ((tail . dropWhile (/= ':'))) }
          ("$"  "FieldDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_FieldDeclaration . ((tail . dropWhile (/= ':'))) }
          ("$"  "NestedTypeDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_NestedTypeDeclaration . ((tail . dropWhile (/= ':'))) }
          ("$"  "EnumDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_EnumDeclaration . ((tail . dropWhile (/= ':'))) }
          ("$"  "EnumConstantList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_EnumConstantList . ((tail . dropWhile (/= ':'))) }
          ("$"  "EnumConstant"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_EnumConstant . ((tail . dropWhile (/= ':'))) }
          ("$"  "AnnotationTypeElement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AnnotationTypeElement . ((tail . dropWhile (/= ':'))) }
          ("$"  "AnnotationTypeElementList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AnnotationTypeElementList . ((tail . dropWhile (/= ':'))) }
          ("$"  "AnnotationDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AnnotationDeclaration . ((tail . dropWhile (/= ':'))) }
          ("$"  "InterfaceDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_InterfaceDeclaration . ((tail . dropWhile (/= ':'))) }
          ("$"  "ClassDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ClassDeclaration . ((tail . dropWhile (/= ':'))) }
          ("$"  "FieldDeclarationList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_FieldDeclarationList . ((tail . dropWhile (/= ':'))) }
          ("$"  "ImplementsList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ImplementsList . ((tail . dropWhile (/= ':'))) }
          ("$"  "ExtendsList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ExtendsList . ((tail . dropWhile (/= ':'))) }
          ("$"  "ModifierList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ModifierList . ((tail . dropWhile (/= ':'))) }
          ("$"  "AnnotationList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AnnotationList . ((tail . dropWhile (/= ':'))) }
          ("$"  "AnnotationElement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AnnotationElement . ((tail . dropWhile (/= ':'))) }
          ("$"  "AnnotationArguments"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AnnotationArguments . ((tail . dropWhile (/= ':'))) }
          ("$"  "Annotation"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Annotation . ((tail . dropWhile (/= ':'))) }
          ("$"  "DocComment"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_DocComment . ((tail . dropWhile (/= ':'))) }
          ("$"  "ImportStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ImportStatement . ((tail . dropWhile (/= ':'))) }
          ("$"  "Package"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Package . ((tail . dropWhile (/= ':'))) }
          ("$"  "CompilationUnit"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_CompilationUnit . ((tail . dropWhile (/= ':'))) }
          ("$"  "ImportList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ImportList . ((tail . dropWhile (/= ':'))) }
          ("$"  "TypeDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TypeDeclaration . ((tail . dropWhile (/= ':'))) }
          ("$"  "OptDocComment"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptDocComment . ((tail . dropWhile (/= ':'))) }
          ("$"  "Java"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Java . ((tail . dropWhile (/= ':'))) }
          . { rtkError }

{
data Token = EndOfFile |
             Tk__tok_AdditiveOp_dummy_171 |
             Tk__tok_Annotation_dummy_170 |
             Tk__tok_AnnotationArguments_dummy_169 |
             Tk__tok_AnnotationDeclaration_dummy_168 |
             Tk__tok_AnnotationElement_dummy_167 |
             Tk__tok_AnnotationList_dummy_166 |
             Tk__tok_AnnotationTypeElement_dummy_165 |
             Tk__tok_AnnotationTypeElementList_dummy_164 |
             Tk__tok_Arglist_dummy_163 |
             Tk__tok_AssignmentOp_dummy_162 |
             Tk__tok_CatchList_dummy_161 |
             Tk__tok_ClassDeclaration_dummy_160 |
             Tk__tok_CompilationUnit_dummy_159 |
             Tk__tok_CompoundName_dummy_158 |
             Tk__tok_CreationExpression_dummy_157 |
             Tk__tok_DoStatement_dummy_156 |
             Tk__tok_DocComment_dummy_155 |
             Tk__tok_EnumConstant_dummy_154 |
             Tk__tok_EnumConstantList_dummy_153 |
             Tk__tok_EnumDeclaration_dummy_152 |
             Tk__tok_EqualityOp_dummy_151 |
             Tk__tok_Expression_dummy_150 |
             Tk__tok_ExtendsList_dummy_149 |
             Tk__tok_FieldDeclaration_dummy_148 |
             Tk__tok_FieldDeclarationList_dummy_147 |
             Tk__tok_ForStatement_dummy_146 |
             Tk__tok_IfStatement_dummy_145 |
             Tk__tok_ImplementsList_dummy_144 |
             Tk__tok_ImportList_dummy_143 |
             Tk__tok_ImportStatement_dummy_142 |
             Tk__tok_InterfaceDeclaration_dummy_141 |
             Tk__tok_Java_dummy_172 |
             Tk__tok_Literal_dummy_140 |
             Tk__tok_MemberAfterFirstId_dummy_139 |
             Tk__tok_MemberDeclaration_dummy_138 |
             Tk__tok_MemberRest_dummy_137 |
             Tk__tok_Modifier_dummy_136 |
             Tk__tok_ModifierList_dummy_135 |
             Tk__tok_MoreTypeSpecifier_dummy_134 |
             Tk__tok_MoreVariableDeclarators_dummy_133 |
             Tk__tok_MultiplicativeOp_dummy_132 |
             Tk__tok_NestedTypeDeclaration_dummy_131 |
             Tk__tok_OptDocComment_dummy_130 |
             Tk__tok_OptElsePart_dummy_129 |
             Tk__tok_OptExpression_dummy_128 |
             Tk__tok_OptFinally_dummy_127 |
             Tk__tok_OptId_dummy_126 |
             Tk__tok_OptVariableInitializer_dummy_125 |
             Tk__tok_Package_dummy_124 |
             Tk__tok_Parameter_dummy_123 |
             Tk__tok_ParameterList_dummy_122 |
             Tk__tok_PostfixOp_dummy_121 |
             Tk__tok_PrefixOp_dummy_120 |
             Tk__tok_PrimitiveTypeKeyword_dummy_119 |
             Tk__tok_RelationalOp_dummy_118 |
             Tk__tok_ShiftOp_dummy_117 |
             Tk__tok_SquareBracketsList_dummy_116 |
             Tk__tok_Statement_dummy_115 |
             Tk__tok_StatementBlock_dummy_114 |
             Tk__tok_StatementList_dummy_113 |
             Tk__tok_StatementWithoutIf_dummy_112 |
             Tk__tok_StaticInitializer_dummy_111 |
             Tk__tok_SwitchCaseList_dummy_110 |
             Tk__tok_SwitchStatement_dummy_109 |
             Tk__tok_TryStatement_dummy_108 |
             Tk__tok_Type_dummy_107 |
             Tk__tok_TypeArgument_dummy_106 |
             Tk__tok_TypeArguments_dummy_105 |
             Tk__tok_TypeDeclaration_dummy_104 |
             Tk__tok_TypeParameter_dummy_103 |
             Tk__tok_TypeParameters_dummy_102 |
             Tk__tok_TypeSpecifier_dummy_101 |
             Tk__tok_VariableDeclaration_dummy_100 |
             Tk__tok_VariableDeclarator_dummy_99 |
             Tk__tok_VariableDeclaratorList_dummy_98 |
             Tk__tok_VariableInitializer_dummy_97 |
             Tk__tok_VariableInitializerList_dummy_96 |
             Tk__tok_WhileStatement_dummy_95 |
             Tk__tok_WildcardType_dummy_94 |
             Tk__tok__tilde__78 |
             Tk__tok__symbol__14 |
             Tk__tok__pipe__pipe__57 |
             Tk__tok__pipe__eql__49 |
             Tk__tok__pipe__59 |
             Tk__tok__symbol__13 |
             Tk__tok_while_38 |
             Tk__tok_void_28 |
             Tk__tok_try_42 |
             Tk__tok_true_83 |
             Tk__tok_transient_94 |
             Tk__tok_throw_31 |
             Tk__tok_threadsafe_93 |
             Tk__tok_this_80 |
             Tk__tok_synchronized_30 |
             Tk__tok_switch_44 |
             Tk__tok_super_81 |
             Tk__tok_static_89 |
             Tk__tok_short_23 |
             Tk__tok_return_29 |
             Tk__tok_public_86 |
             Tk__tok_protected_88 |
             Tk__tok_private_87 |
             Tk__tok_package_0 |
             Tk__tok_null_85 |
             Tk__tok_new_82 |
             Tk__tok_native_91 |
             Tk__tok_long_26 |
             Tk__tok_interface_15 |
             Tk__tok_int_24 |
             Tk__tok_instanceof_68 |
             Tk__tok_import_2 |
             Tk__tok_implements_11 |
             Tk__tok_if_36 |
             Tk__tok_for_39 |
             Tk__tok_float_25 |
             Tk__tok_finally_41 |
             Tk__tok_final_90 |
             Tk__tok_false_84 |
             Tk__tok_extends_10 |
             Tk__tok_enum_17 |
             Tk__tok_else_35 |
             Tk__tok_double_27 |
             Tk__tok_do_37 |
             Tk__tok_default_16 |
             Tk__tok_continue_34 |
             Tk__tok_class_12 |
             Tk__tok_char_22 |
             Tk__tok_catch_40 |
             Tk__tok_case_43 |
             Tk__tok_byte_21 |
             Tk__tok_break_33 |
             Tk__tok_boolean_20 |
             Tk__tok_abstract_92 |
             Tk__tok__symbol__eql__51 |
             Tk__tok__symbol__60 |
             Tk__tok__sq_bkt_r__19 |
             Tk__tok__sq_bkt_l__18 |
             Tk__tok__symbol__5 |
             Tk__tok__symbol__56 |
             Tk__tok__symbol__symbol__symbol__eql__55 |
             Tk__tok__symbol__symbol__symbol__71 |
             Tk__tok__symbol__symbol__eql__54 |
             Tk__tok__symbol__symbol__69 |
             Tk__tok__symbol__eql__67 |
             Tk__tok__symbol__65 |
             Tk__tok__eql__eql__62 |
             Tk__tok__eql__9 |
             Tk__tok__symbol__eql__66 |
             Tk__tok__symbol__symbol__eql__53 |
             Tk__tok__symbol__symbol__70 |
             Tk__tok__symbol__64 |
             Tk__tok__semi__1 |
             Tk__tok__colon__32 |
             Tk__tok__symbol__eql__48 |
             Tk__tok__symbol__74 |
             Tk__tok__dot__3 |
             Tk__tok__minus__eql__46 |
             Tk__tok__minus__minus__77 |
             Tk__tok__minus__73 |
             Tk__tok__coma__8 |
             Tk__tok__plus__eql__45 |
             Tk__tok__plus__plus__76 |
             Tk__tok__plus__72 |
             Tk__tok__star__eql__47 |
             Tk__tok__star__4 |
             Tk__tok__rparen__7 |
             Tk__tok__lparen__6 |
             Tk__tok__symbol__eql__50 |
             Tk__tok__symbol__symbol__58 |
             Tk__tok__symbol__61 |
             Tk__tok__symbol__eql__52 |
             Tk__tok__symbol__75 |
             Tk__tok__exclamation__eql__63 |
             Tk__tok__exclamation__79 |
             Tk__doccomment String |
             Tk__id String |
             Tk__string String |
             Tk__char String |
             Tk__floatTypeSuffix String |
             Tk__exponentPart String |
             Tk__floatLiteral String |
             Tk__integerLiteral String |
             Tk__qq_CompoundName String |
             Tk__qq_Modifier String |
             Tk__qq_TypeSpecifier String |
             Tk__qq_Type String |
             Tk__qq_TypeParameter String |
             Tk__qq_TypeParameters String |
             Tk__qq_WildcardType String |
             Tk__qq_TypeArgument String |
             Tk__qq_TypeArguments String |
             Tk__qq_Arglist String |
             Tk__qq_Literal String |
             Tk__qq_CreationExpression String |
             Tk__qq_PostfixOp String |
             Tk__qq_PrefixOp String |
             Tk__qq_MultiplicativeOp String |
             Tk__qq_AdditiveOp String |
             Tk__qq_ShiftOp String |
             Tk__qq_RelationalOp String |
             Tk__qq_EqualityOp String |
             Tk__qq_AssignmentOp String |
             Tk__qq_Expression String |
             Tk__qq_SwitchStatement String |
             Tk__qq_SwitchCaseList String |
             Tk__qq_TryStatement String |
             Tk__qq_OptFinally String |
             Tk__qq_CatchList String |
             Tk__qq_ForStatement String |
             Tk__qq_WhileStatement String |
             Tk__qq_DoStatement String |
             Tk__qq_IfStatement String |
             Tk__qq_OptElsePart String |
             Tk__qq_StatementWithoutIf String |
             Tk__qq_Statement String |
             Tk__qq_OptId String |
             Tk__qq_OptExpression String |
             Tk__qq_StatementList String |
             Tk__qq_Parameter String |
             Tk__qq_ParameterList String |
             Tk__qq_StaticInitializer String |
             Tk__qq_VariableInitializer String |
             Tk__qq_VariableInitializerList String |
             Tk__qq_VariableDeclarator String |
             Tk__qq_OptVariableInitializer String |
             Tk__qq_VariableDeclaration String |
             Tk__qq_VariableDeclaratorList String |
             Tk__qq_StatementBlock String |
             Tk__qq_MoreVariableDeclarators String |
             Tk__qq_MemberRest String |
             Tk__qq_MoreTypeSpecifier String |
             Tk__qq_MemberAfterFirstId String |
             Tk__qq_PrimitiveTypeKeyword String |
             Tk__qq_MemberDeclaration String |
             Tk__qq_SquareBracketsList String |
             Tk__qq_FieldDeclaration String |
             Tk__qq_NestedTypeDeclaration String |
             Tk__qq_EnumDeclaration String |
             Tk__qq_EnumConstantList String |
             Tk__qq_EnumConstant String |
             Tk__qq_AnnotationTypeElement String |
             Tk__qq_AnnotationTypeElementList String |
             Tk__qq_AnnotationDeclaration String |
             Tk__qq_InterfaceDeclaration String |
             Tk__qq_ClassDeclaration String |
             Tk__qq_FieldDeclarationList String |
             Tk__qq_ImplementsList String |
             Tk__qq_ExtendsList String |
             Tk__qq_ModifierList String |
             Tk__qq_AnnotationList String |
             Tk__qq_AnnotationElement String |
             Tk__qq_AnnotationArguments String |
             Tk__qq_Annotation String |
             Tk__qq_DocComment String |
             Tk__qq_ImportStatement String |
             Tk__qq_Package String |
             Tk__qq_CompilationUnit String |
             Tk__qq_ImportList String |
             Tk__qq_TypeDeclaration String |
             Tk__qq_OptDocComment String |
             Tk__qq_Java String
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