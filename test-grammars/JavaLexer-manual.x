{
module JavaLexer(alexScanTokens, Token(..))
where

 }
%wrapper "monad"

@exponentPart = ("e"  ("+"| "-") ?  [0-9]+)
@floatTypeSuffix = ("f"| "d")

tokens :-

<0>       "/*"                              { begin blockcomment }
<blockcomment> "*/"                         { begin 0 }
<blockcomment> .                             ;
<blockcomment> \n                            ;

<0>       "/**"                              { begin doccomment }
<doccomment> .                               ;
<doccomment> \n                              ;
<doccomment> "*/"                           { \ _ _ -> alexSetStartCode 0 >> return (Tk__doccomment "/**...*/") }

<0>       "//"                               { begin linecomment }
<linecomment> \n                            { begin 0 }
<linecomment> .                              ;

<0>       "tok_AdditiveExpression_dummy_379" { simple Tk__tok_AdditiveExpression_dummy_379 }
<0>       "tok_AdditiveOp_dummy_378" { simple Tk__tok_AdditiveOp_dummy_378 }
<0>       "tok_AndExpression_dummy_377" { simple Tk__tok_AndExpression_dummy_377 }
<0>       "tok_Annotation_dummy_376" { simple Tk__tok_Annotation_dummy_376 }
<0>       "tok_AnnotationArguments_dummy_375" { simple Tk__tok_AnnotationArguments_dummy_375 }
<0>       "tok_AnnotationDeclaration_dummy_374" { simple Tk__tok_AnnotationDeclaration_dummy_374 }
<0>       "tok_AnnotationElement_dummy_373" { simple Tk__tok_AnnotationElement_dummy_373 }
<0>       "tok_AnnotationList_dummy_372" { simple Tk__tok_AnnotationList_dummy_372 }
<0>       "tok_AnnotationTypeElement_dummy_371" { simple Tk__tok_AnnotationTypeElement_dummy_371 }
<0>       "tok_AnnotationTypeElementList_dummy_370" { simple Tk__tok_AnnotationTypeElementList_dummy_370 }
<0>       "tok_Arglist_dummy_369" { simple Tk__tok_Arglist_dummy_369 }
<0>       "tok_AssignmentExpression_dummy_368" { simple Tk__tok_AssignmentExpression_dummy_368 }
<0>       "tok_AssignmentOp_dummy_367" { simple Tk__tok_AssignmentOp_dummy_367 }
<0>       "tok_CastExpression_dummy_366" { simple Tk__tok_CastExpression_dummy_366 }
<0>       "tok_CatchList_dummy_365" { simple Tk__tok_CatchList_dummy_365 }
<0>       "tok_ClassDeclaration_dummy_364" { simple Tk__tok_ClassDeclaration_dummy_364 }
<0>       "tok_CompilationUnit_dummy_363" { simple Tk__tok_CompilationUnit_dummy_363 }
<0>       "tok_CompoundName_dummy_362" { simple Tk__tok_CompoundName_dummy_362 }
<0>       "tok_ConditionalAndExpression_dummy_361" { simple Tk__tok_ConditionalAndExpression_dummy_361 }
<0>       "tok_ConditionalExpression_dummy_360" { simple Tk__tok_ConditionalExpression_dummy_360 }
<0>       "tok_ConditionalOrExpression_dummy_359" { simple Tk__tok_ConditionalOrExpression_dummy_359 }
<0>       "tok_CreationExpression_dummy_358" { simple Tk__tok_CreationExpression_dummy_358 }
<0>       "tok_DoStatement_dummy_357" { simple Tk__tok_DoStatement_dummy_357 }
<0>       "tok_DocComment_dummy_356" { simple Tk__tok_DocComment_dummy_356 }
<0>       "tok_EnumConstant_dummy_355" { simple Tk__tok_EnumConstant_dummy_355 }
<0>       "tok_EnumConstantList_dummy_354" { simple Tk__tok_EnumConstantList_dummy_354 }
<0>       "tok_EnumDeclaration_dummy_353" { simple Tk__tok_EnumDeclaration_dummy_353 }
<0>       "tok_EqualityExpression_dummy_352" { simple Tk__tok_EqualityExpression_dummy_352 }
<0>       "tok_EqualityOp_dummy_351" { simple Tk__tok_EqualityOp_dummy_351 }
<0>       "tok_ExclusiveOrExpression_dummy_350" { simple Tk__tok_ExclusiveOrExpression_dummy_350 }
<0>       "tok_Expression_dummy_349" { simple Tk__tok_Expression_dummy_349 }
<0>       "tok_ExtendsList_dummy_348" { simple Tk__tok_ExtendsList_dummy_348 }
<0>       "tok_FieldDeclaration_dummy_347" { simple Tk__tok_FieldDeclaration_dummy_347 }
<0>       "tok_FieldDeclarationList_dummy_346" { simple Tk__tok_FieldDeclarationList_dummy_346 }
<0>       "tok_ForStatement_dummy_345" { simple Tk__tok_ForStatement_dummy_345 }
<0>       "tok_IfStatement_dummy_344" { simple Tk__tok_IfStatement_dummy_344 }
<0>       "tok_ImplementsList_dummy_343" { simple Tk__tok_ImplementsList_dummy_343 }
<0>       "tok_ImportList_dummy_342" { simple Tk__tok_ImportList_dummy_342 }
<0>       "tok_ImportStatement_dummy_341" { simple Tk__tok_ImportStatement_dummy_341 }
<0>       "tok_InclusiveOrEpression_dummy_340" { simple Tk__tok_InclusiveOrEpression_dummy_340 }
<0>       "tok_InterfaceDeclaration_dummy_339" { simple Tk__tok_InterfaceDeclaration_dummy_339 }
<0>       "tok_Java_dummy_380" { simple Tk__tok_Java_dummy_380 }
<0>       "tok_Literal_dummy_338" { simple Tk__tok_Literal_dummy_338 }
<0>       "tok_MemberAfterFirstId_dummy_337" { simple Tk__tok_MemberAfterFirstId_dummy_337 }
<0>       "tok_MemberDeclaration_dummy_336" { simple Tk__tok_MemberDeclaration_dummy_336 }
<0>       "tok_MemberRest_dummy_335" { simple Tk__tok_MemberRest_dummy_335 }
<0>       "tok_Modifier_dummy_334" { simple Tk__tok_Modifier_dummy_334 }
<0>       "tok_ModifierList_dummy_333" { simple Tk__tok_ModifierList_dummy_333 }
<0>       "tok_MoreTypeSpecifier_dummy_332" { simple Tk__tok_MoreTypeSpecifier_dummy_332 }
<0>       "tok_MoreVariableDeclarators_dummy_331" { simple Tk__tok_MoreVariableDeclarators_dummy_331 }
<0>       "tok_MultiplicativeExpression_dummy_330" { simple Tk__tok_MultiplicativeExpression_dummy_330 }
<0>       "tok_MultiplicativeOp_dummy_329" { simple Tk__tok_MultiplicativeOp_dummy_329 }
<0>       "tok_OptDocComment_dummy_328" { simple Tk__tok_OptDocComment_dummy_328 }
<0>       "tok_OptElsePart_dummy_327" { simple Tk__tok_OptElsePart_dummy_327 }
<0>       "tok_OptExpression_dummy_326" { simple Tk__tok_OptExpression_dummy_326 }
<0>       "tok_OptFinally_dummy_325" { simple Tk__tok_OptFinally_dummy_325 }
<0>       "tok_OptId_dummy_324" { simple Tk__tok_OptId_dummy_324 }
<0>       "tok_OptVariableInitializer_dummy_323" { simple Tk__tok_OptVariableInitializer_dummy_323 }
<0>       "tok_Package_dummy_322" { simple Tk__tok_Package_dummy_322 }
<0>       "tok_Parameter_dummy_321" { simple Tk__tok_Parameter_dummy_321 }
<0>       "tok_ParameterList_dummy_320" { simple Tk__tok_ParameterList_dummy_320 }
<0>       "tok_PostfixExpression_dummy_319" { simple Tk__tok_PostfixExpression_dummy_319 }
<0>       "tok_PostfixOp_dummy_318" { simple Tk__tok_PostfixOp_dummy_318 }
<0>       "tok_PrefixOp_dummy_317" { simple Tk__tok_PrefixOp_dummy_317 }
<0>       "tok_PrimaryNoPostfix_dummy_316" { simple Tk__tok_PrimaryNoPostfix_dummy_316 }
<0>       "tok_PrimitiveTypeKeyword_dummy_315" { simple Tk__tok_PrimitiveTypeKeyword_dummy_315 }
<0>       "tok_RelationalExpression_dummy_314" { simple Tk__tok_RelationalExpression_dummy_314 }
<0>       "tok_RelationalOp_dummy_313" { simple Tk__tok_RelationalOp_dummy_313 }
<0>       "tok_ShiftExpression_dummy_312" { simple Tk__tok_ShiftExpression_dummy_312 }
<0>       "tok_ShiftOp_dummy_311" { simple Tk__tok_ShiftOp_dummy_311 }
<0>       "tok_SquareBracketsList_dummy_310" { simple Tk__tok_SquareBracketsList_dummy_310 }
<0>       "tok_Statement_dummy_309" { simple Tk__tok_Statement_dummy_309 }
<0>       "tok_StatementBlock_dummy_308" { simple Tk__tok_StatementBlock_dummy_308 }
<0>       "tok_StatementList_dummy_307" { simple Tk__tok_StatementList_dummy_307 }
<0>       "tok_StatementWithoutIf_dummy_306" { simple Tk__tok_StatementWithoutIf_dummy_306 }
<0>       "tok_StaticInitializer_dummy_305" { simple Tk__tok_StaticInitializer_dummy_305 }
<0>       "tok_SwitchCaseList_dummy_304" { simple Tk__tok_SwitchCaseList_dummy_304 }
<0>       "tok_SwitchStatement_dummy_303" { simple Tk__tok_SwitchStatement_dummy_303 }
<0>       "tok_TryStatement_dummy_302" { simple Tk__tok_TryStatement_dummy_302 }
<0>       "tok_Type_dummy_301" { simple Tk__tok_Type_dummy_301 }
<0>       "tok_TypeArgument_dummy_300" { simple Tk__tok_TypeArgument_dummy_300 }
<0>       "tok_TypeArguments_dummy_299" { simple Tk__tok_TypeArguments_dummy_299 }
<0>       "tok_TypeDeclaration_dummy_298" { simple Tk__tok_TypeDeclaration_dummy_298 }
<0>       "tok_TypeParameter_dummy_297" { simple Tk__tok_TypeParameter_dummy_297 }
<0>       "tok_TypeParameters_dummy_296" { simple Tk__tok_TypeParameters_dummy_296 }
<0>       "tok_TypeSpecifier_dummy_295" { simple Tk__tok_TypeSpecifier_dummy_295 }
<0>       "tok_UnaryExpression_dummy_294" { simple Tk__tok_UnaryExpression_dummy_294 }
<0>       "tok_UnaryExpressionNotPlusMinus_dummy_293" { simple Tk__tok_UnaryExpressionNotPlusMinus_dummy_293 }
<0>       "tok_VariableDeclaration_dummy_292" { simple Tk__tok_VariableDeclaration_dummy_292 }
<0>       "tok_VariableDeclarator_dummy_291" { simple Tk__tok_VariableDeclarator_dummy_291 }
<0>       "tok_VariableDeclaratorList_dummy_290" { simple Tk__tok_VariableDeclaratorList_dummy_290 }
<0>       "tok_VariableInitializer_dummy_289" { simple Tk__tok_VariableInitializer_dummy_289 }
<0>       "tok_VariableInitializerList_dummy_288" { simple Tk__tok_VariableInitializerList_dummy_288 }
<0>       "tok_WhileStatement_dummy_287" { simple Tk__tok_WhileStatement_dummy_287 }
<0>       "tok_WildcardType_dummy_286" { simple Tk__tok_WildcardType_dummy_286 }
<0>       "~" { simple Tk__tok__tilde__78 }
<0>       "}" { simple Tk__tok__symbol__14 }
<0>       "||" { simple Tk__tok__pipe__pipe__57 }
<0>       "|=" { simple Tk__tok__pipe__eql__49 }
<0>       "|" { simple Tk__tok__pipe__59 }
<0>       "{" { simple Tk__tok__symbol__13 }
<0>       "while" { simple Tk__tok_while_38 }
<0>       "void" { simple Tk__tok_void_28 }
<0>       "try" { simple Tk__tok_try_42 }
<0>       "true" { simple Tk__tok_true_83 }
<0>       "transient" { simple Tk__tok_transient_94 }
<0>       "throw" { simple Tk__tok_throw_31 }
<0>       "threadsafe" { simple Tk__tok_threadsafe_93 }
<0>       "this" { simple Tk__tok_this_80 }
<0>       "synchronized" { simple Tk__tok_synchronized_30 }
<0>       "switch" { simple Tk__tok_switch_44 }
<0>       "super" { simple Tk__tok_super_81 }
<0>       "static" { simple Tk__tok_static_89 }
<0>       "short" { simple Tk__tok_short_23 }
<0>       "return" { simple Tk__tok_return_29 }
<0>       "public" { simple Tk__tok_public_86 }
<0>       "protected" { simple Tk__tok_protected_88 }
<0>       "private" { simple Tk__tok_private_87 }
<0>       "package" { simple Tk__tok_package_0 }
<0>       "null" { simple Tk__tok_null_85 }
<0>       "new" { simple Tk__tok_new_82 }
<0>       "native" { simple Tk__tok_native_91 }
<0>       "long" { simple Tk__tok_long_26 }
<0>       "interface" { simple Tk__tok_interface_15 }
<0>       "int" { simple Tk__tok_int_24 }
<0>       "instanceof" { simple Tk__tok_instanceof_68 }
<0>       "import" { simple Tk__tok_import_2 }
<0>       "implements" { simple Tk__tok_implements_11 }
<0>       "if" { simple Tk__tok_if_36 }
<0>       "for" { simple Tk__tok_for_39 }
<0>       "float" { simple Tk__tok_float_25 }
<0>       "finally" { simple Tk__tok_finally_41 }
<0>       "final" { simple Tk__tok_final_90 }
<0>       "false" { simple Tk__tok_false_84 }
<0>       "extends" { simple Tk__tok_extends_10 }
<0>       "enum" { simple Tk__tok_enum_17 }
<0>       "else" { simple Tk__tok_else_35 }
<0>       "double" { simple Tk__tok_double_27 }
<0>       "do" { simple Tk__tok_do_37 }
<0>       "default" { simple Tk__tok_default_16 }
<0>       "continue" { simple Tk__tok_continue_34 }
<0>       "class" { simple Tk__tok_class_12 }
<0>       "char" { simple Tk__tok_char_22 }
<0>       "catch" { simple Tk__tok_catch_40 }
<0>       "case" { simple Tk__tok_case_43 }
<0>       "byte" { simple Tk__tok_byte_21 }
<0>       "break" { simple Tk__tok_break_33 }
<0>       "boolean" { simple Tk__tok_boolean_20 }
<0>       "abstract" { simple Tk__tok_abstract_92 }
<0>       "^=" { simple Tk__tok__symbol__eql__51 }
<0>       "^" { simple Tk__tok__symbol__60 }
<0>       "]" { simple Tk__tok__sq_bkt_r__19 }
<0>       "[" { simple Tk__tok__sq_bkt_l__18 }
<0>       "@" { simple Tk__tok__symbol__5 }
<0>       "?" { simple Tk__tok__symbol__56 }
<0>       ">>>=" { simple Tk__tok__symbol__symbol__symbol__eql__55 }
<0>       ">>>" { simple Tk__tok__symbol__symbol__symbol__71 }
<0>       ">>=" { simple Tk__tok__symbol__symbol__eql__54 }
<0>       ">>" { simple Tk__tok__symbol__symbol__69 }
<0>       ">=" { simple Tk__tok__symbol__eql__67 }
<0>       ">" { simple Tk__tok__symbol__65 }
<0>       "==" { simple Tk__tok__eql__eql__62 }
<0>       "=" { simple Tk__tok__eql__9 }
<0>       "<=" { simple Tk__tok__symbol__eql__66 }
<0>       "<<=" { simple Tk__tok__symbol__symbol__eql__53 }
<0>       "<<" { simple Tk__tok__symbol__symbol__70 }
<0>       "<" { simple Tk__tok__symbol__64 }
<0>       ";" { simple Tk__tok__semi__1 }
<0>       ":" { simple Tk__tok__colon__32 }
<0>       "/=" { simple Tk__tok__symbol__eql__48 }
<0>       "/" { simple Tk__tok__symbol__74 }
<0>       "." { simple Tk__tok__dot__3 }
<0>       "-=" { simple Tk__tok__minus__eql__46 }
<0>       "--" { simple Tk__tok__minus__minus__77 }
<0>       "-" { simple Tk__tok__minus__73 }
<0>       "," { simple Tk__tok__coma__8 }
<0>       "+=" { simple Tk__tok__plus__eql__45 }
<0>       "++" { simple Tk__tok__plus__plus__76 }
<0>       "+" { simple Tk__tok__plus__72 }
<0>       "*=" { simple Tk__tok__star__eql__47 }
<0>       "*" { simple Tk__tok__star__4 }
<0>       ")" { simple Tk__tok__rparen__7 }
<0>       "(" { simple Tk__tok__lparen__6 }
<0>       "&=" { simple Tk__tok__symbol__eql__50 }
<0>       "&&" { simple Tk__tok__symbol__symbol__58 }
<0>       "&" { simple Tk__tok__symbol__61 }
<0>       "%=" { simple Tk__tok__symbol__eql__52 }
<0>       "%" { simple Tk__tok__symbol__75 }
<0>       "!=" { simple Tk__tok__exclamation__eql__63 }
<0>       "!" { simple Tk__tok__exclamation__79 }

<0>       ([\ \t\n\r]+) ;
<0>       ([a-zA-Z\$_]  [a-zA-Z\$_0-9]*) { simple1 $  Tk__id . (id) }
<0>       ([\"]  [\x20-\x21\x23-\x7E]*  [\"]) { simple1 $  Tk__string . (id) }
<0>       (\047  [\x00-\xff]  \047) { simple1 $  Tk__char . (id) }
<0>       ("f"| "d") { simple1 $  Tk__floatTypeSuffix . (id) }
<0>       ("e"  ("+"| "-") ?  [0-9]+) { simple1 $  Tk__exponentPart . (id) }
<0>       ([0-9]+  "."  ([0-9]+) ?  @exponentPart ?  @floatTypeSuffix ?| "."  [0-9]+  @exponentPart ?  @floatTypeSuffix ?| [0-9]+  @floatTypeSuffix) { simple1 $  Tk__floatLiteral . (id) }
<0>       ((([1-9]  [0-9]*)| ("0"  [0-7]*)| ("0"  "x"  [0-9a-fA-F]+))  [l] ?) { simple1 $  Tk__integerLiteral . (id) }
<0>       ("$"  "CompoundName"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_CompoundName284 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "Modifier"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Modifier280 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "TypeSpecifier"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TypeSpecifier278 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "Type"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Type276 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "TypeParameter"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TypeParameter274 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "TypeParameters"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TypeParameters268 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "WildcardType"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_WildcardType263 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "TypeArgument"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TypeArgument261 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "TypeArguments"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TypeArguments259 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "Arglist"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Arglist254 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "Literal"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Literal249 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "CreationExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_CreationExpression247 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "PrimaryNoPostfix"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_PrimaryNoPostfix244 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "PostfixExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_PostfixExpression238 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "UnaryExpressionNotPlusMinus"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_UnaryExpressionNotPlusMinus236 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "UnaryExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_UnaryExpression234 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "PostfixOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_PostfixOp232 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "PrefixOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_PrefixOp230 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "CastExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_CastExpression228 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "MultiplicativeExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_MultiplicativeExpression226 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "MultiplicativeOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_MultiplicativeOp224 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "AdditiveExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AdditiveExpression222 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "AdditiveOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AdditiveOp220 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "ShiftExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ShiftExpression218 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "ShiftOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ShiftOp216 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "RelationalExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_RelationalExpression214 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "RelationalOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_RelationalOp212 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "EqualityExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_EqualityExpression210 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "EqualityOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_EqualityOp208 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "AndExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AndExpression206 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "ExclusiveOrExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ExclusiveOrExpression204 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "InclusiveOrEpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_InclusiveOrEpression202 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "ConditionalAndExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ConditionalAndExpression200 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "ConditionalOrExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ConditionalOrExpression198 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "ConditionalExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ConditionalExpression196 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "AssignmentOp"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AssignmentOp194 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "AssignmentExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AssignmentExpression192 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "Expression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Expression188 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "SwitchStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_SwitchStatement186 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "SwitchCaseList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_SwitchCaseList184 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "TryStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TryStatement178 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "OptFinally"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptFinally176 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "CatchList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_CatchList173 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "ForStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ForStatement169 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "WhileStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_WhileStatement165 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "DoStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_DoStatement163 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "IfStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_IfStatement161 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "OptElsePart"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptElsePart159 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "StatementWithoutIf"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_StatementWithoutIf156 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "Statement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Statement154 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "OptId"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptId152 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "OptExpression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptExpression150 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "StatementList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_StatementList148 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "Parameter"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Parameter145 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "ParameterList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ParameterList143 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "StaticInitializer"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_StaticInitializer139 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "VariableInitializer"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_VariableInitializer137 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "VariableInitializerList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_VariableInitializerList135 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "VariableDeclarator"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_VariableDeclarator128 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "OptVariableInitializer"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptVariableInitializer126 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "VariableDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_VariableDeclaration123 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "VariableDeclaratorList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_VariableDeclaratorList121 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "StatementBlock"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_StatementBlock117 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "MoreVariableDeclarators"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_MoreVariableDeclarators115 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "MemberRest"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_MemberRest111 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "MoreTypeSpecifier"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_MoreTypeSpecifier107 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "MemberAfterFirstId"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_MemberAfterFirstId105 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "PrimitiveTypeKeyword"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_PrimitiveTypeKeyword102 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "MemberDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_MemberDeclaration100 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "SquareBracketsList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_SquareBracketsList98 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "FieldDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_FieldDeclaration94 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "EnumDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_EnumDeclaration87 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "EnumConstantList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_EnumConstantList82 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "EnumConstant"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_EnumConstant76 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "AnnotationTypeElement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AnnotationTypeElement70 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "AnnotationTypeElementList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AnnotationTypeElementList66 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "AnnotationDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AnnotationDeclaration63 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "InterfaceDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_InterfaceDeclaration61 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "ClassDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ClassDeclaration59 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "FieldDeclarationList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_FieldDeclarationList55 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "ImplementsList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ImplementsList52 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "ExtendsList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ExtendsList49 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "ModifierList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ModifierList44 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "AnnotationList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AnnotationList40 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "AnnotationElement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AnnotationElement37 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "AnnotationArguments"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_AnnotationArguments33 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "Annotation"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Annotation29 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "DocComment"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_DocComment24 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "ImportStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ImportStatement22 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "Package"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Package18 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "CompilationUnit"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_CompilationUnit16 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "ImportList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ImportList10 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "TypeDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_TypeDeclaration6 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "OptDocComment"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_OptDocComment3 . ((tail . dropWhile (/= ':'))) }
<0>       ("$"  "Java"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Java0 . ((tail . dropWhile (/= ':'))) }
          . { rtkError }

{
data Token = EndOfFile |
             Tk__tok_AdditiveExpression_dummy_379 |
             Tk__tok_AdditiveOp_dummy_378 |
             Tk__tok_AndExpression_dummy_377 |
             Tk__tok_Annotation_dummy_376 |
             Tk__tok_AnnotationArguments_dummy_375 |
             Tk__tok_AnnotationDeclaration_dummy_374 |
             Tk__tok_AnnotationElement_dummy_373 |
             Tk__tok_AnnotationList_dummy_372 |
             Tk__tok_AnnotationTypeElement_dummy_371 |
             Tk__tok_AnnotationTypeElementList_dummy_370 |
             Tk__tok_Arglist_dummy_369 |
             Tk__tok_AssignmentExpression_dummy_368 |
             Tk__tok_AssignmentOp_dummy_367 |
             Tk__tok_CastExpression_dummy_366 |
             Tk__tok_CatchList_dummy_365 |
             Tk__tok_ClassDeclaration_dummy_364 |
             Tk__tok_CompilationUnit_dummy_363 |
             Tk__tok_CompoundName_dummy_362 |
             Tk__tok_ConditionalAndExpression_dummy_361 |
             Tk__tok_ConditionalExpression_dummy_360 |
             Tk__tok_ConditionalOrExpression_dummy_359 |
             Tk__tok_CreationExpression_dummy_358 |
             Tk__tok_DoStatement_dummy_357 |
             Tk__tok_DocComment_dummy_356 |
             Tk__tok_EnumConstant_dummy_355 |
             Tk__tok_EnumConstantList_dummy_354 |
             Tk__tok_EnumDeclaration_dummy_353 |
             Tk__tok_EqualityExpression_dummy_352 |
             Tk__tok_EqualityOp_dummy_351 |
             Tk__tok_ExclusiveOrExpression_dummy_350 |
             Tk__tok_Expression_dummy_349 |
             Tk__tok_ExtendsList_dummy_348 |
             Tk__tok_FieldDeclaration_dummy_347 |
             Tk__tok_FieldDeclarationList_dummy_346 |
             Tk__tok_ForStatement_dummy_345 |
             Tk__tok_IfStatement_dummy_344 |
             Tk__tok_ImplementsList_dummy_343 |
             Tk__tok_ImportList_dummy_342 |
             Tk__tok_ImportStatement_dummy_341 |
             Tk__tok_InclusiveOrEpression_dummy_340 |
             Tk__tok_InterfaceDeclaration_dummy_339 |
             Tk__tok_Java_dummy_380 |
             Tk__tok_Literal_dummy_338 |
             Tk__tok_MemberAfterFirstId_dummy_337 |
             Tk__tok_MemberDeclaration_dummy_336 |
             Tk__tok_MemberRest_dummy_335 |
             Tk__tok_Modifier_dummy_334 |
             Tk__tok_ModifierList_dummy_333 |
             Tk__tok_MoreTypeSpecifier_dummy_332 |
             Tk__tok_MoreVariableDeclarators_dummy_331 |
             Tk__tok_MultiplicativeExpression_dummy_330 |
             Tk__tok_MultiplicativeOp_dummy_329 |
             Tk__tok_OptDocComment_dummy_328 |
             Tk__tok_OptElsePart_dummy_327 |
             Tk__tok_OptExpression_dummy_326 |
             Tk__tok_OptFinally_dummy_325 |
             Tk__tok_OptId_dummy_324 |
             Tk__tok_OptVariableInitializer_dummy_323 |
             Tk__tok_Package_dummy_322 |
             Tk__tok_Parameter_dummy_321 |
             Tk__tok_ParameterList_dummy_320 |
             Tk__tok_PostfixExpression_dummy_319 |
             Tk__tok_PostfixOp_dummy_318 |
             Tk__tok_PrefixOp_dummy_317 |
             Tk__tok_PrimaryNoPostfix_dummy_316 |
             Tk__tok_PrimitiveTypeKeyword_dummy_315 |
             Tk__tok_RelationalExpression_dummy_314 |
             Tk__tok_RelationalOp_dummy_313 |
             Tk__tok_ShiftExpression_dummy_312 |
             Tk__tok_ShiftOp_dummy_311 |
             Tk__tok_SquareBracketsList_dummy_310 |
             Tk__tok_Statement_dummy_309 |
             Tk__tok_StatementBlock_dummy_308 |
             Tk__tok_StatementList_dummy_307 |
             Tk__tok_StatementWithoutIf_dummy_306 |
             Tk__tok_StaticInitializer_dummy_305 |
             Tk__tok_SwitchCaseList_dummy_304 |
             Tk__tok_SwitchStatement_dummy_303 |
             Tk__tok_TryStatement_dummy_302 |
             Tk__tok_Type_dummy_301 |
             Tk__tok_TypeArgument_dummy_300 |
             Tk__tok_TypeArguments_dummy_299 |
             Tk__tok_TypeDeclaration_dummy_298 |
             Tk__tok_TypeParameter_dummy_297 |
             Tk__tok_TypeParameters_dummy_296 |
             Tk__tok_TypeSpecifier_dummy_295 |
             Tk__tok_UnaryExpression_dummy_294 |
             Tk__tok_UnaryExpressionNotPlusMinus_dummy_293 |
             Tk__tok_VariableDeclaration_dummy_292 |
             Tk__tok_VariableDeclarator_dummy_291 |
             Tk__tok_VariableDeclaratorList_dummy_290 |
             Tk__tok_VariableInitializer_dummy_289 |
             Tk__tok_VariableInitializerList_dummy_288 |
             Tk__tok_WhileStatement_dummy_287 |
             Tk__tok_WildcardType_dummy_286 |
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
             Tk__qq_CompoundName284 String |
             Tk__qq_Modifier280 String |
             Tk__qq_TypeSpecifier278 String |
             Tk__qq_Type276 String |
             Tk__qq_TypeParameter274 String |
             Tk__qq_TypeParameters268 String |
             Tk__qq_WildcardType263 String |
             Tk__qq_TypeArgument261 String |
             Tk__qq_TypeArguments259 String |
             Tk__qq_Arglist254 String |
             Tk__qq_Literal249 String |
             Tk__qq_CreationExpression247 String |
             Tk__qq_PrimaryNoPostfix244 String |
             Tk__qq_PostfixExpression238 String |
             Tk__qq_UnaryExpressionNotPlusMinus236 String |
             Tk__qq_UnaryExpression234 String |
             Tk__qq_PostfixOp232 String |
             Tk__qq_PrefixOp230 String |
             Tk__qq_CastExpression228 String |
             Tk__qq_MultiplicativeExpression226 String |
             Tk__qq_MultiplicativeOp224 String |
             Tk__qq_AdditiveExpression222 String |
             Tk__qq_AdditiveOp220 String |
             Tk__qq_ShiftExpression218 String |
             Tk__qq_ShiftOp216 String |
             Tk__qq_RelationalExpression214 String |
             Tk__qq_RelationalOp212 String |
             Tk__qq_EqualityExpression210 String |
             Tk__qq_EqualityOp208 String |
             Tk__qq_AndExpression206 String |
             Tk__qq_ExclusiveOrExpression204 String |
             Tk__qq_InclusiveOrEpression202 String |
             Tk__qq_ConditionalAndExpression200 String |
             Tk__qq_ConditionalOrExpression198 String |
             Tk__qq_ConditionalExpression196 String |
             Tk__qq_AssignmentOp194 String |
             Tk__qq_AssignmentExpression192 String |
             Tk__qq_Expression188 String |
             Tk__qq_SwitchStatement186 String |
             Tk__qq_SwitchCaseList184 String |
             Tk__qq_TryStatement178 String |
             Tk__qq_OptFinally176 String |
             Tk__qq_CatchList173 String |
             Tk__qq_ForStatement169 String |
             Tk__qq_WhileStatement165 String |
             Tk__qq_DoStatement163 String |
             Tk__qq_IfStatement161 String |
             Tk__qq_OptElsePart159 String |
             Tk__qq_StatementWithoutIf156 String |
             Tk__qq_Statement154 String |
             Tk__qq_OptId152 String |
             Tk__qq_OptExpression150 String |
             Tk__qq_StatementList148 String |
             Tk__qq_Parameter145 String |
             Tk__qq_ParameterList143 String |
             Tk__qq_StaticInitializer139 String |
             Tk__qq_VariableInitializer137 String |
             Tk__qq_VariableInitializerList135 String |
             Tk__qq_VariableDeclarator128 String |
             Tk__qq_OptVariableInitializer126 String |
             Tk__qq_VariableDeclaration123 String |
             Tk__qq_VariableDeclaratorList121 String |
             Tk__qq_StatementBlock117 String |
             Tk__qq_MoreVariableDeclarators115 String |
             Tk__qq_MemberRest111 String |
             Tk__qq_MoreTypeSpecifier107 String |
             Tk__qq_MemberAfterFirstId105 String |
             Tk__qq_PrimitiveTypeKeyword102 String |
             Tk__qq_MemberDeclaration100 String |
             Tk__qq_SquareBracketsList98 String |
             Tk__qq_FieldDeclaration94 String |
             Tk__qq_EnumDeclaration87 String |
             Tk__qq_EnumConstantList82 String |
             Tk__qq_EnumConstant76 String |
             Tk__qq_AnnotationTypeElement70 String |
             Tk__qq_AnnotationTypeElementList66 String |
             Tk__qq_AnnotationDeclaration63 String |
             Tk__qq_InterfaceDeclaration61 String |
             Tk__qq_ClassDeclaration59 String |
             Tk__qq_FieldDeclarationList55 String |
             Tk__qq_ImplementsList52 String |
             Tk__qq_ExtendsList49 String |
             Tk__qq_ModifierList44 String |
             Tk__qq_AnnotationList40 String |
             Tk__qq_AnnotationElement37 String |
             Tk__qq_AnnotationArguments33 String |
             Tk__qq_Annotation29 String |
             Tk__qq_DocComment24 String |
             Tk__qq_ImportStatement22 String |
             Tk__qq_Package18 String |
             Tk__qq_CompilationUnit16 String |
             Tk__qq_ImportList10 String |
             Tk__qq_TypeDeclaration6 String |
             Tk__qq_OptDocComment3 String |
             Tk__qq_Java0 String
             deriving (Show)

alexEOF = return EndOfFile
alexScanTokens :: String -> [Token]
alexScanTokens str = 
               case alexScanTokens1 str of
                  Right toks -> toks
                  Left err -> error err

alexScanTokens1 str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                       EndOfFile -> return $ reverse toks
                       _ -> let toks' = tok : toks 
                            in toks' `seq` loop toks'
  loop []
simple1 :: (String -> Token) -> AlexInput -> Int -> Alex Token
simple1 t (_, _, _, str) len = return $ t (take len str)

simple t input len = return t

rtkError ((AlexPn _ line column), _, _, str) len = alexError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column" ++ ". Following chars :" ++ (take 10 str)

}