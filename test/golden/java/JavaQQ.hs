{-# LANGUAGE TemplateHaskell #-}
module JavaQQ
where

import Text.Regex.Posix
import Text.Regex.Base
import qualified Data.Map as M
import Data.List
import Data.Maybe
import qualified Data.Generics as Generics
import qualified Data.Data as Data
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import JavaLexer
import JavaParser

qqPattern = "\\$[A-Za-z_][A-Za-z_0-9]*[^A-Za-z_0-9:]"

qqShortcuts :: M.Map String String

-- A $name metavariable is rewritten to $Type:name using the qqShortcuts
-- table below. The rewrite is purely textual, so it would also fire inside
-- the quoted language's own string literals: write $$name there to escape
-- it and get the literal text $name. Each '$$' pair directly before a
-- metavariable stands for one literal '$' (so $$$x is a literal '$'
-- followed by the metavariable $x). A '$' not followed by an identifier is
-- never rewritten and needs no escape.
replaceAllPatterns1 :: String -> String
replaceAllPatterns1 str = let (pre, match, post) = str =~ qqPattern :: (String, String, String)
                          in if match == ""
                              then pre
                              else let varName = init $ tail match
                                       addSym = last match
                                       escCount = length $ takeWhile (== '$') $ reverse pre
                                       keptPre = take (length pre - escCount) pre ++ replicate (div escCount 2) '$'
                                       ruleVariants = catMaybes $ map (\ prefix -> M.lookup prefix qqShortcuts) $ reverse $ inits varName
                                       rule = case ruleVariants of
                                                [] -> error $ unlines
                                                        [ "Unknown metavariable $" ++ varName ++ " in quasi-quote:"
                                                        , "no prefix of '" ++ varName ++ "' is a known shortcut. Known shortcuts:"
                                                        , "  " ++ intercalate ", " (M.keys qqShortcuts)
                                                        , "To include the literal text $" ++ varName ++ " in the quoted code"
                                                        , "(e.g. inside a string literal), escape it as $$" ++ varName ++ "." ]
                                                (rule : _) -> rule
                                   in if odd escCount
                                       then keptPre ++ ('$' : varName) ++ (replaceAllPatterns1 $ addSym : post)
                                       else keptPre ++ ('$' : rule ++ ":") ++ varName ++ (replaceAllPatterns1 $ addSym : post)

-- Add ' ' at the end, so regex can match variable in the end of the string
replaceAllPatterns :: String -> String
replaceAllPatterns str = init $ replaceAllPatterns1 (str ++ " ")

qqShortcuts = M.fromList [ ("java","Java"),("additiveOp","AdditiveOp"),("annotation","Annotation"),("annotationArguments","AnnotationArguments"),("annotationDeclaration","AnnotationDeclaration"),("annotationElement","AnnotationElement"),("annotationList","AnnotationList"),("annotationTypeElement","AnnotationTypeElement"),("annotationTypeElementList","AnnotationTypeElementList"),("arglist","Arglist"),("assignmentOp","AssignmentOp"),("catchList","CatchList"),("classDeclaration","ClassDeclaration"),("compilationUnit","CompilationUnit"),("compoundName","CompoundName"),("creationExpression","CreationExpression"),("doStatement","DoStatement"),("docComment","DocComment"),("enumConstant","EnumConstant"),("enumConstantList","EnumConstantList"),("enumDeclaration","EnumDeclaration"),("equalityOp","EqualityOp"),("expression","Expression"),("extendsList","ExtendsList"),("fieldDeclaration","FieldDeclaration"),("fieldDeclarationList","FieldDeclarationList"),("forStatement","ForStatement"),("ifStatement","IfStatement"),("implementsList","ImplementsList"),("importList","ImportList"),("importStatement","ImportStatement"),("interfaceDeclaration","InterfaceDeclaration"),("literal","Literal"),("memberAfterFirstId","MemberAfterFirstId"),("memberDeclaration","MemberDeclaration"),("memberRest","MemberRest"),("modifier","Modifier"),("modifierList","ModifierList"),("moreTypeSpecifier","MoreTypeSpecifier"),("moreVariableDeclarators","MoreVariableDeclarators"),("multiplicativeOp","MultiplicativeOp"),("nestedTypeDeclaration","NestedTypeDeclaration"),("optDocComment","OptDocComment"),("optElsePart","OptElsePart"),("optExpression","OptExpression"),("optFinally","OptFinally"),("optId","OptId"),("optVariableInitializer","OptVariableInitializer"),("package","Package"),("parameter","Parameter"),("parameterList","ParameterList"),("postfixOp","PostfixOp"),("prefixOp","PrefixOp"),("primitiveTypeKeyword","PrimitiveTypeKeyword"),("relationalOp","RelationalOp"),("shiftOp","ShiftOp"),("squareBracketsList","SquareBracketsList"),("statement","Statement"),("statementBlock","StatementBlock"),("statementList","StatementList"),("statementWithoutIf","StatementWithoutIf"),("staticInitializer","StaticInitializer"),("switchCaseList","SwitchCaseList"),("switchStatement","SwitchStatement"),("tryStatement","TryStatement"),("type","Type"),("typeArgument","TypeArgument"),("typeArguments","TypeArguments"),("typeDeclaration","TypeDeclaration"),("typeParameter","TypeParameter"),("typeParameters","TypeParameters"),("typeSpecifier","TypeSpecifier"),("variableDeclaration","VariableDeclaration"),("variableDeclarator","VariableDeclarator"),("variableDeclaratorList","VariableDeclaratorList"),("variableInitializer","VariableInitializer"),("variableInitializerList","VariableInitializerList"),("whileStatement","WhileStatement"),("wildcardType","WildcardType")]

quoteJavaExp :: Data.Data a => String -> (Java -> a) -> String -> TH.ExpQ
quoteJavaExp dummy func s = do
  let s1 = replaceAllPatterns s
      expr = func $ parseJava $ alexScanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy)
  dataToExpQ (const Nothing `Generics.extQ` antiJavaExp `Generics.extQ` antiOptDocCommentExp `Generics.extQ` antiTypeDeclarationExp `Generics.extQ` antiRule_2Exp `Generics.extQ` antiCompilationUnitExp `Generics.extQ` antiPackageExp `Generics.extQ` antiImportStatementExp `Generics.extQ` antiDocCommentExp `Generics.extQ` antiAnnotationExp `Generics.extQ` antiAnnotationArgumentsExp `Generics.extQ` antiAnnotationElementExp `Generics.extQ` antiRule_18Exp `Generics.extQ` antiExtendsListExp `Generics.extQ` antiImplementsListExp `Generics.extQ` antiFieldDeclarationExp `Generics.extQ` antiClassDeclarationExp `Generics.extQ` antiInterfaceDeclarationExp `Generics.extQ` antiAnnotationDeclarationExp `Generics.extQ` antiAnnotationTypeElementExp `Generics.extQ` antiEnumConstantExp `Generics.extQ` antiEnumConstantListExp `Generics.extQ` antiEnumDeclarationExp `Generics.extQ` antiNestedTypeDeclarationExp `Generics.extQ` antiRule_44Exp `Generics.extQ` antiMemberDeclarationExp `Generics.extQ` antiPrimitiveTypeKeywordExp `Generics.extQ` antiMemberAfterFirstIdExp `Generics.extQ` antiMoreTypeSpecifierExp `Generics.extQ` antiMemberRestExp `Generics.extQ` antiRule_49Exp `Generics.extQ` antiStatementBlockExp `Generics.extQ` antiVariableDeclaratorListExp `Generics.extQ` antiVariableDeclarationExp `Generics.extQ` antiOptVariableInitializerExp `Generics.extQ` antiVariableDeclaratorExp `Generics.extQ` antiVariableInitializerListExp `Generics.extQ` antiVariableInitializerExp `Generics.extQ` antiStaticInitializerExp `Generics.extQ` antiParameterListExp `Generics.extQ` antiParameterExp `Generics.extQ` antiStatementExp `Generics.extQ` antiOptExpressionExp `Generics.extQ` antiOptIdExp `Generics.extQ` antiStatementWithoutIfExp `Generics.extQ` antiOptElsePartExp `Generics.extQ` antiIfStatementExp `Generics.extQ` antiDoStatementExp `Generics.extQ` antiWhileStatementExp `Generics.extQ` antiForStatementExp `Generics.extQ` antiRule_65Exp `Generics.extQ` antiOptFinallyExp `Generics.extQ` antiTryStatementExp `Generics.extQ` antiRule_68Exp `Generics.extQ` antiSwitchStatementExp `Generics.extQ` antiExpressionExp `Generics.extQ` antiAssignmentOpExp `Generics.extQ` antiEqualityOpExp `Generics.extQ` antiRelationalOpExp `Generics.extQ` antiShiftOpExp `Generics.extQ` antiAdditiveOpExp `Generics.extQ` antiMultiplicativeOpExp `Generics.extQ` antiPrefixOpExp `Generics.extQ` antiPostfixOpExp `Generics.extQ` antiCreationExpressionExp `Generics.extQ` antiLiteralExp `Generics.extQ` antiArglistExp `Generics.extQ` antiTypeArgumentsExp `Generics.extQ` antiTypeArgumentExp `Generics.extQ` antiWildcardTypeExp `Generics.extQ` antiTypeParametersExp `Generics.extQ` antiTypeParameterExp `Generics.extQ` antiTypeExp `Generics.extQ` antiTypeSpecifierExp `Generics.extQ` antiModifierExp `Generics.extQ` antiCompoundNameExp) expr
quoteJavaPat :: Data.Data a => String -> (Java -> a) -> String -> TH.PatQ
quoteJavaPat dummy func s = do
  let s1 = replaceAllPatterns s
      expr = func $ parseJava $ alexScanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy)
  dataToPatQ (const Nothing `Generics.extQ` antiJavaPat `Generics.extQ` antiOptDocCommentPat `Generics.extQ` antiTypeDeclarationPat `Generics.extQ` antiRule_2Pat `Generics.extQ` antiCompilationUnitPat `Generics.extQ` antiPackagePat `Generics.extQ` antiImportStatementPat `Generics.extQ` antiDocCommentPat `Generics.extQ` antiAnnotationPat `Generics.extQ` antiAnnotationArgumentsPat `Generics.extQ` antiAnnotationElementPat `Generics.extQ` antiRule_18Pat `Generics.extQ` antiExtendsListPat `Generics.extQ` antiImplementsListPat `Generics.extQ` antiFieldDeclarationPat `Generics.extQ` antiClassDeclarationPat `Generics.extQ` antiInterfaceDeclarationPat `Generics.extQ` antiAnnotationDeclarationPat `Generics.extQ` antiAnnotationTypeElementPat `Generics.extQ` antiEnumConstantPat `Generics.extQ` antiEnumConstantListPat `Generics.extQ` antiEnumDeclarationPat `Generics.extQ` antiNestedTypeDeclarationPat `Generics.extQ` antiRule_44Pat `Generics.extQ` antiMemberDeclarationPat `Generics.extQ` antiPrimitiveTypeKeywordPat `Generics.extQ` antiMemberAfterFirstIdPat `Generics.extQ` antiMoreTypeSpecifierPat `Generics.extQ` antiMemberRestPat `Generics.extQ` antiRule_49Pat `Generics.extQ` antiStatementBlockPat `Generics.extQ` antiVariableDeclaratorListPat `Generics.extQ` antiVariableDeclarationPat `Generics.extQ` antiOptVariableInitializerPat `Generics.extQ` antiVariableDeclaratorPat `Generics.extQ` antiVariableInitializerListPat `Generics.extQ` antiVariableInitializerPat `Generics.extQ` antiStaticInitializerPat `Generics.extQ` antiParameterListPat `Generics.extQ` antiParameterPat `Generics.extQ` antiStatementPat `Generics.extQ` antiOptExpressionPat `Generics.extQ` antiOptIdPat `Generics.extQ` antiStatementWithoutIfPat `Generics.extQ` antiOptElsePartPat `Generics.extQ` antiIfStatementPat `Generics.extQ` antiDoStatementPat `Generics.extQ` antiWhileStatementPat `Generics.extQ` antiForStatementPat `Generics.extQ` antiRule_65Pat `Generics.extQ` antiOptFinallyPat `Generics.extQ` antiTryStatementPat `Generics.extQ` antiRule_68Pat `Generics.extQ` antiSwitchStatementPat `Generics.extQ` antiExpressionPat `Generics.extQ` antiAssignmentOpPat `Generics.extQ` antiEqualityOpPat `Generics.extQ` antiRelationalOpPat `Generics.extQ` antiShiftOpPat `Generics.extQ` antiAdditiveOpPat `Generics.extQ` antiMultiplicativeOpPat `Generics.extQ` antiPrefixOpPat `Generics.extQ` antiPostfixOpPat `Generics.extQ` antiCreationExpressionPat `Generics.extQ` antiLiteralPat `Generics.extQ` antiArglistPat `Generics.extQ` antiTypeArgumentsPat `Generics.extQ` antiTypeArgumentPat `Generics.extQ` antiWildcardTypePat `Generics.extQ` antiTypeParametersPat `Generics.extQ` antiTypeParameterPat `Generics.extQ` antiTypePat `Generics.extQ` antiTypeSpecifierPat `Generics.extQ` antiModifierPat `Generics.extQ` antiCompoundNamePat) expr

antiCompoundNameExp :: CompoundName -> Maybe (TH.Q TH.Exp )
antiCompoundNameExp ( Anti_CompoundName v) = Just $ TH.varE (TH.mkName v)
antiCompoundNameExp _ = Nothing


antiModifierExp :: Modifier -> Maybe (TH.Q TH.Exp )
antiModifierExp ( Anti_Modifier v) = Just $ TH.varE (TH.mkName v)
antiModifierExp _ = Nothing


antiTypeSpecifierExp :: TypeSpecifier -> Maybe (TH.Q TH.Exp )
antiTypeSpecifierExp ( Anti_TypeSpecifier v) = Just $ TH.varE (TH.mkName v)
antiTypeSpecifierExp _ = Nothing


antiTypeExp :: Type -> Maybe (TH.Q TH.Exp )
antiTypeExp ( Anti_Type v) = Just $ TH.varE (TH.mkName v)
antiTypeExp _ = Nothing


antiTypeParameterExp :: TypeParameter -> Maybe (TH.Q TH.Exp )
antiTypeParameterExp ( Anti_TypeParameter v) = Just $ TH.varE (TH.mkName v)
antiTypeParameterExp _ = Nothing


antiTypeParametersExp :: TypeParameters -> Maybe (TH.Q TH.Exp )
antiTypeParametersExp ( Anti_TypeParameters v) = Just $ TH.varE (TH.mkName v)
antiTypeParametersExp _ = Nothing


antiWildcardTypeExp :: WildcardType -> Maybe (TH.Q TH.Exp )
antiWildcardTypeExp ( Anti_WildcardType v) = Just $ TH.varE (TH.mkName v)
antiWildcardTypeExp _ = Nothing


antiTypeArgumentExp :: TypeArgument -> Maybe (TH.Q TH.Exp )
antiTypeArgumentExp ( Anti_TypeArgument v) = Just $ TH.varE (TH.mkName v)
antiTypeArgumentExp _ = Nothing


antiTypeArgumentsExp :: TypeArguments -> Maybe (TH.Q TH.Exp )
antiTypeArgumentsExp ( Anti_TypeArguments v) = Just $ TH.varE (TH.mkName v)
antiTypeArgumentsExp _ = Nothing


antiArglistExp :: Arglist -> Maybe (TH.Q TH.Exp )
antiArglistExp ( Anti_Arglist v) = Just $ TH.varE (TH.mkName v)
antiArglistExp _ = Nothing


antiLiteralExp :: Literal -> Maybe (TH.Q TH.Exp )
antiLiteralExp ( Anti_Literal v) = Just $ TH.varE (TH.mkName v)
antiLiteralExp _ = Nothing


antiCreationExpressionExp :: CreationExpression -> Maybe (TH.Q TH.Exp )
antiCreationExpressionExp ( Anti_CreationExpression v) = Just $ TH.varE (TH.mkName v)
antiCreationExpressionExp _ = Nothing


antiPostfixOpExp :: PostfixOp -> Maybe (TH.Q TH.Exp )
antiPostfixOpExp ( Anti_PostfixOp v) = Just $ TH.varE (TH.mkName v)
antiPostfixOpExp _ = Nothing


antiPrefixOpExp :: PrefixOp -> Maybe (TH.Q TH.Exp )
antiPrefixOpExp ( Anti_PrefixOp v) = Just $ TH.varE (TH.mkName v)
antiPrefixOpExp _ = Nothing


antiMultiplicativeOpExp :: MultiplicativeOp -> Maybe (TH.Q TH.Exp )
antiMultiplicativeOpExp ( Anti_MultiplicativeOp v) = Just $ TH.varE (TH.mkName v)
antiMultiplicativeOpExp _ = Nothing


antiAdditiveOpExp :: AdditiveOp -> Maybe (TH.Q TH.Exp )
antiAdditiveOpExp ( Anti_AdditiveOp v) = Just $ TH.varE (TH.mkName v)
antiAdditiveOpExp _ = Nothing


antiShiftOpExp :: ShiftOp -> Maybe (TH.Q TH.Exp )
antiShiftOpExp ( Anti_ShiftOp v) = Just $ TH.varE (TH.mkName v)
antiShiftOpExp _ = Nothing


antiRelationalOpExp :: RelationalOp -> Maybe (TH.Q TH.Exp )
antiRelationalOpExp ( Anti_RelationalOp v) = Just $ TH.varE (TH.mkName v)
antiRelationalOpExp _ = Nothing


antiEqualityOpExp :: EqualityOp -> Maybe (TH.Q TH.Exp )
antiEqualityOpExp ( Anti_EqualityOp v) = Just $ TH.varE (TH.mkName v)
antiEqualityOpExp _ = Nothing


antiAssignmentOpExp :: AssignmentOp -> Maybe (TH.Q TH.Exp )
antiAssignmentOpExp ( Anti_AssignmentOp v) = Just $ TH.varE (TH.mkName v)
antiAssignmentOpExp _ = Nothing


antiExpressionExp :: Expression -> Maybe (TH.Q TH.Exp )
antiExpressionExp ( Anti_Expression v) = Just $ TH.varE (TH.mkName v)
antiExpressionExp _ = Nothing


antiSwitchStatementExp :: SwitchStatement -> Maybe (TH.Q TH.Exp )
antiSwitchStatementExp ( Anti_SwitchStatement v) = Just $ TH.varE (TH.mkName v)
antiSwitchStatementExp _ = Nothing


antiRule_68Exp :: [ Rule_68 ] -> Maybe (TH.Q TH.Exp)
antiRule_68Exp ((Anti_Rule_68 v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiJavaExp `Generics.extQ` antiOptDocCommentExp `Generics.extQ` antiTypeDeclarationExp `Generics.extQ` antiRule_2Exp `Generics.extQ` antiCompilationUnitExp `Generics.extQ` antiPackageExp `Generics.extQ` antiImportStatementExp `Generics.extQ` antiDocCommentExp `Generics.extQ` antiAnnotationExp `Generics.extQ` antiAnnotationArgumentsExp `Generics.extQ` antiAnnotationElementExp `Generics.extQ` antiRule_18Exp `Generics.extQ` antiExtendsListExp `Generics.extQ` antiImplementsListExp `Generics.extQ` antiFieldDeclarationExp `Generics.extQ` antiClassDeclarationExp `Generics.extQ` antiInterfaceDeclarationExp `Generics.extQ` antiAnnotationDeclarationExp `Generics.extQ` antiAnnotationTypeElementExp `Generics.extQ` antiEnumConstantExp `Generics.extQ` antiEnumConstantListExp `Generics.extQ` antiEnumDeclarationExp `Generics.extQ` antiNestedTypeDeclarationExp `Generics.extQ` antiRule_44Exp `Generics.extQ` antiMemberDeclarationExp `Generics.extQ` antiPrimitiveTypeKeywordExp `Generics.extQ` antiMemberAfterFirstIdExp `Generics.extQ` antiMoreTypeSpecifierExp `Generics.extQ` antiMemberRestExp `Generics.extQ` antiRule_49Exp `Generics.extQ` antiStatementBlockExp `Generics.extQ` antiVariableDeclaratorListExp `Generics.extQ` antiVariableDeclarationExp `Generics.extQ` antiOptVariableInitializerExp `Generics.extQ` antiVariableDeclaratorExp `Generics.extQ` antiVariableInitializerListExp `Generics.extQ` antiVariableInitializerExp `Generics.extQ` antiStaticInitializerExp `Generics.extQ` antiParameterListExp `Generics.extQ` antiParameterExp `Generics.extQ` antiStatementExp `Generics.extQ` antiOptExpressionExp `Generics.extQ` antiOptIdExp `Generics.extQ` antiStatementWithoutIfExp `Generics.extQ` antiOptElsePartExp `Generics.extQ` antiIfStatementExp `Generics.extQ` antiDoStatementExp `Generics.extQ` antiWhileStatementExp `Generics.extQ` antiForStatementExp `Generics.extQ` antiRule_65Exp `Generics.extQ` antiOptFinallyExp `Generics.extQ` antiTryStatementExp `Generics.extQ` antiRule_68Exp `Generics.extQ` antiSwitchStatementExp `Generics.extQ` antiExpressionExp `Generics.extQ` antiAssignmentOpExp `Generics.extQ` antiEqualityOpExp `Generics.extQ` antiRelationalOpExp `Generics.extQ` antiShiftOpExp `Generics.extQ` antiAdditiveOpExp `Generics.extQ` antiMultiplicativeOpExp `Generics.extQ` antiPrefixOpExp `Generics.extQ` antiPostfixOpExp `Generics.extQ` antiCreationExpressionExp `Generics.extQ` antiLiteralExp `Generics.extQ` antiArglistExp `Generics.extQ` antiTypeArgumentsExp `Generics.extQ` antiTypeArgumentExp `Generics.extQ` antiWildcardTypeExp `Generics.extQ` antiTypeParametersExp `Generics.extQ` antiTypeParameterExp `Generics.extQ` antiTypeExp `Generics.extQ` antiTypeSpecifierExp `Generics.extQ` antiModifierExp `Generics.extQ` antiCompoundNameExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiRule_68Exp _ = Nothing


antiTryStatementExp :: TryStatement -> Maybe (TH.Q TH.Exp )
antiTryStatementExp ( Anti_TryStatement v) = Just $ TH.varE (TH.mkName v)
antiTryStatementExp _ = Nothing


antiOptFinallyExp :: OptFinally -> Maybe (TH.Q TH.Exp )
antiOptFinallyExp ( Anti_OptFinally v) = Just $ TH.varE (TH.mkName v)
antiOptFinallyExp _ = Nothing


antiRule_65Exp :: [ Rule_65 ] -> Maybe (TH.Q TH.Exp)
antiRule_65Exp ((Anti_Rule_65 v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiJavaExp `Generics.extQ` antiOptDocCommentExp `Generics.extQ` antiTypeDeclarationExp `Generics.extQ` antiRule_2Exp `Generics.extQ` antiCompilationUnitExp `Generics.extQ` antiPackageExp `Generics.extQ` antiImportStatementExp `Generics.extQ` antiDocCommentExp `Generics.extQ` antiAnnotationExp `Generics.extQ` antiAnnotationArgumentsExp `Generics.extQ` antiAnnotationElementExp `Generics.extQ` antiRule_18Exp `Generics.extQ` antiExtendsListExp `Generics.extQ` antiImplementsListExp `Generics.extQ` antiFieldDeclarationExp `Generics.extQ` antiClassDeclarationExp `Generics.extQ` antiInterfaceDeclarationExp `Generics.extQ` antiAnnotationDeclarationExp `Generics.extQ` antiAnnotationTypeElementExp `Generics.extQ` antiEnumConstantExp `Generics.extQ` antiEnumConstantListExp `Generics.extQ` antiEnumDeclarationExp `Generics.extQ` antiNestedTypeDeclarationExp `Generics.extQ` antiRule_44Exp `Generics.extQ` antiMemberDeclarationExp `Generics.extQ` antiPrimitiveTypeKeywordExp `Generics.extQ` antiMemberAfterFirstIdExp `Generics.extQ` antiMoreTypeSpecifierExp `Generics.extQ` antiMemberRestExp `Generics.extQ` antiRule_49Exp `Generics.extQ` antiStatementBlockExp `Generics.extQ` antiVariableDeclaratorListExp `Generics.extQ` antiVariableDeclarationExp `Generics.extQ` antiOptVariableInitializerExp `Generics.extQ` antiVariableDeclaratorExp `Generics.extQ` antiVariableInitializerListExp `Generics.extQ` antiVariableInitializerExp `Generics.extQ` antiStaticInitializerExp `Generics.extQ` antiParameterListExp `Generics.extQ` antiParameterExp `Generics.extQ` antiStatementExp `Generics.extQ` antiOptExpressionExp `Generics.extQ` antiOptIdExp `Generics.extQ` antiStatementWithoutIfExp `Generics.extQ` antiOptElsePartExp `Generics.extQ` antiIfStatementExp `Generics.extQ` antiDoStatementExp `Generics.extQ` antiWhileStatementExp `Generics.extQ` antiForStatementExp `Generics.extQ` antiRule_65Exp `Generics.extQ` antiOptFinallyExp `Generics.extQ` antiTryStatementExp `Generics.extQ` antiRule_68Exp `Generics.extQ` antiSwitchStatementExp `Generics.extQ` antiExpressionExp `Generics.extQ` antiAssignmentOpExp `Generics.extQ` antiEqualityOpExp `Generics.extQ` antiRelationalOpExp `Generics.extQ` antiShiftOpExp `Generics.extQ` antiAdditiveOpExp `Generics.extQ` antiMultiplicativeOpExp `Generics.extQ` antiPrefixOpExp `Generics.extQ` antiPostfixOpExp `Generics.extQ` antiCreationExpressionExp `Generics.extQ` antiLiteralExp `Generics.extQ` antiArglistExp `Generics.extQ` antiTypeArgumentsExp `Generics.extQ` antiTypeArgumentExp `Generics.extQ` antiWildcardTypeExp `Generics.extQ` antiTypeParametersExp `Generics.extQ` antiTypeParameterExp `Generics.extQ` antiTypeExp `Generics.extQ` antiTypeSpecifierExp `Generics.extQ` antiModifierExp `Generics.extQ` antiCompoundNameExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiRule_65Exp _ = Nothing


antiForStatementExp :: ForStatement -> Maybe (TH.Q TH.Exp )
antiForStatementExp ( Anti_ForStatement v) = Just $ TH.varE (TH.mkName v)
antiForStatementExp _ = Nothing


antiWhileStatementExp :: WhileStatement -> Maybe (TH.Q TH.Exp )
antiWhileStatementExp ( Anti_WhileStatement v) = Just $ TH.varE (TH.mkName v)
antiWhileStatementExp _ = Nothing


antiDoStatementExp :: DoStatement -> Maybe (TH.Q TH.Exp )
antiDoStatementExp ( Anti_DoStatement v) = Just $ TH.varE (TH.mkName v)
antiDoStatementExp _ = Nothing


antiIfStatementExp :: IfStatement -> Maybe (TH.Q TH.Exp )
antiIfStatementExp ( Anti_IfStatement v) = Just $ TH.varE (TH.mkName v)
antiIfStatementExp _ = Nothing


antiOptElsePartExp :: OptElsePart -> Maybe (TH.Q TH.Exp )
antiOptElsePartExp ( Anti_OptElsePart v) = Just $ TH.varE (TH.mkName v)
antiOptElsePartExp _ = Nothing


antiStatementWithoutIfExp :: StatementWithoutIf -> Maybe (TH.Q TH.Exp )
antiStatementWithoutIfExp ( Anti_StatementWithoutIf v) = Just $ TH.varE (TH.mkName v)
antiStatementWithoutIfExp _ = Nothing


antiOptIdExp :: OptId -> Maybe (TH.Q TH.Exp )
antiOptIdExp ( Anti_OptId v) = Just $ TH.varE (TH.mkName v)
antiOptIdExp _ = Nothing


antiOptExpressionExp :: OptExpression -> Maybe (TH.Q TH.Exp )
antiOptExpressionExp ( Anti_OptExpression v) = Just $ TH.varE (TH.mkName v)
antiOptExpressionExp _ = Nothing


antiStatementExp :: [ Statement ] -> Maybe (TH.Q TH.Exp)
antiStatementExp ((Anti_Statement v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiJavaExp `Generics.extQ` antiOptDocCommentExp `Generics.extQ` antiTypeDeclarationExp `Generics.extQ` antiRule_2Exp `Generics.extQ` antiCompilationUnitExp `Generics.extQ` antiPackageExp `Generics.extQ` antiImportStatementExp `Generics.extQ` antiDocCommentExp `Generics.extQ` antiAnnotationExp `Generics.extQ` antiAnnotationArgumentsExp `Generics.extQ` antiAnnotationElementExp `Generics.extQ` antiRule_18Exp `Generics.extQ` antiExtendsListExp `Generics.extQ` antiImplementsListExp `Generics.extQ` antiFieldDeclarationExp `Generics.extQ` antiClassDeclarationExp `Generics.extQ` antiInterfaceDeclarationExp `Generics.extQ` antiAnnotationDeclarationExp `Generics.extQ` antiAnnotationTypeElementExp `Generics.extQ` antiEnumConstantExp `Generics.extQ` antiEnumConstantListExp `Generics.extQ` antiEnumDeclarationExp `Generics.extQ` antiNestedTypeDeclarationExp `Generics.extQ` antiRule_44Exp `Generics.extQ` antiMemberDeclarationExp `Generics.extQ` antiPrimitiveTypeKeywordExp `Generics.extQ` antiMemberAfterFirstIdExp `Generics.extQ` antiMoreTypeSpecifierExp `Generics.extQ` antiMemberRestExp `Generics.extQ` antiRule_49Exp `Generics.extQ` antiStatementBlockExp `Generics.extQ` antiVariableDeclaratorListExp `Generics.extQ` antiVariableDeclarationExp `Generics.extQ` antiOptVariableInitializerExp `Generics.extQ` antiVariableDeclaratorExp `Generics.extQ` antiVariableInitializerListExp `Generics.extQ` antiVariableInitializerExp `Generics.extQ` antiStaticInitializerExp `Generics.extQ` antiParameterListExp `Generics.extQ` antiParameterExp `Generics.extQ` antiStatementExp `Generics.extQ` antiOptExpressionExp `Generics.extQ` antiOptIdExp `Generics.extQ` antiStatementWithoutIfExp `Generics.extQ` antiOptElsePartExp `Generics.extQ` antiIfStatementExp `Generics.extQ` antiDoStatementExp `Generics.extQ` antiWhileStatementExp `Generics.extQ` antiForStatementExp `Generics.extQ` antiRule_65Exp `Generics.extQ` antiOptFinallyExp `Generics.extQ` antiTryStatementExp `Generics.extQ` antiRule_68Exp `Generics.extQ` antiSwitchStatementExp `Generics.extQ` antiExpressionExp `Generics.extQ` antiAssignmentOpExp `Generics.extQ` antiEqualityOpExp `Generics.extQ` antiRelationalOpExp `Generics.extQ` antiShiftOpExp `Generics.extQ` antiAdditiveOpExp `Generics.extQ` antiMultiplicativeOpExp `Generics.extQ` antiPrefixOpExp `Generics.extQ` antiPostfixOpExp `Generics.extQ` antiCreationExpressionExp `Generics.extQ` antiLiteralExp `Generics.extQ` antiArglistExp `Generics.extQ` antiTypeArgumentsExp `Generics.extQ` antiTypeArgumentExp `Generics.extQ` antiWildcardTypeExp `Generics.extQ` antiTypeParametersExp `Generics.extQ` antiTypeParameterExp `Generics.extQ` antiTypeExp `Generics.extQ` antiTypeSpecifierExp `Generics.extQ` antiModifierExp `Generics.extQ` antiCompoundNameExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiStatementExp _ = Nothing


antiParameterExp :: Parameter -> Maybe (TH.Q TH.Exp )
antiParameterExp ( Anti_Parameter v) = Just $ TH.varE (TH.mkName v)
antiParameterExp _ = Nothing


antiParameterListExp :: ParameterList -> Maybe (TH.Q TH.Exp )
antiParameterListExp ( Anti_ParameterList v) = Just $ TH.varE (TH.mkName v)
antiParameterListExp _ = Nothing


antiStaticInitializerExp :: StaticInitializer -> Maybe (TH.Q TH.Exp )
antiStaticInitializerExp ( Anti_StaticInitializer v) = Just $ TH.varE (TH.mkName v)
antiStaticInitializerExp _ = Nothing


antiVariableInitializerExp :: VariableInitializer -> Maybe (TH.Q TH.Exp )
antiVariableInitializerExp ( Anti_VariableInitializer v) = Just $ TH.varE (TH.mkName v)
antiVariableInitializerExp _ = Nothing


antiVariableInitializerListExp :: VariableInitializerList -> Maybe (TH.Q TH.Exp )
antiVariableInitializerListExp ( Anti_VariableInitializerList v) = Just $ TH.varE (TH.mkName v)
antiVariableInitializerListExp _ = Nothing


antiVariableDeclaratorExp :: VariableDeclarator -> Maybe (TH.Q TH.Exp )
antiVariableDeclaratorExp ( Anti_VariableDeclarator v) = Just $ TH.varE (TH.mkName v)
antiVariableDeclaratorExp _ = Nothing


antiOptVariableInitializerExp :: OptVariableInitializer -> Maybe (TH.Q TH.Exp )
antiOptVariableInitializerExp ( Anti_OptVariableInitializer v) = Just $ TH.varE (TH.mkName v)
antiOptVariableInitializerExp _ = Nothing


antiVariableDeclarationExp :: VariableDeclaration -> Maybe (TH.Q TH.Exp )
antiVariableDeclarationExp ( Anti_VariableDeclaration v) = Just $ TH.varE (TH.mkName v)
antiVariableDeclarationExp _ = Nothing


antiVariableDeclaratorListExp :: VariableDeclaratorList -> Maybe (TH.Q TH.Exp )
antiVariableDeclaratorListExp ( Anti_VariableDeclaratorList v) = Just $ TH.varE (TH.mkName v)
antiVariableDeclaratorListExp _ = Nothing


antiStatementBlockExp :: StatementBlock -> Maybe (TH.Q TH.Exp )
antiStatementBlockExp ( Anti_StatementBlock v) = Just $ TH.varE (TH.mkName v)
antiStatementBlockExp _ = Nothing


antiRule_49Exp :: [ Rule_49 ] -> Maybe (TH.Q TH.Exp)
antiRule_49Exp ((Anti_Rule_49 v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiJavaExp `Generics.extQ` antiOptDocCommentExp `Generics.extQ` antiTypeDeclarationExp `Generics.extQ` antiRule_2Exp `Generics.extQ` antiCompilationUnitExp `Generics.extQ` antiPackageExp `Generics.extQ` antiImportStatementExp `Generics.extQ` antiDocCommentExp `Generics.extQ` antiAnnotationExp `Generics.extQ` antiAnnotationArgumentsExp `Generics.extQ` antiAnnotationElementExp `Generics.extQ` antiRule_18Exp `Generics.extQ` antiExtendsListExp `Generics.extQ` antiImplementsListExp `Generics.extQ` antiFieldDeclarationExp `Generics.extQ` antiClassDeclarationExp `Generics.extQ` antiInterfaceDeclarationExp `Generics.extQ` antiAnnotationDeclarationExp `Generics.extQ` antiAnnotationTypeElementExp `Generics.extQ` antiEnumConstantExp `Generics.extQ` antiEnumConstantListExp `Generics.extQ` antiEnumDeclarationExp `Generics.extQ` antiNestedTypeDeclarationExp `Generics.extQ` antiRule_44Exp `Generics.extQ` antiMemberDeclarationExp `Generics.extQ` antiPrimitiveTypeKeywordExp `Generics.extQ` antiMemberAfterFirstIdExp `Generics.extQ` antiMoreTypeSpecifierExp `Generics.extQ` antiMemberRestExp `Generics.extQ` antiRule_49Exp `Generics.extQ` antiStatementBlockExp `Generics.extQ` antiVariableDeclaratorListExp `Generics.extQ` antiVariableDeclarationExp `Generics.extQ` antiOptVariableInitializerExp `Generics.extQ` antiVariableDeclaratorExp `Generics.extQ` antiVariableInitializerListExp `Generics.extQ` antiVariableInitializerExp `Generics.extQ` antiStaticInitializerExp `Generics.extQ` antiParameterListExp `Generics.extQ` antiParameterExp `Generics.extQ` antiStatementExp `Generics.extQ` antiOptExpressionExp `Generics.extQ` antiOptIdExp `Generics.extQ` antiStatementWithoutIfExp `Generics.extQ` antiOptElsePartExp `Generics.extQ` antiIfStatementExp `Generics.extQ` antiDoStatementExp `Generics.extQ` antiWhileStatementExp `Generics.extQ` antiForStatementExp `Generics.extQ` antiRule_65Exp `Generics.extQ` antiOptFinallyExp `Generics.extQ` antiTryStatementExp `Generics.extQ` antiRule_68Exp `Generics.extQ` antiSwitchStatementExp `Generics.extQ` antiExpressionExp `Generics.extQ` antiAssignmentOpExp `Generics.extQ` antiEqualityOpExp `Generics.extQ` antiRelationalOpExp `Generics.extQ` antiShiftOpExp `Generics.extQ` antiAdditiveOpExp `Generics.extQ` antiMultiplicativeOpExp `Generics.extQ` antiPrefixOpExp `Generics.extQ` antiPostfixOpExp `Generics.extQ` antiCreationExpressionExp `Generics.extQ` antiLiteralExp `Generics.extQ` antiArglistExp `Generics.extQ` antiTypeArgumentsExp `Generics.extQ` antiTypeArgumentExp `Generics.extQ` antiWildcardTypeExp `Generics.extQ` antiTypeParametersExp `Generics.extQ` antiTypeParameterExp `Generics.extQ` antiTypeExp `Generics.extQ` antiTypeSpecifierExp `Generics.extQ` antiModifierExp `Generics.extQ` antiCompoundNameExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiRule_49Exp _ = Nothing


antiMemberRestExp :: MemberRest -> Maybe (TH.Q TH.Exp )
antiMemberRestExp ( Anti_MemberRest v) = Just $ TH.varE (TH.mkName v)
antiMemberRestExp _ = Nothing


antiMoreTypeSpecifierExp :: MoreTypeSpecifier -> Maybe (TH.Q TH.Exp )
antiMoreTypeSpecifierExp ( Anti_MoreTypeSpecifier v) = Just $ TH.varE (TH.mkName v)
antiMoreTypeSpecifierExp _ = Nothing


antiMemberAfterFirstIdExp :: MemberAfterFirstId -> Maybe (TH.Q TH.Exp )
antiMemberAfterFirstIdExp ( Anti_MemberAfterFirstId v) = Just $ TH.varE (TH.mkName v)
antiMemberAfterFirstIdExp _ = Nothing


antiPrimitiveTypeKeywordExp :: PrimitiveTypeKeyword -> Maybe (TH.Q TH.Exp )
antiPrimitiveTypeKeywordExp ( Anti_PrimitiveTypeKeyword v) = Just $ TH.varE (TH.mkName v)
antiPrimitiveTypeKeywordExp _ = Nothing


antiMemberDeclarationExp :: MemberDeclaration -> Maybe (TH.Q TH.Exp )
antiMemberDeclarationExp ( Anti_MemberDeclaration v) = Just $ TH.varE (TH.mkName v)
antiMemberDeclarationExp _ = Nothing


antiRule_44Exp :: [ Rule_44 ] -> Maybe (TH.Q TH.Exp)
antiRule_44Exp ((Anti_Rule_44 v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiJavaExp `Generics.extQ` antiOptDocCommentExp `Generics.extQ` antiTypeDeclarationExp `Generics.extQ` antiRule_2Exp `Generics.extQ` antiCompilationUnitExp `Generics.extQ` antiPackageExp `Generics.extQ` antiImportStatementExp `Generics.extQ` antiDocCommentExp `Generics.extQ` antiAnnotationExp `Generics.extQ` antiAnnotationArgumentsExp `Generics.extQ` antiAnnotationElementExp `Generics.extQ` antiRule_18Exp `Generics.extQ` antiExtendsListExp `Generics.extQ` antiImplementsListExp `Generics.extQ` antiFieldDeclarationExp `Generics.extQ` antiClassDeclarationExp `Generics.extQ` antiInterfaceDeclarationExp `Generics.extQ` antiAnnotationDeclarationExp `Generics.extQ` antiAnnotationTypeElementExp `Generics.extQ` antiEnumConstantExp `Generics.extQ` antiEnumConstantListExp `Generics.extQ` antiEnumDeclarationExp `Generics.extQ` antiNestedTypeDeclarationExp `Generics.extQ` antiRule_44Exp `Generics.extQ` antiMemberDeclarationExp `Generics.extQ` antiPrimitiveTypeKeywordExp `Generics.extQ` antiMemberAfterFirstIdExp `Generics.extQ` antiMoreTypeSpecifierExp `Generics.extQ` antiMemberRestExp `Generics.extQ` antiRule_49Exp `Generics.extQ` antiStatementBlockExp `Generics.extQ` antiVariableDeclaratorListExp `Generics.extQ` antiVariableDeclarationExp `Generics.extQ` antiOptVariableInitializerExp `Generics.extQ` antiVariableDeclaratorExp `Generics.extQ` antiVariableInitializerListExp `Generics.extQ` antiVariableInitializerExp `Generics.extQ` antiStaticInitializerExp `Generics.extQ` antiParameterListExp `Generics.extQ` antiParameterExp `Generics.extQ` antiStatementExp `Generics.extQ` antiOptExpressionExp `Generics.extQ` antiOptIdExp `Generics.extQ` antiStatementWithoutIfExp `Generics.extQ` antiOptElsePartExp `Generics.extQ` antiIfStatementExp `Generics.extQ` antiDoStatementExp `Generics.extQ` antiWhileStatementExp `Generics.extQ` antiForStatementExp `Generics.extQ` antiRule_65Exp `Generics.extQ` antiOptFinallyExp `Generics.extQ` antiTryStatementExp `Generics.extQ` antiRule_68Exp `Generics.extQ` antiSwitchStatementExp `Generics.extQ` antiExpressionExp `Generics.extQ` antiAssignmentOpExp `Generics.extQ` antiEqualityOpExp `Generics.extQ` antiRelationalOpExp `Generics.extQ` antiShiftOpExp `Generics.extQ` antiAdditiveOpExp `Generics.extQ` antiMultiplicativeOpExp `Generics.extQ` antiPrefixOpExp `Generics.extQ` antiPostfixOpExp `Generics.extQ` antiCreationExpressionExp `Generics.extQ` antiLiteralExp `Generics.extQ` antiArglistExp `Generics.extQ` antiTypeArgumentsExp `Generics.extQ` antiTypeArgumentExp `Generics.extQ` antiWildcardTypeExp `Generics.extQ` antiTypeParametersExp `Generics.extQ` antiTypeParameterExp `Generics.extQ` antiTypeExp `Generics.extQ` antiTypeSpecifierExp `Generics.extQ` antiModifierExp `Generics.extQ` antiCompoundNameExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiRule_44Exp _ = Nothing


antiNestedTypeDeclarationExp :: NestedTypeDeclaration -> Maybe (TH.Q TH.Exp )
antiNestedTypeDeclarationExp ( Anti_NestedTypeDeclaration v) = Just $ TH.varE (TH.mkName v)
antiNestedTypeDeclarationExp _ = Nothing


antiEnumDeclarationExp :: EnumDeclaration -> Maybe (TH.Q TH.Exp )
antiEnumDeclarationExp ( Anti_EnumDeclaration v) = Just $ TH.varE (TH.mkName v)
antiEnumDeclarationExp _ = Nothing


antiEnumConstantListExp :: EnumConstantList -> Maybe (TH.Q TH.Exp )
antiEnumConstantListExp ( Anti_EnumConstantList v) = Just $ TH.varE (TH.mkName v)
antiEnumConstantListExp _ = Nothing


antiEnumConstantExp :: EnumConstant -> Maybe (TH.Q TH.Exp )
antiEnumConstantExp ( Anti_EnumConstant v) = Just $ TH.varE (TH.mkName v)
antiEnumConstantExp _ = Nothing


antiAnnotationTypeElementExp :: [ AnnotationTypeElement ] -> Maybe (TH.Q TH.Exp)
antiAnnotationTypeElementExp ((Anti_AnnotationTypeElement v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiJavaExp `Generics.extQ` antiOptDocCommentExp `Generics.extQ` antiTypeDeclarationExp `Generics.extQ` antiRule_2Exp `Generics.extQ` antiCompilationUnitExp `Generics.extQ` antiPackageExp `Generics.extQ` antiImportStatementExp `Generics.extQ` antiDocCommentExp `Generics.extQ` antiAnnotationExp `Generics.extQ` antiAnnotationArgumentsExp `Generics.extQ` antiAnnotationElementExp `Generics.extQ` antiRule_18Exp `Generics.extQ` antiExtendsListExp `Generics.extQ` antiImplementsListExp `Generics.extQ` antiFieldDeclarationExp `Generics.extQ` antiClassDeclarationExp `Generics.extQ` antiInterfaceDeclarationExp `Generics.extQ` antiAnnotationDeclarationExp `Generics.extQ` antiAnnotationTypeElementExp `Generics.extQ` antiEnumConstantExp `Generics.extQ` antiEnumConstantListExp `Generics.extQ` antiEnumDeclarationExp `Generics.extQ` antiNestedTypeDeclarationExp `Generics.extQ` antiRule_44Exp `Generics.extQ` antiMemberDeclarationExp `Generics.extQ` antiPrimitiveTypeKeywordExp `Generics.extQ` antiMemberAfterFirstIdExp `Generics.extQ` antiMoreTypeSpecifierExp `Generics.extQ` antiMemberRestExp `Generics.extQ` antiRule_49Exp `Generics.extQ` antiStatementBlockExp `Generics.extQ` antiVariableDeclaratorListExp `Generics.extQ` antiVariableDeclarationExp `Generics.extQ` antiOptVariableInitializerExp `Generics.extQ` antiVariableDeclaratorExp `Generics.extQ` antiVariableInitializerListExp `Generics.extQ` antiVariableInitializerExp `Generics.extQ` antiStaticInitializerExp `Generics.extQ` antiParameterListExp `Generics.extQ` antiParameterExp `Generics.extQ` antiStatementExp `Generics.extQ` antiOptExpressionExp `Generics.extQ` antiOptIdExp `Generics.extQ` antiStatementWithoutIfExp `Generics.extQ` antiOptElsePartExp `Generics.extQ` antiIfStatementExp `Generics.extQ` antiDoStatementExp `Generics.extQ` antiWhileStatementExp `Generics.extQ` antiForStatementExp `Generics.extQ` antiRule_65Exp `Generics.extQ` antiOptFinallyExp `Generics.extQ` antiTryStatementExp `Generics.extQ` antiRule_68Exp `Generics.extQ` antiSwitchStatementExp `Generics.extQ` antiExpressionExp `Generics.extQ` antiAssignmentOpExp `Generics.extQ` antiEqualityOpExp `Generics.extQ` antiRelationalOpExp `Generics.extQ` antiShiftOpExp `Generics.extQ` antiAdditiveOpExp `Generics.extQ` antiMultiplicativeOpExp `Generics.extQ` antiPrefixOpExp `Generics.extQ` antiPostfixOpExp `Generics.extQ` antiCreationExpressionExp `Generics.extQ` antiLiteralExp `Generics.extQ` antiArglistExp `Generics.extQ` antiTypeArgumentsExp `Generics.extQ` antiTypeArgumentExp `Generics.extQ` antiWildcardTypeExp `Generics.extQ` antiTypeParametersExp `Generics.extQ` antiTypeParameterExp `Generics.extQ` antiTypeExp `Generics.extQ` antiTypeSpecifierExp `Generics.extQ` antiModifierExp `Generics.extQ` antiCompoundNameExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiAnnotationTypeElementExp _ = Nothing


antiAnnotationDeclarationExp :: AnnotationDeclaration -> Maybe (TH.Q TH.Exp )
antiAnnotationDeclarationExp ( Anti_AnnotationDeclaration v) = Just $ TH.varE (TH.mkName v)
antiAnnotationDeclarationExp _ = Nothing


antiInterfaceDeclarationExp :: InterfaceDeclaration -> Maybe (TH.Q TH.Exp )
antiInterfaceDeclarationExp ( Anti_InterfaceDeclaration v) = Just $ TH.varE (TH.mkName v)
antiInterfaceDeclarationExp _ = Nothing


antiClassDeclarationExp :: ClassDeclaration -> Maybe (TH.Q TH.Exp )
antiClassDeclarationExp ( Anti_ClassDeclaration v) = Just $ TH.varE (TH.mkName v)
antiClassDeclarationExp _ = Nothing


antiFieldDeclarationExp :: [ FieldDeclaration ] -> Maybe (TH.Q TH.Exp)
antiFieldDeclarationExp ((Anti_FieldDeclaration v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiJavaExp `Generics.extQ` antiOptDocCommentExp `Generics.extQ` antiTypeDeclarationExp `Generics.extQ` antiRule_2Exp `Generics.extQ` antiCompilationUnitExp `Generics.extQ` antiPackageExp `Generics.extQ` antiImportStatementExp `Generics.extQ` antiDocCommentExp `Generics.extQ` antiAnnotationExp `Generics.extQ` antiAnnotationArgumentsExp `Generics.extQ` antiAnnotationElementExp `Generics.extQ` antiRule_18Exp `Generics.extQ` antiExtendsListExp `Generics.extQ` antiImplementsListExp `Generics.extQ` antiFieldDeclarationExp `Generics.extQ` antiClassDeclarationExp `Generics.extQ` antiInterfaceDeclarationExp `Generics.extQ` antiAnnotationDeclarationExp `Generics.extQ` antiAnnotationTypeElementExp `Generics.extQ` antiEnumConstantExp `Generics.extQ` antiEnumConstantListExp `Generics.extQ` antiEnumDeclarationExp `Generics.extQ` antiNestedTypeDeclarationExp `Generics.extQ` antiRule_44Exp `Generics.extQ` antiMemberDeclarationExp `Generics.extQ` antiPrimitiveTypeKeywordExp `Generics.extQ` antiMemberAfterFirstIdExp `Generics.extQ` antiMoreTypeSpecifierExp `Generics.extQ` antiMemberRestExp `Generics.extQ` antiRule_49Exp `Generics.extQ` antiStatementBlockExp `Generics.extQ` antiVariableDeclaratorListExp `Generics.extQ` antiVariableDeclarationExp `Generics.extQ` antiOptVariableInitializerExp `Generics.extQ` antiVariableDeclaratorExp `Generics.extQ` antiVariableInitializerListExp `Generics.extQ` antiVariableInitializerExp `Generics.extQ` antiStaticInitializerExp `Generics.extQ` antiParameterListExp `Generics.extQ` antiParameterExp `Generics.extQ` antiStatementExp `Generics.extQ` antiOptExpressionExp `Generics.extQ` antiOptIdExp `Generics.extQ` antiStatementWithoutIfExp `Generics.extQ` antiOptElsePartExp `Generics.extQ` antiIfStatementExp `Generics.extQ` antiDoStatementExp `Generics.extQ` antiWhileStatementExp `Generics.extQ` antiForStatementExp `Generics.extQ` antiRule_65Exp `Generics.extQ` antiOptFinallyExp `Generics.extQ` antiTryStatementExp `Generics.extQ` antiRule_68Exp `Generics.extQ` antiSwitchStatementExp `Generics.extQ` antiExpressionExp `Generics.extQ` antiAssignmentOpExp `Generics.extQ` antiEqualityOpExp `Generics.extQ` antiRelationalOpExp `Generics.extQ` antiShiftOpExp `Generics.extQ` antiAdditiveOpExp `Generics.extQ` antiMultiplicativeOpExp `Generics.extQ` antiPrefixOpExp `Generics.extQ` antiPostfixOpExp `Generics.extQ` antiCreationExpressionExp `Generics.extQ` antiLiteralExp `Generics.extQ` antiArglistExp `Generics.extQ` antiTypeArgumentsExp `Generics.extQ` antiTypeArgumentExp `Generics.extQ` antiWildcardTypeExp `Generics.extQ` antiTypeParametersExp `Generics.extQ` antiTypeParameterExp `Generics.extQ` antiTypeExp `Generics.extQ` antiTypeSpecifierExp `Generics.extQ` antiModifierExp `Generics.extQ` antiCompoundNameExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiFieldDeclarationExp _ = Nothing


antiImplementsListExp :: ImplementsList -> Maybe (TH.Q TH.Exp )
antiImplementsListExp ( Anti_ImplementsList v) = Just $ TH.varE (TH.mkName v)
antiImplementsListExp _ = Nothing


antiExtendsListExp :: ExtendsList -> Maybe (TH.Q TH.Exp )
antiExtendsListExp ( Anti_ExtendsList v) = Just $ TH.varE (TH.mkName v)
antiExtendsListExp _ = Nothing


antiRule_18Exp :: [ Rule_18 ] -> Maybe (TH.Q TH.Exp)
antiRule_18Exp ((Anti_Rule_18 v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiJavaExp `Generics.extQ` antiOptDocCommentExp `Generics.extQ` antiTypeDeclarationExp `Generics.extQ` antiRule_2Exp `Generics.extQ` antiCompilationUnitExp `Generics.extQ` antiPackageExp `Generics.extQ` antiImportStatementExp `Generics.extQ` antiDocCommentExp `Generics.extQ` antiAnnotationExp `Generics.extQ` antiAnnotationArgumentsExp `Generics.extQ` antiAnnotationElementExp `Generics.extQ` antiRule_18Exp `Generics.extQ` antiExtendsListExp `Generics.extQ` antiImplementsListExp `Generics.extQ` antiFieldDeclarationExp `Generics.extQ` antiClassDeclarationExp `Generics.extQ` antiInterfaceDeclarationExp `Generics.extQ` antiAnnotationDeclarationExp `Generics.extQ` antiAnnotationTypeElementExp `Generics.extQ` antiEnumConstantExp `Generics.extQ` antiEnumConstantListExp `Generics.extQ` antiEnumDeclarationExp `Generics.extQ` antiNestedTypeDeclarationExp `Generics.extQ` antiRule_44Exp `Generics.extQ` antiMemberDeclarationExp `Generics.extQ` antiPrimitiveTypeKeywordExp `Generics.extQ` antiMemberAfterFirstIdExp `Generics.extQ` antiMoreTypeSpecifierExp `Generics.extQ` antiMemberRestExp `Generics.extQ` antiRule_49Exp `Generics.extQ` antiStatementBlockExp `Generics.extQ` antiVariableDeclaratorListExp `Generics.extQ` antiVariableDeclarationExp `Generics.extQ` antiOptVariableInitializerExp `Generics.extQ` antiVariableDeclaratorExp `Generics.extQ` antiVariableInitializerListExp `Generics.extQ` antiVariableInitializerExp `Generics.extQ` antiStaticInitializerExp `Generics.extQ` antiParameterListExp `Generics.extQ` antiParameterExp `Generics.extQ` antiStatementExp `Generics.extQ` antiOptExpressionExp `Generics.extQ` antiOptIdExp `Generics.extQ` antiStatementWithoutIfExp `Generics.extQ` antiOptElsePartExp `Generics.extQ` antiIfStatementExp `Generics.extQ` antiDoStatementExp `Generics.extQ` antiWhileStatementExp `Generics.extQ` antiForStatementExp `Generics.extQ` antiRule_65Exp `Generics.extQ` antiOptFinallyExp `Generics.extQ` antiTryStatementExp `Generics.extQ` antiRule_68Exp `Generics.extQ` antiSwitchStatementExp `Generics.extQ` antiExpressionExp `Generics.extQ` antiAssignmentOpExp `Generics.extQ` antiEqualityOpExp `Generics.extQ` antiRelationalOpExp `Generics.extQ` antiShiftOpExp `Generics.extQ` antiAdditiveOpExp `Generics.extQ` antiMultiplicativeOpExp `Generics.extQ` antiPrefixOpExp `Generics.extQ` antiPostfixOpExp `Generics.extQ` antiCreationExpressionExp `Generics.extQ` antiLiteralExp `Generics.extQ` antiArglistExp `Generics.extQ` antiTypeArgumentsExp `Generics.extQ` antiTypeArgumentExp `Generics.extQ` antiWildcardTypeExp `Generics.extQ` antiTypeParametersExp `Generics.extQ` antiTypeParameterExp `Generics.extQ` antiTypeExp `Generics.extQ` antiTypeSpecifierExp `Generics.extQ` antiModifierExp `Generics.extQ` antiCompoundNameExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiRule_18Exp _ = Nothing


antiAnnotationElementExp :: AnnotationElement -> Maybe (TH.Q TH.Exp )
antiAnnotationElementExp ( Anti_AnnotationElement v) = Just $ TH.varE (TH.mkName v)
antiAnnotationElementExp _ = Nothing


antiAnnotationArgumentsExp :: AnnotationArguments -> Maybe (TH.Q TH.Exp )
antiAnnotationArgumentsExp ( Anti_AnnotationArguments v) = Just $ TH.varE (TH.mkName v)
antiAnnotationArgumentsExp _ = Nothing


antiAnnotationExp :: Annotation -> Maybe (TH.Q TH.Exp )
antiAnnotationExp ( Anti_Annotation v) = Just $ TH.varE (TH.mkName v)
antiAnnotationExp _ = Nothing


antiDocCommentExp :: DocComment -> Maybe (TH.Q TH.Exp )
antiDocCommentExp ( Anti_DocComment v) = Just $ TH.varE (TH.mkName v)
antiDocCommentExp _ = Nothing


antiImportStatementExp :: ImportStatement -> Maybe (TH.Q TH.Exp )
antiImportStatementExp ( Anti_ImportStatement v) = Just $ TH.varE (TH.mkName v)
antiImportStatementExp _ = Nothing


antiPackageExp :: Package -> Maybe (TH.Q TH.Exp )
antiPackageExp ( Anti_Package v) = Just $ TH.varE (TH.mkName v)
antiPackageExp _ = Nothing


antiCompilationUnitExp :: CompilationUnit -> Maybe (TH.Q TH.Exp )
antiCompilationUnitExp ( Anti_CompilationUnit v) = Just $ TH.varE (TH.mkName v)
antiCompilationUnitExp _ = Nothing


antiRule_2Exp :: [ Rule_2 ] -> Maybe (TH.Q TH.Exp)
antiRule_2Exp ((Anti_Rule_2 v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiJavaExp `Generics.extQ` antiOptDocCommentExp `Generics.extQ` antiTypeDeclarationExp `Generics.extQ` antiRule_2Exp `Generics.extQ` antiCompilationUnitExp `Generics.extQ` antiPackageExp `Generics.extQ` antiImportStatementExp `Generics.extQ` antiDocCommentExp `Generics.extQ` antiAnnotationExp `Generics.extQ` antiAnnotationArgumentsExp `Generics.extQ` antiAnnotationElementExp `Generics.extQ` antiRule_18Exp `Generics.extQ` antiExtendsListExp `Generics.extQ` antiImplementsListExp `Generics.extQ` antiFieldDeclarationExp `Generics.extQ` antiClassDeclarationExp `Generics.extQ` antiInterfaceDeclarationExp `Generics.extQ` antiAnnotationDeclarationExp `Generics.extQ` antiAnnotationTypeElementExp `Generics.extQ` antiEnumConstantExp `Generics.extQ` antiEnumConstantListExp `Generics.extQ` antiEnumDeclarationExp `Generics.extQ` antiNestedTypeDeclarationExp `Generics.extQ` antiRule_44Exp `Generics.extQ` antiMemberDeclarationExp `Generics.extQ` antiPrimitiveTypeKeywordExp `Generics.extQ` antiMemberAfterFirstIdExp `Generics.extQ` antiMoreTypeSpecifierExp `Generics.extQ` antiMemberRestExp `Generics.extQ` antiRule_49Exp `Generics.extQ` antiStatementBlockExp `Generics.extQ` antiVariableDeclaratorListExp `Generics.extQ` antiVariableDeclarationExp `Generics.extQ` antiOptVariableInitializerExp `Generics.extQ` antiVariableDeclaratorExp `Generics.extQ` antiVariableInitializerListExp `Generics.extQ` antiVariableInitializerExp `Generics.extQ` antiStaticInitializerExp `Generics.extQ` antiParameterListExp `Generics.extQ` antiParameterExp `Generics.extQ` antiStatementExp `Generics.extQ` antiOptExpressionExp `Generics.extQ` antiOptIdExp `Generics.extQ` antiStatementWithoutIfExp `Generics.extQ` antiOptElsePartExp `Generics.extQ` antiIfStatementExp `Generics.extQ` antiDoStatementExp `Generics.extQ` antiWhileStatementExp `Generics.extQ` antiForStatementExp `Generics.extQ` antiRule_65Exp `Generics.extQ` antiOptFinallyExp `Generics.extQ` antiTryStatementExp `Generics.extQ` antiRule_68Exp `Generics.extQ` antiSwitchStatementExp `Generics.extQ` antiExpressionExp `Generics.extQ` antiAssignmentOpExp `Generics.extQ` antiEqualityOpExp `Generics.extQ` antiRelationalOpExp `Generics.extQ` antiShiftOpExp `Generics.extQ` antiAdditiveOpExp `Generics.extQ` antiMultiplicativeOpExp `Generics.extQ` antiPrefixOpExp `Generics.extQ` antiPostfixOpExp `Generics.extQ` antiCreationExpressionExp `Generics.extQ` antiLiteralExp `Generics.extQ` antiArglistExp `Generics.extQ` antiTypeArgumentsExp `Generics.extQ` antiTypeArgumentExp `Generics.extQ` antiWildcardTypeExp `Generics.extQ` antiTypeParametersExp `Generics.extQ` antiTypeParameterExp `Generics.extQ` antiTypeExp `Generics.extQ` antiTypeSpecifierExp `Generics.extQ` antiModifierExp `Generics.extQ` antiCompoundNameExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiRule_2Exp _ = Nothing


antiTypeDeclarationExp :: TypeDeclaration -> Maybe (TH.Q TH.Exp )
antiTypeDeclarationExp ( Anti_TypeDeclaration v) = Just $ TH.varE (TH.mkName v)
antiTypeDeclarationExp _ = Nothing


antiOptDocCommentExp :: OptDocComment -> Maybe (TH.Q TH.Exp )
antiOptDocCommentExp ( Anti_OptDocComment v) = Just $ TH.varE (TH.mkName v)
antiOptDocCommentExp _ = Nothing


antiJavaExp :: Java -> Maybe (TH.Q TH.Exp )
antiJavaExp ( Anti_Java v) = Just $ TH.varE (TH.mkName v)
antiJavaExp _ = Nothing



antiCompoundNamePat :: CompoundName -> Maybe (TH.Q TH.Pat )
antiCompoundNamePat ( Anti_CompoundName v) = Just $ TH.varP (TH.mkName v)
antiCompoundNamePat _ = Nothing


antiModifierPat :: Modifier -> Maybe (TH.Q TH.Pat )
antiModifierPat ( Anti_Modifier v) = Just $ TH.varP (TH.mkName v)
antiModifierPat _ = Nothing


antiTypeSpecifierPat :: TypeSpecifier -> Maybe (TH.Q TH.Pat )
antiTypeSpecifierPat ( Anti_TypeSpecifier v) = Just $ TH.varP (TH.mkName v)
antiTypeSpecifierPat _ = Nothing


antiTypePat :: Type -> Maybe (TH.Q TH.Pat )
antiTypePat ( Anti_Type v) = Just $ TH.varP (TH.mkName v)
antiTypePat _ = Nothing


antiTypeParameterPat :: TypeParameter -> Maybe (TH.Q TH.Pat )
antiTypeParameterPat ( Anti_TypeParameter v) = Just $ TH.varP (TH.mkName v)
antiTypeParameterPat _ = Nothing


antiTypeParametersPat :: TypeParameters -> Maybe (TH.Q TH.Pat )
antiTypeParametersPat ( Anti_TypeParameters v) = Just $ TH.varP (TH.mkName v)
antiTypeParametersPat _ = Nothing


antiWildcardTypePat :: WildcardType -> Maybe (TH.Q TH.Pat )
antiWildcardTypePat ( Anti_WildcardType v) = Just $ TH.varP (TH.mkName v)
antiWildcardTypePat _ = Nothing


antiTypeArgumentPat :: TypeArgument -> Maybe (TH.Q TH.Pat )
antiTypeArgumentPat ( Anti_TypeArgument v) = Just $ TH.varP (TH.mkName v)
antiTypeArgumentPat _ = Nothing


antiTypeArgumentsPat :: TypeArguments -> Maybe (TH.Q TH.Pat )
antiTypeArgumentsPat ( Anti_TypeArguments v) = Just $ TH.varP (TH.mkName v)
antiTypeArgumentsPat _ = Nothing


antiArglistPat :: Arglist -> Maybe (TH.Q TH.Pat )
antiArglistPat ( Anti_Arglist v) = Just $ TH.varP (TH.mkName v)
antiArglistPat _ = Nothing


antiLiteralPat :: Literal -> Maybe (TH.Q TH.Pat )
antiLiteralPat ( Anti_Literal v) = Just $ TH.varP (TH.mkName v)
antiLiteralPat _ = Nothing


antiCreationExpressionPat :: CreationExpression -> Maybe (TH.Q TH.Pat )
antiCreationExpressionPat ( Anti_CreationExpression v) = Just $ TH.varP (TH.mkName v)
antiCreationExpressionPat _ = Nothing


antiPostfixOpPat :: PostfixOp -> Maybe (TH.Q TH.Pat )
antiPostfixOpPat ( Anti_PostfixOp v) = Just $ TH.varP (TH.mkName v)
antiPostfixOpPat _ = Nothing


antiPrefixOpPat :: PrefixOp -> Maybe (TH.Q TH.Pat )
antiPrefixOpPat ( Anti_PrefixOp v) = Just $ TH.varP (TH.mkName v)
antiPrefixOpPat _ = Nothing


antiMultiplicativeOpPat :: MultiplicativeOp -> Maybe (TH.Q TH.Pat )
antiMultiplicativeOpPat ( Anti_MultiplicativeOp v) = Just $ TH.varP (TH.mkName v)
antiMultiplicativeOpPat _ = Nothing


antiAdditiveOpPat :: AdditiveOp -> Maybe (TH.Q TH.Pat )
antiAdditiveOpPat ( Anti_AdditiveOp v) = Just $ TH.varP (TH.mkName v)
antiAdditiveOpPat _ = Nothing


antiShiftOpPat :: ShiftOp -> Maybe (TH.Q TH.Pat )
antiShiftOpPat ( Anti_ShiftOp v) = Just $ TH.varP (TH.mkName v)
antiShiftOpPat _ = Nothing


antiRelationalOpPat :: RelationalOp -> Maybe (TH.Q TH.Pat )
antiRelationalOpPat ( Anti_RelationalOp v) = Just $ TH.varP (TH.mkName v)
antiRelationalOpPat _ = Nothing


antiEqualityOpPat :: EqualityOp -> Maybe (TH.Q TH.Pat )
antiEqualityOpPat ( Anti_EqualityOp v) = Just $ TH.varP (TH.mkName v)
antiEqualityOpPat _ = Nothing


antiAssignmentOpPat :: AssignmentOp -> Maybe (TH.Q TH.Pat )
antiAssignmentOpPat ( Anti_AssignmentOp v) = Just $ TH.varP (TH.mkName v)
antiAssignmentOpPat _ = Nothing


antiExpressionPat :: Expression -> Maybe (TH.Q TH.Pat )
antiExpressionPat ( Anti_Expression v) = Just $ TH.varP (TH.mkName v)
antiExpressionPat _ = Nothing


antiSwitchStatementPat :: SwitchStatement -> Maybe (TH.Q TH.Pat )
antiSwitchStatementPat ( Anti_SwitchStatement v) = Just $ TH.varP (TH.mkName v)
antiSwitchStatementPat _ = Nothing


antiRule_68Pat :: [ Rule_68 ] -> Maybe (TH.Q TH.Pat)
antiRule_68Pat [Anti_Rule_68 v] = Just $ TH.varP (TH.mkName v)
antiRule_68Pat _ = Nothing


antiTryStatementPat :: TryStatement -> Maybe (TH.Q TH.Pat )
antiTryStatementPat ( Anti_TryStatement v) = Just $ TH.varP (TH.mkName v)
antiTryStatementPat _ = Nothing


antiOptFinallyPat :: OptFinally -> Maybe (TH.Q TH.Pat )
antiOptFinallyPat ( Anti_OptFinally v) = Just $ TH.varP (TH.mkName v)
antiOptFinallyPat _ = Nothing


antiRule_65Pat :: [ Rule_65 ] -> Maybe (TH.Q TH.Pat)
antiRule_65Pat [Anti_Rule_65 v] = Just $ TH.varP (TH.mkName v)
antiRule_65Pat _ = Nothing


antiForStatementPat :: ForStatement -> Maybe (TH.Q TH.Pat )
antiForStatementPat ( Anti_ForStatement v) = Just $ TH.varP (TH.mkName v)
antiForStatementPat _ = Nothing


antiWhileStatementPat :: WhileStatement -> Maybe (TH.Q TH.Pat )
antiWhileStatementPat ( Anti_WhileStatement v) = Just $ TH.varP (TH.mkName v)
antiWhileStatementPat _ = Nothing


antiDoStatementPat :: DoStatement -> Maybe (TH.Q TH.Pat )
antiDoStatementPat ( Anti_DoStatement v) = Just $ TH.varP (TH.mkName v)
antiDoStatementPat _ = Nothing


antiIfStatementPat :: IfStatement -> Maybe (TH.Q TH.Pat )
antiIfStatementPat ( Anti_IfStatement v) = Just $ TH.varP (TH.mkName v)
antiIfStatementPat _ = Nothing


antiOptElsePartPat :: OptElsePart -> Maybe (TH.Q TH.Pat )
antiOptElsePartPat ( Anti_OptElsePart v) = Just $ TH.varP (TH.mkName v)
antiOptElsePartPat _ = Nothing


antiStatementWithoutIfPat :: StatementWithoutIf -> Maybe (TH.Q TH.Pat )
antiStatementWithoutIfPat ( Anti_StatementWithoutIf v) = Just $ TH.varP (TH.mkName v)
antiStatementWithoutIfPat _ = Nothing


antiOptIdPat :: OptId -> Maybe (TH.Q TH.Pat )
antiOptIdPat ( Anti_OptId v) = Just $ TH.varP (TH.mkName v)
antiOptIdPat _ = Nothing


antiOptExpressionPat :: OptExpression -> Maybe (TH.Q TH.Pat )
antiOptExpressionPat ( Anti_OptExpression v) = Just $ TH.varP (TH.mkName v)
antiOptExpressionPat _ = Nothing


antiStatementPat :: [ Statement ] -> Maybe (TH.Q TH.Pat)
antiStatementPat [Anti_Statement v] = Just $ TH.varP (TH.mkName v)
antiStatementPat _ = Nothing


antiParameterPat :: Parameter -> Maybe (TH.Q TH.Pat )
antiParameterPat ( Anti_Parameter v) = Just $ TH.varP (TH.mkName v)
antiParameterPat _ = Nothing


antiParameterListPat :: ParameterList -> Maybe (TH.Q TH.Pat )
antiParameterListPat ( Anti_ParameterList v) = Just $ TH.varP (TH.mkName v)
antiParameterListPat _ = Nothing


antiStaticInitializerPat :: StaticInitializer -> Maybe (TH.Q TH.Pat )
antiStaticInitializerPat ( Anti_StaticInitializer v) = Just $ TH.varP (TH.mkName v)
antiStaticInitializerPat _ = Nothing


antiVariableInitializerPat :: VariableInitializer -> Maybe (TH.Q TH.Pat )
antiVariableInitializerPat ( Anti_VariableInitializer v) = Just $ TH.varP (TH.mkName v)
antiVariableInitializerPat _ = Nothing


antiVariableInitializerListPat :: VariableInitializerList -> Maybe (TH.Q TH.Pat )
antiVariableInitializerListPat ( Anti_VariableInitializerList v) = Just $ TH.varP (TH.mkName v)
antiVariableInitializerListPat _ = Nothing


antiVariableDeclaratorPat :: VariableDeclarator -> Maybe (TH.Q TH.Pat )
antiVariableDeclaratorPat ( Anti_VariableDeclarator v) = Just $ TH.varP (TH.mkName v)
antiVariableDeclaratorPat _ = Nothing


antiOptVariableInitializerPat :: OptVariableInitializer -> Maybe (TH.Q TH.Pat )
antiOptVariableInitializerPat ( Anti_OptVariableInitializer v) = Just $ TH.varP (TH.mkName v)
antiOptVariableInitializerPat _ = Nothing


antiVariableDeclarationPat :: VariableDeclaration -> Maybe (TH.Q TH.Pat )
antiVariableDeclarationPat ( Anti_VariableDeclaration v) = Just $ TH.varP (TH.mkName v)
antiVariableDeclarationPat _ = Nothing


antiVariableDeclaratorListPat :: VariableDeclaratorList -> Maybe (TH.Q TH.Pat )
antiVariableDeclaratorListPat ( Anti_VariableDeclaratorList v) = Just $ TH.varP (TH.mkName v)
antiVariableDeclaratorListPat _ = Nothing


antiStatementBlockPat :: StatementBlock -> Maybe (TH.Q TH.Pat )
antiStatementBlockPat ( Anti_StatementBlock v) = Just $ TH.varP (TH.mkName v)
antiStatementBlockPat _ = Nothing


antiRule_49Pat :: [ Rule_49 ] -> Maybe (TH.Q TH.Pat)
antiRule_49Pat [Anti_Rule_49 v] = Just $ TH.varP (TH.mkName v)
antiRule_49Pat _ = Nothing


antiMemberRestPat :: MemberRest -> Maybe (TH.Q TH.Pat )
antiMemberRestPat ( Anti_MemberRest v) = Just $ TH.varP (TH.mkName v)
antiMemberRestPat _ = Nothing


antiMoreTypeSpecifierPat :: MoreTypeSpecifier -> Maybe (TH.Q TH.Pat )
antiMoreTypeSpecifierPat ( Anti_MoreTypeSpecifier v) = Just $ TH.varP (TH.mkName v)
antiMoreTypeSpecifierPat _ = Nothing


antiMemberAfterFirstIdPat :: MemberAfterFirstId -> Maybe (TH.Q TH.Pat )
antiMemberAfterFirstIdPat ( Anti_MemberAfterFirstId v) = Just $ TH.varP (TH.mkName v)
antiMemberAfterFirstIdPat _ = Nothing


antiPrimitiveTypeKeywordPat :: PrimitiveTypeKeyword -> Maybe (TH.Q TH.Pat )
antiPrimitiveTypeKeywordPat ( Anti_PrimitiveTypeKeyword v) = Just $ TH.varP (TH.mkName v)
antiPrimitiveTypeKeywordPat _ = Nothing


antiMemberDeclarationPat :: MemberDeclaration -> Maybe (TH.Q TH.Pat )
antiMemberDeclarationPat ( Anti_MemberDeclaration v) = Just $ TH.varP (TH.mkName v)
antiMemberDeclarationPat _ = Nothing


antiRule_44Pat :: [ Rule_44 ] -> Maybe (TH.Q TH.Pat)
antiRule_44Pat [Anti_Rule_44 v] = Just $ TH.varP (TH.mkName v)
antiRule_44Pat _ = Nothing


antiNestedTypeDeclarationPat :: NestedTypeDeclaration -> Maybe (TH.Q TH.Pat )
antiNestedTypeDeclarationPat ( Anti_NestedTypeDeclaration v) = Just $ TH.varP (TH.mkName v)
antiNestedTypeDeclarationPat _ = Nothing


antiEnumDeclarationPat :: EnumDeclaration -> Maybe (TH.Q TH.Pat )
antiEnumDeclarationPat ( Anti_EnumDeclaration v) = Just $ TH.varP (TH.mkName v)
antiEnumDeclarationPat _ = Nothing


antiEnumConstantListPat :: EnumConstantList -> Maybe (TH.Q TH.Pat )
antiEnumConstantListPat ( Anti_EnumConstantList v) = Just $ TH.varP (TH.mkName v)
antiEnumConstantListPat _ = Nothing


antiEnumConstantPat :: EnumConstant -> Maybe (TH.Q TH.Pat )
antiEnumConstantPat ( Anti_EnumConstant v) = Just $ TH.varP (TH.mkName v)
antiEnumConstantPat _ = Nothing


antiAnnotationTypeElementPat :: [ AnnotationTypeElement ] -> Maybe (TH.Q TH.Pat)
antiAnnotationTypeElementPat [Anti_AnnotationTypeElement v] = Just $ TH.varP (TH.mkName v)
antiAnnotationTypeElementPat _ = Nothing


antiAnnotationDeclarationPat :: AnnotationDeclaration -> Maybe (TH.Q TH.Pat )
antiAnnotationDeclarationPat ( Anti_AnnotationDeclaration v) = Just $ TH.varP (TH.mkName v)
antiAnnotationDeclarationPat _ = Nothing


antiInterfaceDeclarationPat :: InterfaceDeclaration -> Maybe (TH.Q TH.Pat )
antiInterfaceDeclarationPat ( Anti_InterfaceDeclaration v) = Just $ TH.varP (TH.mkName v)
antiInterfaceDeclarationPat _ = Nothing


antiClassDeclarationPat :: ClassDeclaration -> Maybe (TH.Q TH.Pat )
antiClassDeclarationPat ( Anti_ClassDeclaration v) = Just $ TH.varP (TH.mkName v)
antiClassDeclarationPat _ = Nothing


antiFieldDeclarationPat :: [ FieldDeclaration ] -> Maybe (TH.Q TH.Pat)
antiFieldDeclarationPat [Anti_FieldDeclaration v] = Just $ TH.varP (TH.mkName v)
antiFieldDeclarationPat _ = Nothing


antiImplementsListPat :: ImplementsList -> Maybe (TH.Q TH.Pat )
antiImplementsListPat ( Anti_ImplementsList v) = Just $ TH.varP (TH.mkName v)
antiImplementsListPat _ = Nothing


antiExtendsListPat :: ExtendsList -> Maybe (TH.Q TH.Pat )
antiExtendsListPat ( Anti_ExtendsList v) = Just $ TH.varP (TH.mkName v)
antiExtendsListPat _ = Nothing


antiRule_18Pat :: [ Rule_18 ] -> Maybe (TH.Q TH.Pat)
antiRule_18Pat [Anti_Rule_18 v] = Just $ TH.varP (TH.mkName v)
antiRule_18Pat _ = Nothing


antiAnnotationElementPat :: AnnotationElement -> Maybe (TH.Q TH.Pat )
antiAnnotationElementPat ( Anti_AnnotationElement v) = Just $ TH.varP (TH.mkName v)
antiAnnotationElementPat _ = Nothing


antiAnnotationArgumentsPat :: AnnotationArguments -> Maybe (TH.Q TH.Pat )
antiAnnotationArgumentsPat ( Anti_AnnotationArguments v) = Just $ TH.varP (TH.mkName v)
antiAnnotationArgumentsPat _ = Nothing


antiAnnotationPat :: Annotation -> Maybe (TH.Q TH.Pat )
antiAnnotationPat ( Anti_Annotation v) = Just $ TH.varP (TH.mkName v)
antiAnnotationPat _ = Nothing


antiDocCommentPat :: DocComment -> Maybe (TH.Q TH.Pat )
antiDocCommentPat ( Anti_DocComment v) = Just $ TH.varP (TH.mkName v)
antiDocCommentPat _ = Nothing


antiImportStatementPat :: ImportStatement -> Maybe (TH.Q TH.Pat )
antiImportStatementPat ( Anti_ImportStatement v) = Just $ TH.varP (TH.mkName v)
antiImportStatementPat _ = Nothing


antiPackagePat :: Package -> Maybe (TH.Q TH.Pat )
antiPackagePat ( Anti_Package v) = Just $ TH.varP (TH.mkName v)
antiPackagePat _ = Nothing


antiCompilationUnitPat :: CompilationUnit -> Maybe (TH.Q TH.Pat )
antiCompilationUnitPat ( Anti_CompilationUnit v) = Just $ TH.varP (TH.mkName v)
antiCompilationUnitPat _ = Nothing


antiRule_2Pat :: [ Rule_2 ] -> Maybe (TH.Q TH.Pat)
antiRule_2Pat [Anti_Rule_2 v] = Just $ TH.varP (TH.mkName v)
antiRule_2Pat _ = Nothing


antiTypeDeclarationPat :: TypeDeclaration -> Maybe (TH.Q TH.Pat )
antiTypeDeclarationPat ( Anti_TypeDeclaration v) = Just $ TH.varP (TH.mkName v)
antiTypeDeclarationPat _ = Nothing


antiOptDocCommentPat :: OptDocComment -> Maybe (TH.Q TH.Pat )
antiOptDocCommentPat ( Anti_OptDocComment v) = Just $ TH.varP (TH.mkName v)
antiOptDocCommentPat _ = Nothing


antiJavaPat :: Java -> Maybe (TH.Q TH.Pat )
antiJavaPat ( Anti_Java v) = Just $ TH.varP (TH.mkName v)
antiJavaPat _ = Nothing



quoteJavaType s = return TH.ListT
quoteJavaDecs s = return []

getJava ( Ctr__Java__0 s) = s

java :: QuasiQuoter
java = QuasiQuoter (quoteJavaExp "tok_Java_dummy_172" getJava ) (quoteJavaPat "tok_Java_dummy_172" getJava ) quoteJavaType quoteJavaDecs

getAdditiveOp ( Ctr__Java__1 s) = s

additiveOp :: QuasiQuoter
additiveOp = QuasiQuoter (quoteJavaExp "tok_AdditiveOp_dummy_171" getAdditiveOp ) (quoteJavaPat "tok_AdditiveOp_dummy_171" getAdditiveOp ) quoteJavaType quoteJavaDecs

getAnnotation ( Ctr__Java__2 s) = s

annotation :: QuasiQuoter
annotation = QuasiQuoter (quoteJavaExp "tok_Annotation_dummy_170" getAnnotation ) (quoteJavaPat "tok_Annotation_dummy_170" getAnnotation ) quoteJavaType quoteJavaDecs

getAnnotationArguments ( Ctr__Java__3 s) = s

annotationArguments :: QuasiQuoter
annotationArguments = QuasiQuoter (quoteJavaExp "tok_AnnotationArguments_dummy_169" getAnnotationArguments ) (quoteJavaPat "tok_AnnotationArguments_dummy_169" getAnnotationArguments ) quoteJavaType quoteJavaDecs

getAnnotationDeclaration ( Ctr__Java__4 s) = s

annotationDeclaration :: QuasiQuoter
annotationDeclaration = QuasiQuoter (quoteJavaExp "tok_AnnotationDeclaration_dummy_168" getAnnotationDeclaration ) (quoteJavaPat "tok_AnnotationDeclaration_dummy_168" getAnnotationDeclaration ) quoteJavaType quoteJavaDecs

getAnnotationElement ( Ctr__Java__5 s) = s

annotationElement :: QuasiQuoter
annotationElement = QuasiQuoter (quoteJavaExp "tok_AnnotationElement_dummy_167" getAnnotationElement ) (quoteJavaPat "tok_AnnotationElement_dummy_167" getAnnotationElement ) quoteJavaType quoteJavaDecs

getAnnotationList ( Ctr__Java__6 s) = s

annotationList :: QuasiQuoter
annotationList = QuasiQuoter (quoteJavaExp "tok_AnnotationList_dummy_166" getAnnotationList ) (quoteJavaPat "tok_AnnotationList_dummy_166" getAnnotationList ) quoteJavaType quoteJavaDecs

getAnnotationTypeElement ( Ctr__Java__7 s) = s

annotationTypeElement :: QuasiQuoter
annotationTypeElement = QuasiQuoter (quoteJavaExp "tok_AnnotationTypeElement_dummy_165" getAnnotationTypeElement ) (quoteJavaPat "tok_AnnotationTypeElement_dummy_165" getAnnotationTypeElement ) quoteJavaType quoteJavaDecs

getAnnotationTypeElementList ( Ctr__Java__8 s) = s

annotationTypeElementList :: QuasiQuoter
annotationTypeElementList = QuasiQuoter (quoteJavaExp "tok_AnnotationTypeElementList_dummy_164" getAnnotationTypeElementList ) (quoteJavaPat "tok_AnnotationTypeElementList_dummy_164" getAnnotationTypeElementList ) quoteJavaType quoteJavaDecs

getArglist ( Ctr__Java__9 s) = s

arglist :: QuasiQuoter
arglist = QuasiQuoter (quoteJavaExp "tok_Arglist_dummy_163" getArglist ) (quoteJavaPat "tok_Arglist_dummy_163" getArglist ) quoteJavaType quoteJavaDecs

getAssignmentOp ( Ctr__Java__10 s) = s

assignmentOp :: QuasiQuoter
assignmentOp = QuasiQuoter (quoteJavaExp "tok_AssignmentOp_dummy_162" getAssignmentOp ) (quoteJavaPat "tok_AssignmentOp_dummy_162" getAssignmentOp ) quoteJavaType quoteJavaDecs

getCatchList ( Ctr__Java__11 s) = s

catchList :: QuasiQuoter
catchList = QuasiQuoter (quoteJavaExp "tok_CatchList_dummy_161" getCatchList ) (quoteJavaPat "tok_CatchList_dummy_161" getCatchList ) quoteJavaType quoteJavaDecs

getClassDeclaration ( Ctr__Java__12 s) = s

classDeclaration :: QuasiQuoter
classDeclaration = QuasiQuoter (quoteJavaExp "tok_ClassDeclaration_dummy_160" getClassDeclaration ) (quoteJavaPat "tok_ClassDeclaration_dummy_160" getClassDeclaration ) quoteJavaType quoteJavaDecs

getCompilationUnit ( Ctr__Java__13 s) = s

compilationUnit :: QuasiQuoter
compilationUnit = QuasiQuoter (quoteJavaExp "tok_CompilationUnit_dummy_159" getCompilationUnit ) (quoteJavaPat "tok_CompilationUnit_dummy_159" getCompilationUnit ) quoteJavaType quoteJavaDecs

getCompoundName ( Ctr__Java__14 s) = s

compoundName :: QuasiQuoter
compoundName = QuasiQuoter (quoteJavaExp "tok_CompoundName_dummy_158" getCompoundName ) (quoteJavaPat "tok_CompoundName_dummy_158" getCompoundName ) quoteJavaType quoteJavaDecs

getCreationExpression ( Ctr__Java__15 s) = s

creationExpression :: QuasiQuoter
creationExpression = QuasiQuoter (quoteJavaExp "tok_CreationExpression_dummy_157" getCreationExpression ) (quoteJavaPat "tok_CreationExpression_dummy_157" getCreationExpression ) quoteJavaType quoteJavaDecs

getDoStatement ( Ctr__Java__16 s) = s

doStatement :: QuasiQuoter
doStatement = QuasiQuoter (quoteJavaExp "tok_DoStatement_dummy_156" getDoStatement ) (quoteJavaPat "tok_DoStatement_dummy_156" getDoStatement ) quoteJavaType quoteJavaDecs

getDocComment ( Ctr__Java__17 s) = s

docComment :: QuasiQuoter
docComment = QuasiQuoter (quoteJavaExp "tok_DocComment_dummy_155" getDocComment ) (quoteJavaPat "tok_DocComment_dummy_155" getDocComment ) quoteJavaType quoteJavaDecs

getEnumConstant ( Ctr__Java__18 s) = s

enumConstant :: QuasiQuoter
enumConstant = QuasiQuoter (quoteJavaExp "tok_EnumConstant_dummy_154" getEnumConstant ) (quoteJavaPat "tok_EnumConstant_dummy_154" getEnumConstant ) quoteJavaType quoteJavaDecs

getEnumConstantList ( Ctr__Java__19 s) = s

enumConstantList :: QuasiQuoter
enumConstantList = QuasiQuoter (quoteJavaExp "tok_EnumConstantList_dummy_153" getEnumConstantList ) (quoteJavaPat "tok_EnumConstantList_dummy_153" getEnumConstantList ) quoteJavaType quoteJavaDecs

getEnumDeclaration ( Ctr__Java__20 s) = s

enumDeclaration :: QuasiQuoter
enumDeclaration = QuasiQuoter (quoteJavaExp "tok_EnumDeclaration_dummy_152" getEnumDeclaration ) (quoteJavaPat "tok_EnumDeclaration_dummy_152" getEnumDeclaration ) quoteJavaType quoteJavaDecs

getEqualityOp ( Ctr__Java__21 s) = s

equalityOp :: QuasiQuoter
equalityOp = QuasiQuoter (quoteJavaExp "tok_EqualityOp_dummy_151" getEqualityOp ) (quoteJavaPat "tok_EqualityOp_dummy_151" getEqualityOp ) quoteJavaType quoteJavaDecs

getExpression ( Ctr__Java__22 s) = s

expression :: QuasiQuoter
expression = QuasiQuoter (quoteJavaExp "tok_Expression_dummy_150" getExpression ) (quoteJavaPat "tok_Expression_dummy_150" getExpression ) quoteJavaType quoteJavaDecs

getExtendsList ( Ctr__Java__23 s) = s

extendsList :: QuasiQuoter
extendsList = QuasiQuoter (quoteJavaExp "tok_ExtendsList_dummy_149" getExtendsList ) (quoteJavaPat "tok_ExtendsList_dummy_149" getExtendsList ) quoteJavaType quoteJavaDecs

getFieldDeclaration ( Ctr__Java__24 s) = s

fieldDeclaration :: QuasiQuoter
fieldDeclaration = QuasiQuoter (quoteJavaExp "tok_FieldDeclaration_dummy_148" getFieldDeclaration ) (quoteJavaPat "tok_FieldDeclaration_dummy_148" getFieldDeclaration ) quoteJavaType quoteJavaDecs

getFieldDeclarationList ( Ctr__Java__25 s) = s

fieldDeclarationList :: QuasiQuoter
fieldDeclarationList = QuasiQuoter (quoteJavaExp "tok_FieldDeclarationList_dummy_147" getFieldDeclarationList ) (quoteJavaPat "tok_FieldDeclarationList_dummy_147" getFieldDeclarationList ) quoteJavaType quoteJavaDecs

getForStatement ( Ctr__Java__26 s) = s

forStatement :: QuasiQuoter
forStatement = QuasiQuoter (quoteJavaExp "tok_ForStatement_dummy_146" getForStatement ) (quoteJavaPat "tok_ForStatement_dummy_146" getForStatement ) quoteJavaType quoteJavaDecs

getIfStatement ( Ctr__Java__27 s) = s

ifStatement :: QuasiQuoter
ifStatement = QuasiQuoter (quoteJavaExp "tok_IfStatement_dummy_145" getIfStatement ) (quoteJavaPat "tok_IfStatement_dummy_145" getIfStatement ) quoteJavaType quoteJavaDecs

getImplementsList ( Ctr__Java__28 s) = s

implementsList :: QuasiQuoter
implementsList = QuasiQuoter (quoteJavaExp "tok_ImplementsList_dummy_144" getImplementsList ) (quoteJavaPat "tok_ImplementsList_dummy_144" getImplementsList ) quoteJavaType quoteJavaDecs

getImportList ( Ctr__Java__29 s) = s

importList :: QuasiQuoter
importList = QuasiQuoter (quoteJavaExp "tok_ImportList_dummy_143" getImportList ) (quoteJavaPat "tok_ImportList_dummy_143" getImportList ) quoteJavaType quoteJavaDecs

getImportStatement ( Ctr__Java__30 s) = s

importStatement :: QuasiQuoter
importStatement = QuasiQuoter (quoteJavaExp "tok_ImportStatement_dummy_142" getImportStatement ) (quoteJavaPat "tok_ImportStatement_dummy_142" getImportStatement ) quoteJavaType quoteJavaDecs

getInterfaceDeclaration ( Ctr__Java__31 s) = s

interfaceDeclaration :: QuasiQuoter
interfaceDeclaration = QuasiQuoter (quoteJavaExp "tok_InterfaceDeclaration_dummy_141" getInterfaceDeclaration ) (quoteJavaPat "tok_InterfaceDeclaration_dummy_141" getInterfaceDeclaration ) quoteJavaType quoteJavaDecs

getLiteral ( Ctr__Java__32 s) = s

literal :: QuasiQuoter
literal = QuasiQuoter (quoteJavaExp "tok_Literal_dummy_140" getLiteral ) (quoteJavaPat "tok_Literal_dummy_140" getLiteral ) quoteJavaType quoteJavaDecs

getMemberAfterFirstId ( Ctr__Java__33 s) = s

memberAfterFirstId :: QuasiQuoter
memberAfterFirstId = QuasiQuoter (quoteJavaExp "tok_MemberAfterFirstId_dummy_139" getMemberAfterFirstId ) (quoteJavaPat "tok_MemberAfterFirstId_dummy_139" getMemberAfterFirstId ) quoteJavaType quoteJavaDecs

getMemberDeclaration ( Ctr__Java__34 s) = s

memberDeclaration :: QuasiQuoter
memberDeclaration = QuasiQuoter (quoteJavaExp "tok_MemberDeclaration_dummy_138" getMemberDeclaration ) (quoteJavaPat "tok_MemberDeclaration_dummy_138" getMemberDeclaration ) quoteJavaType quoteJavaDecs

getMemberRest ( Ctr__Java__35 s) = s

memberRest :: QuasiQuoter
memberRest = QuasiQuoter (quoteJavaExp "tok_MemberRest_dummy_137" getMemberRest ) (quoteJavaPat "tok_MemberRest_dummy_137" getMemberRest ) quoteJavaType quoteJavaDecs

getModifier ( Ctr__Java__36 s) = s

modifier :: QuasiQuoter
modifier = QuasiQuoter (quoteJavaExp "tok_Modifier_dummy_136" getModifier ) (quoteJavaPat "tok_Modifier_dummy_136" getModifier ) quoteJavaType quoteJavaDecs

getModifierList ( Ctr__Java__37 s) = s

modifierList :: QuasiQuoter
modifierList = QuasiQuoter (quoteJavaExp "tok_ModifierList_dummy_135" getModifierList ) (quoteJavaPat "tok_ModifierList_dummy_135" getModifierList ) quoteJavaType quoteJavaDecs

getMoreTypeSpecifier ( Ctr__Java__38 s) = s

moreTypeSpecifier :: QuasiQuoter
moreTypeSpecifier = QuasiQuoter (quoteJavaExp "tok_MoreTypeSpecifier_dummy_134" getMoreTypeSpecifier ) (quoteJavaPat "tok_MoreTypeSpecifier_dummy_134" getMoreTypeSpecifier ) quoteJavaType quoteJavaDecs

getMoreVariableDeclarators ( Ctr__Java__39 s) = s

moreVariableDeclarators :: QuasiQuoter
moreVariableDeclarators = QuasiQuoter (quoteJavaExp "tok_MoreVariableDeclarators_dummy_133" getMoreVariableDeclarators ) (quoteJavaPat "tok_MoreVariableDeclarators_dummy_133" getMoreVariableDeclarators ) quoteJavaType quoteJavaDecs

getMultiplicativeOp ( Ctr__Java__40 s) = s

multiplicativeOp :: QuasiQuoter
multiplicativeOp = QuasiQuoter (quoteJavaExp "tok_MultiplicativeOp_dummy_132" getMultiplicativeOp ) (quoteJavaPat "tok_MultiplicativeOp_dummy_132" getMultiplicativeOp ) quoteJavaType quoteJavaDecs

getNestedTypeDeclaration ( Ctr__Java__41 s) = s

nestedTypeDeclaration :: QuasiQuoter
nestedTypeDeclaration = QuasiQuoter (quoteJavaExp "tok_NestedTypeDeclaration_dummy_131" getNestedTypeDeclaration ) (quoteJavaPat "tok_NestedTypeDeclaration_dummy_131" getNestedTypeDeclaration ) quoteJavaType quoteJavaDecs

getOptDocComment ( Ctr__Java__42 s) = s

optDocComment :: QuasiQuoter
optDocComment = QuasiQuoter (quoteJavaExp "tok_OptDocComment_dummy_130" getOptDocComment ) (quoteJavaPat "tok_OptDocComment_dummy_130" getOptDocComment ) quoteJavaType quoteJavaDecs

getOptElsePart ( Ctr__Java__43 s) = s

optElsePart :: QuasiQuoter
optElsePart = QuasiQuoter (quoteJavaExp "tok_OptElsePart_dummy_129" getOptElsePart ) (quoteJavaPat "tok_OptElsePart_dummy_129" getOptElsePart ) quoteJavaType quoteJavaDecs

getOptExpression ( Ctr__Java__44 s) = s

optExpression :: QuasiQuoter
optExpression = QuasiQuoter (quoteJavaExp "tok_OptExpression_dummy_128" getOptExpression ) (quoteJavaPat "tok_OptExpression_dummy_128" getOptExpression ) quoteJavaType quoteJavaDecs

getOptFinally ( Ctr__Java__45 s) = s

optFinally :: QuasiQuoter
optFinally = QuasiQuoter (quoteJavaExp "tok_OptFinally_dummy_127" getOptFinally ) (quoteJavaPat "tok_OptFinally_dummy_127" getOptFinally ) quoteJavaType quoteJavaDecs

getOptId ( Ctr__Java__46 s) = s

optId :: QuasiQuoter
optId = QuasiQuoter (quoteJavaExp "tok_OptId_dummy_126" getOptId ) (quoteJavaPat "tok_OptId_dummy_126" getOptId ) quoteJavaType quoteJavaDecs

getOptVariableInitializer ( Ctr__Java__47 s) = s

optVariableInitializer :: QuasiQuoter
optVariableInitializer = QuasiQuoter (quoteJavaExp "tok_OptVariableInitializer_dummy_125" getOptVariableInitializer ) (quoteJavaPat "tok_OptVariableInitializer_dummy_125" getOptVariableInitializer ) quoteJavaType quoteJavaDecs

getPackage ( Ctr__Java__48 s) = s

package :: QuasiQuoter
package = QuasiQuoter (quoteJavaExp "tok_Package_dummy_124" getPackage ) (quoteJavaPat "tok_Package_dummy_124" getPackage ) quoteJavaType quoteJavaDecs

getParameter ( Ctr__Java__49 s) = s

parameter :: QuasiQuoter
parameter = QuasiQuoter (quoteJavaExp "tok_Parameter_dummy_123" getParameter ) (quoteJavaPat "tok_Parameter_dummy_123" getParameter ) quoteJavaType quoteJavaDecs

getParameterList ( Ctr__Java__50 s) = s

parameterList :: QuasiQuoter
parameterList = QuasiQuoter (quoteJavaExp "tok_ParameterList_dummy_122" getParameterList ) (quoteJavaPat "tok_ParameterList_dummy_122" getParameterList ) quoteJavaType quoteJavaDecs

getPostfixOp ( Ctr__Java__51 s) = s

postfixOp :: QuasiQuoter
postfixOp = QuasiQuoter (quoteJavaExp "tok_PostfixOp_dummy_121" getPostfixOp ) (quoteJavaPat "tok_PostfixOp_dummy_121" getPostfixOp ) quoteJavaType quoteJavaDecs

getPrefixOp ( Ctr__Java__52 s) = s

prefixOp :: QuasiQuoter
prefixOp = QuasiQuoter (quoteJavaExp "tok_PrefixOp_dummy_120" getPrefixOp ) (quoteJavaPat "tok_PrefixOp_dummy_120" getPrefixOp ) quoteJavaType quoteJavaDecs

getPrimitiveTypeKeyword ( Ctr__Java__53 s) = s

primitiveTypeKeyword :: QuasiQuoter
primitiveTypeKeyword = QuasiQuoter (quoteJavaExp "tok_PrimitiveTypeKeyword_dummy_119" getPrimitiveTypeKeyword ) (quoteJavaPat "tok_PrimitiveTypeKeyword_dummy_119" getPrimitiveTypeKeyword ) quoteJavaType quoteJavaDecs

getRelationalOp ( Ctr__Java__54 s) = s

relationalOp :: QuasiQuoter
relationalOp = QuasiQuoter (quoteJavaExp "tok_RelationalOp_dummy_118" getRelationalOp ) (quoteJavaPat "tok_RelationalOp_dummy_118" getRelationalOp ) quoteJavaType quoteJavaDecs

getShiftOp ( Ctr__Java__55 s) = s

shiftOp :: QuasiQuoter
shiftOp = QuasiQuoter (quoteJavaExp "tok_ShiftOp_dummy_117" getShiftOp ) (quoteJavaPat "tok_ShiftOp_dummy_117" getShiftOp ) quoteJavaType quoteJavaDecs

getSquareBracketsList ( Ctr__Java__56 s) = s

squareBracketsList :: QuasiQuoter
squareBracketsList = QuasiQuoter (quoteJavaExp "tok_SquareBracketsList_dummy_116" getSquareBracketsList ) (quoteJavaPat "tok_SquareBracketsList_dummy_116" getSquareBracketsList ) quoteJavaType quoteJavaDecs

getStatement ( Ctr__Java__57 s) = s

statement :: QuasiQuoter
statement = QuasiQuoter (quoteJavaExp "tok_Statement_dummy_115" getStatement ) (quoteJavaPat "tok_Statement_dummy_115" getStatement ) quoteJavaType quoteJavaDecs

getStatementBlock ( Ctr__Java__58 s) = s

statementBlock :: QuasiQuoter
statementBlock = QuasiQuoter (quoteJavaExp "tok_StatementBlock_dummy_114" getStatementBlock ) (quoteJavaPat "tok_StatementBlock_dummy_114" getStatementBlock ) quoteJavaType quoteJavaDecs

getStatementList ( Ctr__Java__59 s) = s

statementList :: QuasiQuoter
statementList = QuasiQuoter (quoteJavaExp "tok_StatementList_dummy_113" getStatementList ) (quoteJavaPat "tok_StatementList_dummy_113" getStatementList ) quoteJavaType quoteJavaDecs

getStatementWithoutIf ( Ctr__Java__60 s) = s

statementWithoutIf :: QuasiQuoter
statementWithoutIf = QuasiQuoter (quoteJavaExp "tok_StatementWithoutIf_dummy_112" getStatementWithoutIf ) (quoteJavaPat "tok_StatementWithoutIf_dummy_112" getStatementWithoutIf ) quoteJavaType quoteJavaDecs

getStaticInitializer ( Ctr__Java__61 s) = s

staticInitializer :: QuasiQuoter
staticInitializer = QuasiQuoter (quoteJavaExp "tok_StaticInitializer_dummy_111" getStaticInitializer ) (quoteJavaPat "tok_StaticInitializer_dummy_111" getStaticInitializer ) quoteJavaType quoteJavaDecs

getSwitchCaseList ( Ctr__Java__62 s) = s

switchCaseList :: QuasiQuoter
switchCaseList = QuasiQuoter (quoteJavaExp "tok_SwitchCaseList_dummy_110" getSwitchCaseList ) (quoteJavaPat "tok_SwitchCaseList_dummy_110" getSwitchCaseList ) quoteJavaType quoteJavaDecs

getSwitchStatement ( Ctr__Java__63 s) = s

switchStatement :: QuasiQuoter
switchStatement = QuasiQuoter (quoteJavaExp "tok_SwitchStatement_dummy_109" getSwitchStatement ) (quoteJavaPat "tok_SwitchStatement_dummy_109" getSwitchStatement ) quoteJavaType quoteJavaDecs

getTryStatement ( Ctr__Java__64 s) = s

tryStatement :: QuasiQuoter
tryStatement = QuasiQuoter (quoteJavaExp "tok_TryStatement_dummy_108" getTryStatement ) (quoteJavaPat "tok_TryStatement_dummy_108" getTryStatement ) quoteJavaType quoteJavaDecs

getType ( Ctr__Java__65 s) = s

__type :: QuasiQuoter
__type = QuasiQuoter (quoteJavaExp "tok_Type_dummy_107" getType ) (quoteJavaPat "tok_Type_dummy_107" getType ) quoteJavaType quoteJavaDecs

getTypeArgument ( Ctr__Java__66 s) = s

typeArgument :: QuasiQuoter
typeArgument = QuasiQuoter (quoteJavaExp "tok_TypeArgument_dummy_106" getTypeArgument ) (quoteJavaPat "tok_TypeArgument_dummy_106" getTypeArgument ) quoteJavaType quoteJavaDecs

getTypeArguments ( Ctr__Java__67 s) = s

typeArguments :: QuasiQuoter
typeArguments = QuasiQuoter (quoteJavaExp "tok_TypeArguments_dummy_105" getTypeArguments ) (quoteJavaPat "tok_TypeArguments_dummy_105" getTypeArguments ) quoteJavaType quoteJavaDecs

getTypeDeclaration ( Ctr__Java__68 s) = s

typeDeclaration :: QuasiQuoter
typeDeclaration = QuasiQuoter (quoteJavaExp "tok_TypeDeclaration_dummy_104" getTypeDeclaration ) (quoteJavaPat "tok_TypeDeclaration_dummy_104" getTypeDeclaration ) quoteJavaType quoteJavaDecs

getTypeParameter ( Ctr__Java__69 s) = s

typeParameter :: QuasiQuoter
typeParameter = QuasiQuoter (quoteJavaExp "tok_TypeParameter_dummy_103" getTypeParameter ) (quoteJavaPat "tok_TypeParameter_dummy_103" getTypeParameter ) quoteJavaType quoteJavaDecs

getTypeParameters ( Ctr__Java__70 s) = s

typeParameters :: QuasiQuoter
typeParameters = QuasiQuoter (quoteJavaExp "tok_TypeParameters_dummy_102" getTypeParameters ) (quoteJavaPat "tok_TypeParameters_dummy_102" getTypeParameters ) quoteJavaType quoteJavaDecs

getTypeSpecifier ( Ctr__Java__71 s) = s

typeSpecifier :: QuasiQuoter
typeSpecifier = QuasiQuoter (quoteJavaExp "tok_TypeSpecifier_dummy_101" getTypeSpecifier ) (quoteJavaPat "tok_TypeSpecifier_dummy_101" getTypeSpecifier ) quoteJavaType quoteJavaDecs

getVariableDeclaration ( Ctr__Java__72 s) = s

variableDeclaration :: QuasiQuoter
variableDeclaration = QuasiQuoter (quoteJavaExp "tok_VariableDeclaration_dummy_100" getVariableDeclaration ) (quoteJavaPat "tok_VariableDeclaration_dummy_100" getVariableDeclaration ) quoteJavaType quoteJavaDecs

getVariableDeclarator ( Ctr__Java__73 s) = s

variableDeclarator :: QuasiQuoter
variableDeclarator = QuasiQuoter (quoteJavaExp "tok_VariableDeclarator_dummy_99" getVariableDeclarator ) (quoteJavaPat "tok_VariableDeclarator_dummy_99" getVariableDeclarator ) quoteJavaType quoteJavaDecs

getVariableDeclaratorList ( Ctr__Java__74 s) = s

variableDeclaratorList :: QuasiQuoter
variableDeclaratorList = QuasiQuoter (quoteJavaExp "tok_VariableDeclaratorList_dummy_98" getVariableDeclaratorList ) (quoteJavaPat "tok_VariableDeclaratorList_dummy_98" getVariableDeclaratorList ) quoteJavaType quoteJavaDecs

getVariableInitializer ( Ctr__Java__75 s) = s

variableInitializer :: QuasiQuoter
variableInitializer = QuasiQuoter (quoteJavaExp "tok_VariableInitializer_dummy_97" getVariableInitializer ) (quoteJavaPat "tok_VariableInitializer_dummy_97" getVariableInitializer ) quoteJavaType quoteJavaDecs

getVariableInitializerList ( Ctr__Java__76 s) = s

variableInitializerList :: QuasiQuoter
variableInitializerList = QuasiQuoter (quoteJavaExp "tok_VariableInitializerList_dummy_96" getVariableInitializerList ) (quoteJavaPat "tok_VariableInitializerList_dummy_96" getVariableInitializerList ) quoteJavaType quoteJavaDecs

getWhileStatement ( Ctr__Java__77 s) = s

whileStatement :: QuasiQuoter
whileStatement = QuasiQuoter (quoteJavaExp "tok_WhileStatement_dummy_95" getWhileStatement ) (quoteJavaPat "tok_WhileStatement_dummy_95" getWhileStatement ) quoteJavaType quoteJavaDecs

getWildcardType ( Ctr__Java__78 s) = s

wildcardType :: QuasiQuoter
wildcardType = QuasiQuoter (quoteJavaExp "tok_WildcardType_dummy_94" getWildcardType ) (quoteJavaPat "tok_WildcardType_dummy_94" getWildcardType ) quoteJavaType quoteJavaDecs

