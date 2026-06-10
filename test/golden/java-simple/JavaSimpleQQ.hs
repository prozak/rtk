{-# LANGUAGE TemplateHaskell #-}
module JavaSimpleQQ
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
import JavaSimpleLexer
import JavaSimpleParser

qqPattern = "\\$[A-Za-z_][A-Za-z_0-9]*[^A-Za-z_0-9:]"

qqShortcuts :: M.Map String String

replaceAllPatterns1 :: String -> String
replaceAllPatterns1 str = let (pre, match, post) = str =~ qqPattern :: (String, String, String)
                          in if match == ""
                              then pre
                              else let varName = init $ tail match
                                       addSym = last match
                                       ruleVariants = catMaybes $ map (\ prefix -> M.lookup prefix qqShortcuts) $ reverse $ inits varName
                                       rule = case ruleVariants of
                                                [] -> error $ "Unknown shortcut for " ++ varName
                                                (rule : _) -> rule
                                   in pre ++ ('$' : rule ++ ":") ++ varName ++ (replaceAllPatterns1 $ addSym : post)

-- Add ' ' at the end, so regex can match variable in the end of the string
replaceAllPatterns :: String -> String
replaceAllPatterns str = init $ replaceAllPatterns1 (str ++ " ")

qqShortcuts = M.fromList [ ("javaSimple","JavaSimple"),("classDeclaration","ClassDeclaration"),("compilationUnit","CompilationUnit"),("compoundName","CompoundName"),("field","Field"),("fieldList","FieldList"),("package","Package"),("type","Type")]

quoteJavaSimpleExp :: Data.Data a => String -> (JavaSimple -> a) -> String -> TH.ExpQ
quoteJavaSimpleExp dummy func s = do
  let s1 = replaceAllPatterns s
      expr = func $ parseJavaSimple $ alexScanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy)
  dataToExpQ (const Nothing `Generics.extQ` antiJavaSimpleExp `Generics.extQ` antiCompilationUnitExp `Generics.extQ` antiPackageExp `Generics.extQ` antiClassDeclarationExp `Generics.extQ` antiFieldExp `Generics.extQ` antiTypeExp `Generics.extQ` antiCompoundNameExp) expr
quoteJavaSimplePat :: Data.Data a => String -> (JavaSimple -> a) -> String -> TH.PatQ
quoteJavaSimplePat dummy func s = do
  let s1 = replaceAllPatterns s
      expr = func $ parseJavaSimple $ alexScanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy)
  dataToPatQ (const Nothing `Generics.extQ` antiJavaSimplePat `Generics.extQ` antiCompilationUnitPat `Generics.extQ` antiPackagePat `Generics.extQ` antiClassDeclarationPat `Generics.extQ` antiFieldPat `Generics.extQ` antiTypePat `Generics.extQ` antiCompoundNamePat) expr

antiCompoundNameExp :: CompoundName -> Maybe (TH.Q TH.Exp )
antiCompoundNameExp ( Anti_CompoundName v) = Just $ TH.varE (TH.mkName v)
antiCompoundNameExp _ = Nothing


antiTypeExp :: Type -> Maybe (TH.Q TH.Exp )
antiTypeExp ( Anti_Type v) = Just $ TH.varE (TH.mkName v)
antiTypeExp _ = Nothing


antiFieldExp :: [ Field ] -> Maybe (TH.Q TH.Exp)
antiFieldExp ((Anti_Field v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiJavaSimpleExp `Generics.extQ` antiCompilationUnitExp `Generics.extQ` antiPackageExp `Generics.extQ` antiClassDeclarationExp `Generics.extQ` antiFieldExp `Generics.extQ` antiTypeExp `Generics.extQ` antiCompoundNameExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiFieldExp _ = Nothing


antiClassDeclarationExp :: ClassDeclaration -> Maybe (TH.Q TH.Exp )
antiClassDeclarationExp ( Anti_ClassDeclaration v) = Just $ TH.varE (TH.mkName v)
antiClassDeclarationExp _ = Nothing


antiPackageExp :: Package -> Maybe (TH.Q TH.Exp )
antiPackageExp ( Anti_Package v) = Just $ TH.varE (TH.mkName v)
antiPackageExp _ = Nothing


antiCompilationUnitExp :: CompilationUnit -> Maybe (TH.Q TH.Exp )
antiCompilationUnitExp ( Anti_CompilationUnit v) = Just $ TH.varE (TH.mkName v)
antiCompilationUnitExp _ = Nothing


antiJavaSimpleExp :: JavaSimple -> Maybe (TH.Q TH.Exp )
antiJavaSimpleExp ( Anti_JavaSimple v) = Just $ TH.varE (TH.mkName v)
antiJavaSimpleExp _ = Nothing



antiCompoundNamePat :: CompoundName -> Maybe (TH.Q TH.Pat )
antiCompoundNamePat ( Anti_CompoundName v) = Just $ TH.varP (TH.mkName v)
antiCompoundNamePat _ = Nothing


antiTypePat :: Type -> Maybe (TH.Q TH.Pat )
antiTypePat ( Anti_Type v) = Just $ TH.varP (TH.mkName v)
antiTypePat _ = Nothing


antiFieldPat :: [ Field ] -> Maybe (TH.Q TH.Pat)
antiFieldPat [Anti_Field v] = Just $ TH.varP (TH.mkName v)
antiFieldPat _ = Nothing


antiClassDeclarationPat :: ClassDeclaration -> Maybe (TH.Q TH.Pat )
antiClassDeclarationPat ( Anti_ClassDeclaration v) = Just $ TH.varP (TH.mkName v)
antiClassDeclarationPat _ = Nothing


antiPackagePat :: Package -> Maybe (TH.Q TH.Pat )
antiPackagePat ( Anti_Package v) = Just $ TH.varP (TH.mkName v)
antiPackagePat _ = Nothing


antiCompilationUnitPat :: CompilationUnit -> Maybe (TH.Q TH.Pat )
antiCompilationUnitPat ( Anti_CompilationUnit v) = Just $ TH.varP (TH.mkName v)
antiCompilationUnitPat _ = Nothing


antiJavaSimplePat :: JavaSimple -> Maybe (TH.Q TH.Pat )
antiJavaSimplePat ( Anti_JavaSimple v) = Just $ TH.varP (TH.mkName v)
antiJavaSimplePat _ = Nothing



quoteJavaSimpleType s = return TH.ListT
quoteJavaSimpleDecs s = return []

getJavaSimple ( Ctr__JavaSimple__0 s) = s

javaSimple :: QuasiQuoter
javaSimple = QuasiQuoter (quoteJavaSimpleExp "tok_JavaSimple_dummy_11" getJavaSimple ) (quoteJavaSimplePat "tok_JavaSimple_dummy_11" getJavaSimple ) quoteJavaSimpleType quoteJavaSimpleDecs

getClassDeclaration ( Ctr__JavaSimple__1 s) = s

classDeclaration :: QuasiQuoter
classDeclaration = QuasiQuoter (quoteJavaSimpleExp "tok_ClassDeclaration_dummy_10" getClassDeclaration ) (quoteJavaSimplePat "tok_ClassDeclaration_dummy_10" getClassDeclaration ) quoteJavaSimpleType quoteJavaSimpleDecs

getCompilationUnit ( Ctr__JavaSimple__2 s) = s

compilationUnit :: QuasiQuoter
compilationUnit = QuasiQuoter (quoteJavaSimpleExp "tok_CompilationUnit_dummy_9" getCompilationUnit ) (quoteJavaSimplePat "tok_CompilationUnit_dummy_9" getCompilationUnit ) quoteJavaSimpleType quoteJavaSimpleDecs

getCompoundName ( Ctr__JavaSimple__3 s) = s

compoundName :: QuasiQuoter
compoundName = QuasiQuoter (quoteJavaSimpleExp "tok_CompoundName_dummy_8" getCompoundName ) (quoteJavaSimplePat "tok_CompoundName_dummy_8" getCompoundName ) quoteJavaSimpleType quoteJavaSimpleDecs

getField ( Ctr__JavaSimple__4 s) = s

field :: QuasiQuoter
field = QuasiQuoter (quoteJavaSimpleExp "tok_Field_dummy_7" getField ) (quoteJavaSimplePat "tok_Field_dummy_7" getField ) quoteJavaSimpleType quoteJavaSimpleDecs

getFieldList ( Ctr__JavaSimple__5 s) = s

fieldList :: QuasiQuoter
fieldList = QuasiQuoter (quoteJavaSimpleExp "tok_FieldList_dummy_6" getFieldList ) (quoteJavaSimplePat "tok_FieldList_dummy_6" getFieldList ) quoteJavaSimpleType quoteJavaSimpleDecs

getPackage ( Ctr__JavaSimple__6 s) = s

package :: QuasiQuoter
package = QuasiQuoter (quoteJavaSimpleExp "tok_Package_dummy_5" getPackage ) (quoteJavaSimplePat "tok_Package_dummy_5" getPackage ) quoteJavaSimpleType quoteJavaSimpleDecs

getType ( Ctr__JavaSimple__7 s) = s

__type :: QuasiQuoter
__type = QuasiQuoter (quoteJavaSimpleExp "tok_Type_dummy_4" getType ) (quoteJavaSimplePat "tok_Type_dummy_4" getType ) quoteJavaSimpleType quoteJavaSimpleDecs

