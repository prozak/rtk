{-# LANGUAGE TemplateHaskell #-}
module HaskellQQ
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
import HaskellLexer
import HaskellParser

qqPattern = "\\$[A-Za-z_][A-Za-z_0-9]*[^A-Za-z_0-9:]"

qqShortcuts :: M.Map String String

-- A $name metavariable is rewritten to $Type:name using the qqShortcuts
-- table below. The rewrite is purely textual, so it would also fire inside
-- the quoted language's own string literals: write $$name there to escape
-- it and get the literal text $name. Each '$$' pair directly before a
-- metavariable stands for one literal '$' (so $$$x is a literal '$'
-- followed by the metavariable $x). A '$' not followed by an identifier is
-- never rewritten and needs no escape.
replaceAllPatterns1 :: String -> Either String String
replaceAllPatterns1 str = let (pre, match, post) = str =~ qqPattern :: (String, String, String)
                          in if match == ""
                              then Right pre
                              else let varName = init $ tail match
                                       addSym = last match
                                       escCount = length $ takeWhile (== '$') $ reverse pre
                                       keptPre = take (length pre - escCount) pre ++ replicate (div escCount 2) '$'
                                       ruleVariants = catMaybes $ map (\ prefix -> M.lookup prefix qqShortcuts) $ reverse $ inits varName
                                   in if odd escCount
                                       then (\rest -> keptPre ++ ('$' : varName) ++ rest) <$> (replaceAllPatterns1 $ addSym : post)
                                       else case ruleVariants of
                                              [] -> Left $ unlines
                                                      [ "Unknown metavariable $" ++ varName ++ " in quasi-quote:"
                                                      , "no prefix of '" ++ varName ++ "' is a known shortcut. Known shortcuts:"
                                                      , "  " ++ intercalate ", " (M.keys qqShortcuts)
                                                      , "To include the literal text $" ++ varName ++ " in the quoted code"
                                                      , "(e.g. inside a string literal), escape it as $$" ++ varName ++ "." ]
                                              (rule : _) -> (\rest -> keptPre ++ ('$' : rule ++ ":") ++ varName ++ rest) <$> (replaceAllPatterns1 $ addSym : post)

-- Add ' ' at the end, so regex can match variable in the end of the string
replaceAllPatterns :: String -> Either String String
replaceAllPatterns str = init <$> replaceAllPatterns1 (str ++ " ")

qqShortcuts = M.fromList [ ("haskell","Haskell"),("aType","AType"),("aTypeList","ATypeList"),("bType","BType"),("body","Body"),("cName","CName"),("cNameList","CNameList"),("class","Class"),("classList","ClassList"),("con","Con"),("constr","Constr"),("constrs","Constrs"),("context","Context"),("dClass","DClass"),("dClassList","DClassList"),("decl","Decl"),("declList","DeclList"),("decls","Decls"),("deriving","Deriving"),("exp","Exp"),("expI","ExpI"),("export","Export"),("exportsList","ExportsList"),("exportsOpt","ExportsOpt"),("fieldDecl","FieldDecl"),("fieldDeclList","FieldDeclList"),("fixity","Fixity"),("funLhs","FunLhs"),("gTyCon","GTyCon"),("gd","Gd"),("gdRhs","GdRhs"),("genDecl","GenDecl"),("impDecl","ImpDecl"),("impDeclList","ImpDeclList"),("import","Import"),("importList","ImportList"),("modId","ModId"),("modIdList","ModIdList"),("module","Module"),("op","Op"),("ops","Ops"),("optContext","OptContext"),("optDeriving","OptDeriving"),("optExpTypeSignature","OptExpTypeSignature"),("optGdRhs","OptGdRhs"),("optImpSpec","OptImpSpec"),("optInteger","OptInteger"),("optQualified","OptQualified"),("optQualifiedAs","OptQualifiedAs"),("optWhere","OptWhere"),("pat","Pat"),("qOp","QOp"),("qTyCls","QTyCls"),("qTyCon","QTyCon"),("qVar","QVar"),("qVarId","QVarId"),("qVarList","QVarList"),("rhs","Rhs"),("simpleType","SimpleType"),("topDecl","TopDecl"),("topDecls","TopDecls"),("tyCls","TyCls"),("tyCon","TyCon"),("tyVar","TyVar"),("tyVars","TyVars"),("type","Type"),("typeList","TypeList"),("var","Var"),("vars","Vars")]

quoteHaskellExp :: Data.Data a => String -> (Haskell -> a) -> String -> TH.ExpQ
quoteHaskellExp dummy func s = do
  s1 <- either fail return (replaceAllPatterns s)
  ast <- case scanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy) >>= parseHaskell of
           Left err -> fail err
           Right a -> return a
  let expr = func ast
  dataToExpQ (const Nothing `Generics.extQ` antiHaskellExp `Generics.extQ` antiModuleExp `Generics.extQ` antiExportsOptExp `Generics.extQ` antiExportsListExp `Generics.extQ` antiExportExp `Generics.extQ` antiBodyExp `Generics.extQ` antiImpDeclListExp `Generics.extQ` antiImportListExp `Generics.extQ` antiVarExp `Generics.extQ` antiConExp `Generics.extQ` antiRule_13Exp `Generics.extQ` antiQVarIdExp `Generics.extQ` antiQVarExp `Generics.extQ` antiQTyClsExp `Generics.extQ` antiQTyConExp `Generics.extQ` antiCNameExp `Generics.extQ` antiCNameListExp `Generics.extQ` antiQVarListExp `Generics.extQ` antiImportExp `Generics.extQ` antiOptQualifiedExp `Generics.extQ` antiOptQualifiedAsExp `Generics.extQ` antiOptImpSpecExp `Generics.extQ` antiImpDeclExp `Generics.extQ` antiTopDeclsExp `Generics.extQ` antiTopDeclExp `Generics.extQ` antiDeclExp `Generics.extQ` antiOptContextExp `Generics.extQ` antiGenDeclExp `Generics.extQ` antiOptIntegerExp `Generics.extQ` antiOpsExp `Generics.extQ` antiFixityExp `Generics.extQ` antiFunLhsExp `Generics.extQ` antiPatExp `Generics.extQ` antiOptWhereExp `Generics.extQ` antiDeclListExp `Generics.extQ` antiDeclsExp `Generics.extQ` antiRhsExp `Generics.extQ` antiOptGdRhsExp `Generics.extQ` antiGdExp `Generics.extQ` antiOptExpTypeSignatureExp `Generics.extQ` antiExpExp `Generics.extQ` antiExpIExp `Generics.extQ` antiGdRhsExp `Generics.extQ` antiConstrsExp `Generics.extQ` antiConstrExp `Generics.extQ` antiFieldDeclListExp `Generics.extQ` antiFieldDeclExp `Generics.extQ` antiVarsExp `Generics.extQ` antiOptDerivingExp `Generics.extQ` antiDerivingExp `Generics.extQ` antiDClassListExp `Generics.extQ` antiDClassExp `Generics.extQ` antiContextExp `Generics.extQ` antiClassListExp `Generics.extQ` antiClassExp `Generics.extQ` antiTypeExp `Generics.extQ` antiBTypeExp `Generics.extQ` antiATypeExp `Generics.extQ` antiGTyConExp `Generics.extQ` antiTypeListExp `Generics.extQ` antiSimpleTypeExp `Generics.extQ` antiTyVarExp `Generics.extQ` antiTyConExp `Generics.extQ` antiModIdExp `Generics.extQ` antiTyClsExp `Generics.extQ` antiOpExp `Generics.extQ` antiQOpExp) expr
quoteHaskellPat :: Data.Data a => String -> (Haskell -> a) -> String -> TH.PatQ
quoteHaskellPat dummy func s = do
  s1 <- either fail return (replaceAllPatterns s)
  ast <- case scanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy) >>= parseHaskell of
           Left err -> fail err
           Right a -> return a
  let expr = func ast
  dataToPatQ (const Nothing `Generics.extQ` antiHaskellPat `Generics.extQ` antiModulePat `Generics.extQ` antiExportsOptPat `Generics.extQ` antiExportsListPat `Generics.extQ` antiExportPat `Generics.extQ` antiBodyPat `Generics.extQ` antiImpDeclListPat `Generics.extQ` antiImportListPat `Generics.extQ` antiVarPat `Generics.extQ` antiConPat `Generics.extQ` antiRule_13Pat `Generics.extQ` antiQVarIdPat `Generics.extQ` antiQVarPat `Generics.extQ` antiQTyClsPat `Generics.extQ` antiQTyConPat `Generics.extQ` antiCNamePat `Generics.extQ` antiCNameListPat `Generics.extQ` antiQVarListPat `Generics.extQ` antiImportPat `Generics.extQ` antiOptQualifiedPat `Generics.extQ` antiOptQualifiedAsPat `Generics.extQ` antiOptImpSpecPat `Generics.extQ` antiImpDeclPat `Generics.extQ` antiTopDeclsPat `Generics.extQ` antiTopDeclPat `Generics.extQ` antiDeclPat `Generics.extQ` antiOptContextPat `Generics.extQ` antiGenDeclPat `Generics.extQ` antiOptIntegerPat `Generics.extQ` antiOpsPat `Generics.extQ` antiFixityPat `Generics.extQ` antiFunLhsPat `Generics.extQ` antiPatPat `Generics.extQ` antiOptWherePat `Generics.extQ` antiDeclListPat `Generics.extQ` antiDeclsPat `Generics.extQ` antiRhsPat `Generics.extQ` antiOptGdRhsPat `Generics.extQ` antiGdPat `Generics.extQ` antiOptExpTypeSignaturePat `Generics.extQ` antiExpPat `Generics.extQ` antiExpIPat `Generics.extQ` antiGdRhsPat `Generics.extQ` antiConstrsPat `Generics.extQ` antiConstrPat `Generics.extQ` antiFieldDeclListPat `Generics.extQ` antiFieldDeclPat `Generics.extQ` antiVarsPat `Generics.extQ` antiOptDerivingPat `Generics.extQ` antiDerivingPat `Generics.extQ` antiDClassListPat `Generics.extQ` antiDClassPat `Generics.extQ` antiContextPat `Generics.extQ` antiClassListPat `Generics.extQ` antiClassPat `Generics.extQ` antiTypePat `Generics.extQ` antiBTypePat `Generics.extQ` antiATypePat `Generics.extQ` antiGTyConPat `Generics.extQ` antiTypeListPat `Generics.extQ` antiSimpleTypePat `Generics.extQ` antiTyVarPat `Generics.extQ` antiTyConPat `Generics.extQ` antiModIdPat `Generics.extQ` antiTyClsPat `Generics.extQ` antiOpPat `Generics.extQ` antiQOpPat) expr

antiQOpExp :: QOp -> Maybe (TH.Q TH.Exp )
antiQOpExp ( Anti_QOp v) = Just $ TH.varE (TH.mkName v)
antiQOpExp _ = Nothing


antiOpExp :: Op -> Maybe (TH.Q TH.Exp )
antiOpExp ( Anti_Op v) = Just $ TH.varE (TH.mkName v)
antiOpExp _ = Nothing


antiTyClsExp :: TyCls -> Maybe (TH.Q TH.Exp )
antiTyClsExp ( Anti_TyCls v) = Just $ TH.varE (TH.mkName v)
antiTyClsExp _ = Nothing


antiModIdExp :: ModId -> Maybe (TH.Q TH.Exp )
antiModIdExp ( Anti_ModId v) = Just $ TH.varE (TH.mkName v)
antiModIdExp _ = Nothing


antiTyConExp :: TyCon -> Maybe (TH.Q TH.Exp )
antiTyConExp ( Anti_TyCon v) = Just $ TH.varE (TH.mkName v)
antiTyConExp _ = Nothing


antiTyVarExp :: [ TyVar ] -> Maybe (TH.Q TH.Exp)
antiTyVarExp ((Anti_TyVar v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiHaskellExp `Generics.extQ` antiModuleExp `Generics.extQ` antiExportsOptExp `Generics.extQ` antiExportsListExp `Generics.extQ` antiExportExp `Generics.extQ` antiBodyExp `Generics.extQ` antiImpDeclListExp `Generics.extQ` antiImportListExp `Generics.extQ` antiVarExp `Generics.extQ` antiConExp `Generics.extQ` antiRule_13Exp `Generics.extQ` antiQVarIdExp `Generics.extQ` antiQVarExp `Generics.extQ` antiQTyClsExp `Generics.extQ` antiQTyConExp `Generics.extQ` antiCNameExp `Generics.extQ` antiCNameListExp `Generics.extQ` antiQVarListExp `Generics.extQ` antiImportExp `Generics.extQ` antiOptQualifiedExp `Generics.extQ` antiOptQualifiedAsExp `Generics.extQ` antiOptImpSpecExp `Generics.extQ` antiImpDeclExp `Generics.extQ` antiTopDeclsExp `Generics.extQ` antiTopDeclExp `Generics.extQ` antiDeclExp `Generics.extQ` antiOptContextExp `Generics.extQ` antiGenDeclExp `Generics.extQ` antiOptIntegerExp `Generics.extQ` antiOpsExp `Generics.extQ` antiFixityExp `Generics.extQ` antiFunLhsExp `Generics.extQ` antiPatExp `Generics.extQ` antiOptWhereExp `Generics.extQ` antiDeclListExp `Generics.extQ` antiDeclsExp `Generics.extQ` antiRhsExp `Generics.extQ` antiOptGdRhsExp `Generics.extQ` antiGdExp `Generics.extQ` antiOptExpTypeSignatureExp `Generics.extQ` antiExpExp `Generics.extQ` antiExpIExp `Generics.extQ` antiGdRhsExp `Generics.extQ` antiConstrsExp `Generics.extQ` antiConstrExp `Generics.extQ` antiFieldDeclListExp `Generics.extQ` antiFieldDeclExp `Generics.extQ` antiVarsExp `Generics.extQ` antiOptDerivingExp `Generics.extQ` antiDerivingExp `Generics.extQ` antiDClassListExp `Generics.extQ` antiDClassExp `Generics.extQ` antiContextExp `Generics.extQ` antiClassListExp `Generics.extQ` antiClassExp `Generics.extQ` antiTypeExp `Generics.extQ` antiBTypeExp `Generics.extQ` antiATypeExp `Generics.extQ` antiGTyConExp `Generics.extQ` antiTypeListExp `Generics.extQ` antiSimpleTypeExp `Generics.extQ` antiTyVarExp `Generics.extQ` antiTyConExp `Generics.extQ` antiModIdExp `Generics.extQ` antiTyClsExp `Generics.extQ` antiOpExp `Generics.extQ` antiQOpExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiTyVarExp _ = Nothing


antiSimpleTypeExp :: SimpleType -> Maybe (TH.Q TH.Exp )
antiSimpleTypeExp ( Anti_SimpleType v) = Just $ TH.varE (TH.mkName v)
antiSimpleTypeExp _ = Nothing


antiTypeListExp :: TypeList -> Maybe (TH.Q TH.Exp )
antiTypeListExp ( Anti_TypeList v) = Just $ TH.varE (TH.mkName v)
antiTypeListExp _ = Nothing


antiGTyConExp :: GTyCon -> Maybe (TH.Q TH.Exp )
antiGTyConExp ( Anti_GTyCon v) = Just $ TH.varE (TH.mkName v)
antiGTyConExp _ = Nothing


antiATypeExp :: [ AType ] -> Maybe (TH.Q TH.Exp)
antiATypeExp ((Anti_AType v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiHaskellExp `Generics.extQ` antiModuleExp `Generics.extQ` antiExportsOptExp `Generics.extQ` antiExportsListExp `Generics.extQ` antiExportExp `Generics.extQ` antiBodyExp `Generics.extQ` antiImpDeclListExp `Generics.extQ` antiImportListExp `Generics.extQ` antiVarExp `Generics.extQ` antiConExp `Generics.extQ` antiRule_13Exp `Generics.extQ` antiQVarIdExp `Generics.extQ` antiQVarExp `Generics.extQ` antiQTyClsExp `Generics.extQ` antiQTyConExp `Generics.extQ` antiCNameExp `Generics.extQ` antiCNameListExp `Generics.extQ` antiQVarListExp `Generics.extQ` antiImportExp `Generics.extQ` antiOptQualifiedExp `Generics.extQ` antiOptQualifiedAsExp `Generics.extQ` antiOptImpSpecExp `Generics.extQ` antiImpDeclExp `Generics.extQ` antiTopDeclsExp `Generics.extQ` antiTopDeclExp `Generics.extQ` antiDeclExp `Generics.extQ` antiOptContextExp `Generics.extQ` antiGenDeclExp `Generics.extQ` antiOptIntegerExp `Generics.extQ` antiOpsExp `Generics.extQ` antiFixityExp `Generics.extQ` antiFunLhsExp `Generics.extQ` antiPatExp `Generics.extQ` antiOptWhereExp `Generics.extQ` antiDeclListExp `Generics.extQ` antiDeclsExp `Generics.extQ` antiRhsExp `Generics.extQ` antiOptGdRhsExp `Generics.extQ` antiGdExp `Generics.extQ` antiOptExpTypeSignatureExp `Generics.extQ` antiExpExp `Generics.extQ` antiExpIExp `Generics.extQ` antiGdRhsExp `Generics.extQ` antiConstrsExp `Generics.extQ` antiConstrExp `Generics.extQ` antiFieldDeclListExp `Generics.extQ` antiFieldDeclExp `Generics.extQ` antiVarsExp `Generics.extQ` antiOptDerivingExp `Generics.extQ` antiDerivingExp `Generics.extQ` antiDClassListExp `Generics.extQ` antiDClassExp `Generics.extQ` antiContextExp `Generics.extQ` antiClassListExp `Generics.extQ` antiClassExp `Generics.extQ` antiTypeExp `Generics.extQ` antiBTypeExp `Generics.extQ` antiATypeExp `Generics.extQ` antiGTyConExp `Generics.extQ` antiTypeListExp `Generics.extQ` antiSimpleTypeExp `Generics.extQ` antiTyVarExp `Generics.extQ` antiTyConExp `Generics.extQ` antiModIdExp `Generics.extQ` antiTyClsExp `Generics.extQ` antiOpExp `Generics.extQ` antiQOpExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiATypeExp _ = Nothing


antiBTypeExp :: BType -> Maybe (TH.Q TH.Exp )
antiBTypeExp ( Anti_BType v) = Just $ TH.varE (TH.mkName v)
antiBTypeExp _ = Nothing


antiTypeExp :: Type -> Maybe (TH.Q TH.Exp )
antiTypeExp ( Anti_Type v) = Just $ TH.varE (TH.mkName v)
antiTypeExp _ = Nothing


antiClassExp :: Class -> Maybe (TH.Q TH.Exp )
antiClassExp ( Anti_Class v) = Just $ TH.varE (TH.mkName v)
antiClassExp _ = Nothing


antiClassListExp :: ClassList -> Maybe (TH.Q TH.Exp )
antiClassListExp ( Anti_ClassList v) = Just $ TH.varE (TH.mkName v)
antiClassListExp _ = Nothing


antiContextExp :: Context -> Maybe (TH.Q TH.Exp )
antiContextExp ( Anti_Context v) = Just $ TH.varE (TH.mkName v)
antiContextExp _ = Nothing


antiDClassExp :: DClass -> Maybe (TH.Q TH.Exp )
antiDClassExp ( Anti_DClass v) = Just $ TH.varE (TH.mkName v)
antiDClassExp _ = Nothing


antiDClassListExp :: DClassList -> Maybe (TH.Q TH.Exp )
antiDClassListExp ( Anti_DClassList v) = Just $ TH.varE (TH.mkName v)
antiDClassListExp _ = Nothing


antiDerivingExp :: Deriving -> Maybe (TH.Q TH.Exp )
antiDerivingExp ( Anti_Deriving v) = Just $ TH.varE (TH.mkName v)
antiDerivingExp _ = Nothing


antiOptDerivingExp :: OptDeriving -> Maybe (TH.Q TH.Exp )
antiOptDerivingExp ( Anti_OptDeriving v) = Just $ TH.varE (TH.mkName v)
antiOptDerivingExp _ = Nothing


antiVarsExp :: Vars -> Maybe (TH.Q TH.Exp )
antiVarsExp ( Anti_Vars v) = Just $ TH.varE (TH.mkName v)
antiVarsExp _ = Nothing


antiFieldDeclExp :: FieldDecl -> Maybe (TH.Q TH.Exp )
antiFieldDeclExp ( Anti_FieldDecl v) = Just $ TH.varE (TH.mkName v)
antiFieldDeclExp _ = Nothing


antiFieldDeclListExp :: FieldDeclList -> Maybe (TH.Q TH.Exp )
antiFieldDeclListExp ( Anti_FieldDeclList v) = Just $ TH.varE (TH.mkName v)
antiFieldDeclListExp _ = Nothing


antiConstrExp :: Constr -> Maybe (TH.Q TH.Exp )
antiConstrExp ( Anti_Constr v) = Just $ TH.varE (TH.mkName v)
antiConstrExp _ = Nothing


antiConstrsExp :: Constrs -> Maybe (TH.Q TH.Exp )
antiConstrsExp ( Anti_Constrs v) = Just $ TH.varE (TH.mkName v)
antiConstrsExp _ = Nothing


antiGdRhsExp :: GdRhs -> Maybe (TH.Q TH.Exp )
antiGdRhsExp ( Anti_GdRhs v) = Just $ TH.varE (TH.mkName v)
antiGdRhsExp _ = Nothing


antiExpIExp :: ExpI -> Maybe (TH.Q TH.Exp )
antiExpIExp ( Anti_ExpI v) = Just $ TH.varE (TH.mkName v)
antiExpIExp _ = Nothing


antiExpExp :: Exp -> Maybe (TH.Q TH.Exp )
antiExpExp ( Anti_Exp v) = Just $ TH.varE (TH.mkName v)
antiExpExp _ = Nothing


antiOptExpTypeSignatureExp :: OptExpTypeSignature -> Maybe (TH.Q TH.Exp )
antiOptExpTypeSignatureExp ( Anti_OptExpTypeSignature v) = Just $ TH.varE (TH.mkName v)
antiOptExpTypeSignatureExp _ = Nothing


antiGdExp :: Gd -> Maybe (TH.Q TH.Exp )
antiGdExp ( Anti_Gd v) = Just $ TH.varE (TH.mkName v)
antiGdExp _ = Nothing


antiOptGdRhsExp :: OptGdRhs -> Maybe (TH.Q TH.Exp )
antiOptGdRhsExp ( Anti_OptGdRhs v) = Just $ TH.varE (TH.mkName v)
antiOptGdRhsExp _ = Nothing


antiRhsExp :: Rhs -> Maybe (TH.Q TH.Exp )
antiRhsExp ( Anti_Rhs v) = Just $ TH.varE (TH.mkName v)
antiRhsExp _ = Nothing


antiDeclsExp :: Decls -> Maybe (TH.Q TH.Exp )
antiDeclsExp ( Anti_Decls v) = Just $ TH.varE (TH.mkName v)
antiDeclsExp _ = Nothing


antiDeclListExp :: DeclList -> Maybe (TH.Q TH.Exp )
antiDeclListExp ( Anti_DeclList v) = Just $ TH.varE (TH.mkName v)
antiDeclListExp _ = Nothing


antiOptWhereExp :: OptWhere -> Maybe (TH.Q TH.Exp )
antiOptWhereExp ( Anti_OptWhere v) = Just $ TH.varE (TH.mkName v)
antiOptWhereExp _ = Nothing


antiPatExp :: Pat -> Maybe (TH.Q TH.Exp )
antiPatExp ( Anti_Pat v) = Just $ TH.varE (TH.mkName v)
antiPatExp _ = Nothing


antiFunLhsExp :: FunLhs -> Maybe (TH.Q TH.Exp )
antiFunLhsExp ( Anti_FunLhs v) = Just $ TH.varE (TH.mkName v)
antiFunLhsExp _ = Nothing


antiFixityExp :: Fixity -> Maybe (TH.Q TH.Exp )
antiFixityExp ( Anti_Fixity v) = Just $ TH.varE (TH.mkName v)
antiFixityExp _ = Nothing


antiOpsExp :: Ops -> Maybe (TH.Q TH.Exp )
antiOpsExp ( Anti_Ops v) = Just $ TH.varE (TH.mkName v)
antiOpsExp _ = Nothing


antiOptIntegerExp :: OptInteger -> Maybe (TH.Q TH.Exp )
antiOptIntegerExp ( Anti_OptInteger v) = Just $ TH.varE (TH.mkName v)
antiOptIntegerExp _ = Nothing


antiGenDeclExp :: GenDecl -> Maybe (TH.Q TH.Exp )
antiGenDeclExp ( Anti_GenDecl v) = Just $ TH.varE (TH.mkName v)
antiGenDeclExp _ = Nothing


antiOptContextExp :: OptContext -> Maybe (TH.Q TH.Exp )
antiOptContextExp ( Anti_OptContext v) = Just $ TH.varE (TH.mkName v)
antiOptContextExp _ = Nothing


antiDeclExp :: Decl -> Maybe (TH.Q TH.Exp )
antiDeclExp ( Anti_Decl v) = Just $ TH.varE (TH.mkName v)
antiDeclExp _ = Nothing


antiTopDeclExp :: TopDecl -> Maybe (TH.Q TH.Exp )
antiTopDeclExp ( Anti_TopDecl v) = Just $ TH.varE (TH.mkName v)
antiTopDeclExp _ = Nothing


antiTopDeclsExp :: TopDecls -> Maybe (TH.Q TH.Exp )
antiTopDeclsExp ( Anti_TopDecls v) = Just $ TH.varE (TH.mkName v)
antiTopDeclsExp _ = Nothing


antiImpDeclExp :: ImpDecl -> Maybe (TH.Q TH.Exp )
antiImpDeclExp ( Anti_ImpDecl v) = Just $ TH.varE (TH.mkName v)
antiImpDeclExp _ = Nothing


antiOptImpSpecExp :: OptImpSpec -> Maybe (TH.Q TH.Exp )
antiOptImpSpecExp ( Anti_OptImpSpec v) = Just $ TH.varE (TH.mkName v)
antiOptImpSpecExp _ = Nothing


antiOptQualifiedAsExp :: OptQualifiedAs -> Maybe (TH.Q TH.Exp )
antiOptQualifiedAsExp ( Anti_OptQualifiedAs v) = Just $ TH.varE (TH.mkName v)
antiOptQualifiedAsExp _ = Nothing


antiOptQualifiedExp :: OptQualified -> Maybe (TH.Q TH.Exp )
antiOptQualifiedExp ( Anti_OptQualified v) = Just $ TH.varE (TH.mkName v)
antiOptQualifiedExp _ = Nothing


antiImportExp :: Import -> Maybe (TH.Q TH.Exp )
antiImportExp ( Anti_Import v) = Just $ TH.varE (TH.mkName v)
antiImportExp _ = Nothing


antiQVarListExp :: QVarList -> Maybe (TH.Q TH.Exp )
antiQVarListExp ( Anti_QVarList v) = Just $ TH.varE (TH.mkName v)
antiQVarListExp _ = Nothing


antiCNameListExp :: CNameList -> Maybe (TH.Q TH.Exp )
antiCNameListExp ( Anti_CNameList v) = Just $ TH.varE (TH.mkName v)
antiCNameListExp _ = Nothing


antiCNameExp :: CName -> Maybe (TH.Q TH.Exp )
antiCNameExp ( Anti_CName v) = Just $ TH.varE (TH.mkName v)
antiCNameExp _ = Nothing


antiQTyConExp :: QTyCon -> Maybe (TH.Q TH.Exp )
antiQTyConExp ( Anti_QTyCon v) = Just $ TH.varE (TH.mkName v)
antiQTyConExp _ = Nothing


antiQTyClsExp :: QTyCls -> Maybe (TH.Q TH.Exp )
antiQTyClsExp ( Anti_QTyCls v) = Just $ TH.varE (TH.mkName v)
antiQTyClsExp _ = Nothing


antiQVarExp :: QVar -> Maybe (TH.Q TH.Exp )
antiQVarExp ( Anti_QVar v) = Just $ TH.varE (TH.mkName v)
antiQVarExp _ = Nothing


antiQVarIdExp :: QVarId -> Maybe (TH.Q TH.Exp )
antiQVarIdExp ( Anti_QVarId v) = Just $ TH.varE (TH.mkName v)
antiQVarIdExp _ = Nothing


antiRule_13Exp :: [ Rule_13 ] -> Maybe (TH.Q TH.Exp)
antiRule_13Exp ((Anti_Rule_13 v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiHaskellExp `Generics.extQ` antiModuleExp `Generics.extQ` antiExportsOptExp `Generics.extQ` antiExportsListExp `Generics.extQ` antiExportExp `Generics.extQ` antiBodyExp `Generics.extQ` antiImpDeclListExp `Generics.extQ` antiImportListExp `Generics.extQ` antiVarExp `Generics.extQ` antiConExp `Generics.extQ` antiRule_13Exp `Generics.extQ` antiQVarIdExp `Generics.extQ` antiQVarExp `Generics.extQ` antiQTyClsExp `Generics.extQ` antiQTyConExp `Generics.extQ` antiCNameExp `Generics.extQ` antiCNameListExp `Generics.extQ` antiQVarListExp `Generics.extQ` antiImportExp `Generics.extQ` antiOptQualifiedExp `Generics.extQ` antiOptQualifiedAsExp `Generics.extQ` antiOptImpSpecExp `Generics.extQ` antiImpDeclExp `Generics.extQ` antiTopDeclsExp `Generics.extQ` antiTopDeclExp `Generics.extQ` antiDeclExp `Generics.extQ` antiOptContextExp `Generics.extQ` antiGenDeclExp `Generics.extQ` antiOptIntegerExp `Generics.extQ` antiOpsExp `Generics.extQ` antiFixityExp `Generics.extQ` antiFunLhsExp `Generics.extQ` antiPatExp `Generics.extQ` antiOptWhereExp `Generics.extQ` antiDeclListExp `Generics.extQ` antiDeclsExp `Generics.extQ` antiRhsExp `Generics.extQ` antiOptGdRhsExp `Generics.extQ` antiGdExp `Generics.extQ` antiOptExpTypeSignatureExp `Generics.extQ` antiExpExp `Generics.extQ` antiExpIExp `Generics.extQ` antiGdRhsExp `Generics.extQ` antiConstrsExp `Generics.extQ` antiConstrExp `Generics.extQ` antiFieldDeclListExp `Generics.extQ` antiFieldDeclExp `Generics.extQ` antiVarsExp `Generics.extQ` antiOptDerivingExp `Generics.extQ` antiDerivingExp `Generics.extQ` antiDClassListExp `Generics.extQ` antiDClassExp `Generics.extQ` antiContextExp `Generics.extQ` antiClassListExp `Generics.extQ` antiClassExp `Generics.extQ` antiTypeExp `Generics.extQ` antiBTypeExp `Generics.extQ` antiATypeExp `Generics.extQ` antiGTyConExp `Generics.extQ` antiTypeListExp `Generics.extQ` antiSimpleTypeExp `Generics.extQ` antiTyVarExp `Generics.extQ` antiTyConExp `Generics.extQ` antiModIdExp `Generics.extQ` antiTyClsExp `Generics.extQ` antiOpExp `Generics.extQ` antiQOpExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiRule_13Exp _ = Nothing


antiConExp :: Con -> Maybe (TH.Q TH.Exp )
antiConExp ( Anti_Con v) = Just $ TH.varE (TH.mkName v)
antiConExp _ = Nothing


antiVarExp :: Var -> Maybe (TH.Q TH.Exp )
antiVarExp ( Anti_Var v) = Just $ TH.varE (TH.mkName v)
antiVarExp _ = Nothing


antiImportListExp :: ImportList -> Maybe (TH.Q TH.Exp )
antiImportListExp ( Anti_ImportList v) = Just $ TH.varE (TH.mkName v)
antiImportListExp _ = Nothing


antiImpDeclListExp :: ImpDeclList -> Maybe (TH.Q TH.Exp )
antiImpDeclListExp ( Anti_ImpDeclList v) = Just $ TH.varE (TH.mkName v)
antiImpDeclListExp _ = Nothing


antiBodyExp :: Body -> Maybe (TH.Q TH.Exp )
antiBodyExp ( Anti_Body v) = Just $ TH.varE (TH.mkName v)
antiBodyExp _ = Nothing


antiExportExp :: Export -> Maybe (TH.Q TH.Exp )
antiExportExp ( Anti_Export v) = Just $ TH.varE (TH.mkName v)
antiExportExp _ = Nothing


antiExportsListExp :: ExportsList -> Maybe (TH.Q TH.Exp )
antiExportsListExp ( Anti_ExportsList v) = Just $ TH.varE (TH.mkName v)
antiExportsListExp _ = Nothing


antiExportsOptExp :: ExportsOpt -> Maybe (TH.Q TH.Exp )
antiExportsOptExp ( Anti_ExportsOpt v) = Just $ TH.varE (TH.mkName v)
antiExportsOptExp _ = Nothing


antiModuleExp :: Module -> Maybe (TH.Q TH.Exp )
antiModuleExp ( Anti_Module v) = Just $ TH.varE (TH.mkName v)
antiModuleExp _ = Nothing


antiHaskellExp :: Haskell -> Maybe (TH.Q TH.Exp )
antiHaskellExp ( Anti_Haskell v) = Just $ TH.varE (TH.mkName v)
antiHaskellExp _ = Nothing



antiQOpPat :: QOp -> Maybe (TH.Q TH.Pat )
antiQOpPat ( Anti_QOp v) = Just $ TH.varP (TH.mkName v)
antiQOpPat _ = Nothing


antiOpPat :: Op -> Maybe (TH.Q TH.Pat )
antiOpPat ( Anti_Op v) = Just $ TH.varP (TH.mkName v)
antiOpPat _ = Nothing


antiTyClsPat :: TyCls -> Maybe (TH.Q TH.Pat )
antiTyClsPat ( Anti_TyCls v) = Just $ TH.varP (TH.mkName v)
antiTyClsPat _ = Nothing


antiModIdPat :: ModId -> Maybe (TH.Q TH.Pat )
antiModIdPat ( Anti_ModId v) = Just $ TH.varP (TH.mkName v)
antiModIdPat _ = Nothing


antiTyConPat :: TyCon -> Maybe (TH.Q TH.Pat )
antiTyConPat ( Anti_TyCon v) = Just $ TH.varP (TH.mkName v)
antiTyConPat _ = Nothing


antiTyVarPat :: [ TyVar ] -> Maybe (TH.Q TH.Pat)
antiTyVarPat [Anti_TyVar v] = Just $ TH.varP (TH.mkName v)
antiTyVarPat _ = Nothing


antiSimpleTypePat :: SimpleType -> Maybe (TH.Q TH.Pat )
antiSimpleTypePat ( Anti_SimpleType v) = Just $ TH.varP (TH.mkName v)
antiSimpleTypePat _ = Nothing


antiTypeListPat :: TypeList -> Maybe (TH.Q TH.Pat )
antiTypeListPat ( Anti_TypeList v) = Just $ TH.varP (TH.mkName v)
antiTypeListPat _ = Nothing


antiGTyConPat :: GTyCon -> Maybe (TH.Q TH.Pat )
antiGTyConPat ( Anti_GTyCon v) = Just $ TH.varP (TH.mkName v)
antiGTyConPat _ = Nothing


antiATypePat :: [ AType ] -> Maybe (TH.Q TH.Pat)
antiATypePat [Anti_AType v] = Just $ TH.varP (TH.mkName v)
antiATypePat _ = Nothing


antiBTypePat :: BType -> Maybe (TH.Q TH.Pat )
antiBTypePat ( Anti_BType v) = Just $ TH.varP (TH.mkName v)
antiBTypePat _ = Nothing


antiTypePat :: Type -> Maybe (TH.Q TH.Pat )
antiTypePat ( Anti_Type v) = Just $ TH.varP (TH.mkName v)
antiTypePat _ = Nothing


antiClassPat :: Class -> Maybe (TH.Q TH.Pat )
antiClassPat ( Anti_Class v) = Just $ TH.varP (TH.mkName v)
antiClassPat _ = Nothing


antiClassListPat :: ClassList -> Maybe (TH.Q TH.Pat )
antiClassListPat ( Anti_ClassList v) = Just $ TH.varP (TH.mkName v)
antiClassListPat _ = Nothing


antiContextPat :: Context -> Maybe (TH.Q TH.Pat )
antiContextPat ( Anti_Context v) = Just $ TH.varP (TH.mkName v)
antiContextPat _ = Nothing


antiDClassPat :: DClass -> Maybe (TH.Q TH.Pat )
antiDClassPat ( Anti_DClass v) = Just $ TH.varP (TH.mkName v)
antiDClassPat _ = Nothing


antiDClassListPat :: DClassList -> Maybe (TH.Q TH.Pat )
antiDClassListPat ( Anti_DClassList v) = Just $ TH.varP (TH.mkName v)
antiDClassListPat _ = Nothing


antiDerivingPat :: Deriving -> Maybe (TH.Q TH.Pat )
antiDerivingPat ( Anti_Deriving v) = Just $ TH.varP (TH.mkName v)
antiDerivingPat _ = Nothing


antiOptDerivingPat :: OptDeriving -> Maybe (TH.Q TH.Pat )
antiOptDerivingPat ( Anti_OptDeriving v) = Just $ TH.varP (TH.mkName v)
antiOptDerivingPat _ = Nothing


antiVarsPat :: Vars -> Maybe (TH.Q TH.Pat )
antiVarsPat ( Anti_Vars v) = Just $ TH.varP (TH.mkName v)
antiVarsPat _ = Nothing


antiFieldDeclPat :: FieldDecl -> Maybe (TH.Q TH.Pat )
antiFieldDeclPat ( Anti_FieldDecl v) = Just $ TH.varP (TH.mkName v)
antiFieldDeclPat _ = Nothing


antiFieldDeclListPat :: FieldDeclList -> Maybe (TH.Q TH.Pat )
antiFieldDeclListPat ( Anti_FieldDeclList v) = Just $ TH.varP (TH.mkName v)
antiFieldDeclListPat _ = Nothing


antiConstrPat :: Constr -> Maybe (TH.Q TH.Pat )
antiConstrPat ( Anti_Constr v) = Just $ TH.varP (TH.mkName v)
antiConstrPat _ = Nothing


antiConstrsPat :: Constrs -> Maybe (TH.Q TH.Pat )
antiConstrsPat ( Anti_Constrs v) = Just $ TH.varP (TH.mkName v)
antiConstrsPat _ = Nothing


antiGdRhsPat :: GdRhs -> Maybe (TH.Q TH.Pat )
antiGdRhsPat ( Anti_GdRhs v) = Just $ TH.varP (TH.mkName v)
antiGdRhsPat _ = Nothing


antiExpIPat :: ExpI -> Maybe (TH.Q TH.Pat )
antiExpIPat ( Anti_ExpI v) = Just $ TH.varP (TH.mkName v)
antiExpIPat _ = Nothing


antiExpPat :: Exp -> Maybe (TH.Q TH.Pat )
antiExpPat ( Anti_Exp v) = Just $ TH.varP (TH.mkName v)
antiExpPat _ = Nothing


antiOptExpTypeSignaturePat :: OptExpTypeSignature -> Maybe (TH.Q TH.Pat )
antiOptExpTypeSignaturePat ( Anti_OptExpTypeSignature v) = Just $ TH.varP (TH.mkName v)
antiOptExpTypeSignaturePat _ = Nothing


antiGdPat :: Gd -> Maybe (TH.Q TH.Pat )
antiGdPat ( Anti_Gd v) = Just $ TH.varP (TH.mkName v)
antiGdPat _ = Nothing


antiOptGdRhsPat :: OptGdRhs -> Maybe (TH.Q TH.Pat )
antiOptGdRhsPat ( Anti_OptGdRhs v) = Just $ TH.varP (TH.mkName v)
antiOptGdRhsPat _ = Nothing


antiRhsPat :: Rhs -> Maybe (TH.Q TH.Pat )
antiRhsPat ( Anti_Rhs v) = Just $ TH.varP (TH.mkName v)
antiRhsPat _ = Nothing


antiDeclsPat :: Decls -> Maybe (TH.Q TH.Pat )
antiDeclsPat ( Anti_Decls v) = Just $ TH.varP (TH.mkName v)
antiDeclsPat _ = Nothing


antiDeclListPat :: DeclList -> Maybe (TH.Q TH.Pat )
antiDeclListPat ( Anti_DeclList v) = Just $ TH.varP (TH.mkName v)
antiDeclListPat _ = Nothing


antiOptWherePat :: OptWhere -> Maybe (TH.Q TH.Pat )
antiOptWherePat ( Anti_OptWhere v) = Just $ TH.varP (TH.mkName v)
antiOptWherePat _ = Nothing


antiPatPat :: Pat -> Maybe (TH.Q TH.Pat )
antiPatPat ( Anti_Pat v) = Just $ TH.varP (TH.mkName v)
antiPatPat _ = Nothing


antiFunLhsPat :: FunLhs -> Maybe (TH.Q TH.Pat )
antiFunLhsPat ( Anti_FunLhs v) = Just $ TH.varP (TH.mkName v)
antiFunLhsPat _ = Nothing


antiFixityPat :: Fixity -> Maybe (TH.Q TH.Pat )
antiFixityPat ( Anti_Fixity v) = Just $ TH.varP (TH.mkName v)
antiFixityPat _ = Nothing


antiOpsPat :: Ops -> Maybe (TH.Q TH.Pat )
antiOpsPat ( Anti_Ops v) = Just $ TH.varP (TH.mkName v)
antiOpsPat _ = Nothing


antiOptIntegerPat :: OptInteger -> Maybe (TH.Q TH.Pat )
antiOptIntegerPat ( Anti_OptInteger v) = Just $ TH.varP (TH.mkName v)
antiOptIntegerPat _ = Nothing


antiGenDeclPat :: GenDecl -> Maybe (TH.Q TH.Pat )
antiGenDeclPat ( Anti_GenDecl v) = Just $ TH.varP (TH.mkName v)
antiGenDeclPat _ = Nothing


antiOptContextPat :: OptContext -> Maybe (TH.Q TH.Pat )
antiOptContextPat ( Anti_OptContext v) = Just $ TH.varP (TH.mkName v)
antiOptContextPat _ = Nothing


antiDeclPat :: Decl -> Maybe (TH.Q TH.Pat )
antiDeclPat ( Anti_Decl v) = Just $ TH.varP (TH.mkName v)
antiDeclPat _ = Nothing


antiTopDeclPat :: TopDecl -> Maybe (TH.Q TH.Pat )
antiTopDeclPat ( Anti_TopDecl v) = Just $ TH.varP (TH.mkName v)
antiTopDeclPat _ = Nothing


antiTopDeclsPat :: TopDecls -> Maybe (TH.Q TH.Pat )
antiTopDeclsPat ( Anti_TopDecls v) = Just $ TH.varP (TH.mkName v)
antiTopDeclsPat _ = Nothing


antiImpDeclPat :: ImpDecl -> Maybe (TH.Q TH.Pat )
antiImpDeclPat ( Anti_ImpDecl v) = Just $ TH.varP (TH.mkName v)
antiImpDeclPat _ = Nothing


antiOptImpSpecPat :: OptImpSpec -> Maybe (TH.Q TH.Pat )
antiOptImpSpecPat ( Anti_OptImpSpec v) = Just $ TH.varP (TH.mkName v)
antiOptImpSpecPat _ = Nothing


antiOptQualifiedAsPat :: OptQualifiedAs -> Maybe (TH.Q TH.Pat )
antiOptQualifiedAsPat ( Anti_OptQualifiedAs v) = Just $ TH.varP (TH.mkName v)
antiOptQualifiedAsPat _ = Nothing


antiOptQualifiedPat :: OptQualified -> Maybe (TH.Q TH.Pat )
antiOptQualifiedPat ( Anti_OptQualified v) = Just $ TH.varP (TH.mkName v)
antiOptQualifiedPat _ = Nothing


antiImportPat :: Import -> Maybe (TH.Q TH.Pat )
antiImportPat ( Anti_Import v) = Just $ TH.varP (TH.mkName v)
antiImportPat _ = Nothing


antiQVarListPat :: QVarList -> Maybe (TH.Q TH.Pat )
antiQVarListPat ( Anti_QVarList v) = Just $ TH.varP (TH.mkName v)
antiQVarListPat _ = Nothing


antiCNameListPat :: CNameList -> Maybe (TH.Q TH.Pat )
antiCNameListPat ( Anti_CNameList v) = Just $ TH.varP (TH.mkName v)
antiCNameListPat _ = Nothing


antiCNamePat :: CName -> Maybe (TH.Q TH.Pat )
antiCNamePat ( Anti_CName v) = Just $ TH.varP (TH.mkName v)
antiCNamePat _ = Nothing


antiQTyConPat :: QTyCon -> Maybe (TH.Q TH.Pat )
antiQTyConPat ( Anti_QTyCon v) = Just $ TH.varP (TH.mkName v)
antiQTyConPat _ = Nothing


antiQTyClsPat :: QTyCls -> Maybe (TH.Q TH.Pat )
antiQTyClsPat ( Anti_QTyCls v) = Just $ TH.varP (TH.mkName v)
antiQTyClsPat _ = Nothing


antiQVarPat :: QVar -> Maybe (TH.Q TH.Pat )
antiQVarPat ( Anti_QVar v) = Just $ TH.varP (TH.mkName v)
antiQVarPat _ = Nothing


antiQVarIdPat :: QVarId -> Maybe (TH.Q TH.Pat )
antiQVarIdPat ( Anti_QVarId v) = Just $ TH.varP (TH.mkName v)
antiQVarIdPat _ = Nothing


antiRule_13Pat :: [ Rule_13 ] -> Maybe (TH.Q TH.Pat)
antiRule_13Pat [Anti_Rule_13 v] = Just $ TH.varP (TH.mkName v)
antiRule_13Pat _ = Nothing


antiConPat :: Con -> Maybe (TH.Q TH.Pat )
antiConPat ( Anti_Con v) = Just $ TH.varP (TH.mkName v)
antiConPat _ = Nothing


antiVarPat :: Var -> Maybe (TH.Q TH.Pat )
antiVarPat ( Anti_Var v) = Just $ TH.varP (TH.mkName v)
antiVarPat _ = Nothing


antiImportListPat :: ImportList -> Maybe (TH.Q TH.Pat )
antiImportListPat ( Anti_ImportList v) = Just $ TH.varP (TH.mkName v)
antiImportListPat _ = Nothing


antiImpDeclListPat :: ImpDeclList -> Maybe (TH.Q TH.Pat )
antiImpDeclListPat ( Anti_ImpDeclList v) = Just $ TH.varP (TH.mkName v)
antiImpDeclListPat _ = Nothing


antiBodyPat :: Body -> Maybe (TH.Q TH.Pat )
antiBodyPat ( Anti_Body v) = Just $ TH.varP (TH.mkName v)
antiBodyPat _ = Nothing


antiExportPat :: Export -> Maybe (TH.Q TH.Pat )
antiExportPat ( Anti_Export v) = Just $ TH.varP (TH.mkName v)
antiExportPat _ = Nothing


antiExportsListPat :: ExportsList -> Maybe (TH.Q TH.Pat )
antiExportsListPat ( Anti_ExportsList v) = Just $ TH.varP (TH.mkName v)
antiExportsListPat _ = Nothing


antiExportsOptPat :: ExportsOpt -> Maybe (TH.Q TH.Pat )
antiExportsOptPat ( Anti_ExportsOpt v) = Just $ TH.varP (TH.mkName v)
antiExportsOptPat _ = Nothing


antiModulePat :: Module -> Maybe (TH.Q TH.Pat )
antiModulePat ( Anti_Module v) = Just $ TH.varP (TH.mkName v)
antiModulePat _ = Nothing


antiHaskellPat :: Haskell -> Maybe (TH.Q TH.Pat )
antiHaskellPat ( Anti_Haskell v) = Just $ TH.varP (TH.mkName v)
antiHaskellPat _ = Nothing



quoteHaskellType s = return TH.ListT
quoteHaskellDecs s = return []

getHaskell ( Ctr__Haskell__0 s) = s

haskell :: QuasiQuoter
haskell = QuasiQuoter (quoteHaskellExp "tok_Haskell_dummy_122" getHaskell ) (quoteHaskellPat "tok_Haskell_dummy_122" getHaskell ) quoteHaskellType quoteHaskellDecs

getAType ( Ctr__Haskell__1 s) = s

aType :: QuasiQuoter
aType = QuasiQuoter (quoteHaskellExp "tok_AType_dummy_121" getAType ) (quoteHaskellPat "tok_AType_dummy_121" getAType ) quoteHaskellType quoteHaskellDecs

getATypeList ( Ctr__Haskell__2 s) = s

aTypeList :: QuasiQuoter
aTypeList = QuasiQuoter (quoteHaskellExp "tok_ATypeList_dummy_120" getATypeList ) (quoteHaskellPat "tok_ATypeList_dummy_120" getATypeList ) quoteHaskellType quoteHaskellDecs

getBType ( Ctr__Haskell__3 s) = s

bType :: QuasiQuoter
bType = QuasiQuoter (quoteHaskellExp "tok_BType_dummy_119" getBType ) (quoteHaskellPat "tok_BType_dummy_119" getBType ) quoteHaskellType quoteHaskellDecs

getBody ( Ctr__Haskell__4 s) = s

body :: QuasiQuoter
body = QuasiQuoter (quoteHaskellExp "tok_Body_dummy_118" getBody ) (quoteHaskellPat "tok_Body_dummy_118" getBody ) quoteHaskellType quoteHaskellDecs

getCName ( Ctr__Haskell__5 s) = s

cName :: QuasiQuoter
cName = QuasiQuoter (quoteHaskellExp "tok_CName_dummy_117" getCName ) (quoteHaskellPat "tok_CName_dummy_117" getCName ) quoteHaskellType quoteHaskellDecs

getCNameList ( Ctr__Haskell__6 s) = s

cNameList :: QuasiQuoter
cNameList = QuasiQuoter (quoteHaskellExp "tok_CNameList_dummy_116" getCNameList ) (quoteHaskellPat "tok_CNameList_dummy_116" getCNameList ) quoteHaskellType quoteHaskellDecs

getClass ( Ctr__Haskell__7 s) = s

__class :: QuasiQuoter
__class = QuasiQuoter (quoteHaskellExp "tok_Class_dummy_115" getClass ) (quoteHaskellPat "tok_Class_dummy_115" getClass ) quoteHaskellType quoteHaskellDecs

getClassList ( Ctr__Haskell__8 s) = s

classList :: QuasiQuoter
classList = QuasiQuoter (quoteHaskellExp "tok_ClassList_dummy_114" getClassList ) (quoteHaskellPat "tok_ClassList_dummy_114" getClassList ) quoteHaskellType quoteHaskellDecs

getCon ( Ctr__Haskell__9 s) = s

con :: QuasiQuoter
con = QuasiQuoter (quoteHaskellExp "tok_Con_dummy_113" getCon ) (quoteHaskellPat "tok_Con_dummy_113" getCon ) quoteHaskellType quoteHaskellDecs

getConstr ( Ctr__Haskell__10 s) = s

constr :: QuasiQuoter
constr = QuasiQuoter (quoteHaskellExp "tok_Constr_dummy_112" getConstr ) (quoteHaskellPat "tok_Constr_dummy_112" getConstr ) quoteHaskellType quoteHaskellDecs

getConstrs ( Ctr__Haskell__11 s) = s

constrs :: QuasiQuoter
constrs = QuasiQuoter (quoteHaskellExp "tok_Constrs_dummy_111" getConstrs ) (quoteHaskellPat "tok_Constrs_dummy_111" getConstrs ) quoteHaskellType quoteHaskellDecs

getContext ( Ctr__Haskell__12 s) = s

context :: QuasiQuoter
context = QuasiQuoter (quoteHaskellExp "tok_Context_dummy_110" getContext ) (quoteHaskellPat "tok_Context_dummy_110" getContext ) quoteHaskellType quoteHaskellDecs

getDClass ( Ctr__Haskell__13 s) = s

dClass :: QuasiQuoter
dClass = QuasiQuoter (quoteHaskellExp "tok_DClass_dummy_109" getDClass ) (quoteHaskellPat "tok_DClass_dummy_109" getDClass ) quoteHaskellType quoteHaskellDecs

getDClassList ( Ctr__Haskell__14 s) = s

dClassList :: QuasiQuoter
dClassList = QuasiQuoter (quoteHaskellExp "tok_DClassList_dummy_108" getDClassList ) (quoteHaskellPat "tok_DClassList_dummy_108" getDClassList ) quoteHaskellType quoteHaskellDecs

getDecl ( Ctr__Haskell__15 s) = s

decl :: QuasiQuoter
decl = QuasiQuoter (quoteHaskellExp "tok_Decl_dummy_107" getDecl ) (quoteHaskellPat "tok_Decl_dummy_107" getDecl ) quoteHaskellType quoteHaskellDecs

getDeclList ( Ctr__Haskell__16 s) = s

declList :: QuasiQuoter
declList = QuasiQuoter (quoteHaskellExp "tok_DeclList_dummy_106" getDeclList ) (quoteHaskellPat "tok_DeclList_dummy_106" getDeclList ) quoteHaskellType quoteHaskellDecs

getDecls ( Ctr__Haskell__17 s) = s

decls :: QuasiQuoter
decls = QuasiQuoter (quoteHaskellExp "tok_Decls_dummy_105" getDecls ) (quoteHaskellPat "tok_Decls_dummy_105" getDecls ) quoteHaskellType quoteHaskellDecs

getDeriving ( Ctr__Haskell__18 s) = s

__deriving :: QuasiQuoter
__deriving = QuasiQuoter (quoteHaskellExp "tok_Deriving_dummy_104" getDeriving ) (quoteHaskellPat "tok_Deriving_dummy_104" getDeriving ) quoteHaskellType quoteHaskellDecs

getExp ( Ctr__Haskell__19 s) = s

exp :: QuasiQuoter
exp = QuasiQuoter (quoteHaskellExp "tok_Exp_dummy_103" getExp ) (quoteHaskellPat "tok_Exp_dummy_103" getExp ) quoteHaskellType quoteHaskellDecs

getExpI ( Ctr__Haskell__20 s) = s

expI :: QuasiQuoter
expI = QuasiQuoter (quoteHaskellExp "tok_ExpI_dummy_102" getExpI ) (quoteHaskellPat "tok_ExpI_dummy_102" getExpI ) quoteHaskellType quoteHaskellDecs

getExport ( Ctr__Haskell__21 s) = s

export :: QuasiQuoter
export = QuasiQuoter (quoteHaskellExp "tok_Export_dummy_101" getExport ) (quoteHaskellPat "tok_Export_dummy_101" getExport ) quoteHaskellType quoteHaskellDecs

getExportsList ( Ctr__Haskell__22 s) = s

exportsList :: QuasiQuoter
exportsList = QuasiQuoter (quoteHaskellExp "tok_ExportsList_dummy_100" getExportsList ) (quoteHaskellPat "tok_ExportsList_dummy_100" getExportsList ) quoteHaskellType quoteHaskellDecs

getExportsOpt ( Ctr__Haskell__23 s) = s

exportsOpt :: QuasiQuoter
exportsOpt = QuasiQuoter (quoteHaskellExp "tok_ExportsOpt_dummy_99" getExportsOpt ) (quoteHaskellPat "tok_ExportsOpt_dummy_99" getExportsOpt ) quoteHaskellType quoteHaskellDecs

getFieldDecl ( Ctr__Haskell__24 s) = s

fieldDecl :: QuasiQuoter
fieldDecl = QuasiQuoter (quoteHaskellExp "tok_FieldDecl_dummy_98" getFieldDecl ) (quoteHaskellPat "tok_FieldDecl_dummy_98" getFieldDecl ) quoteHaskellType quoteHaskellDecs

getFieldDeclList ( Ctr__Haskell__25 s) = s

fieldDeclList :: QuasiQuoter
fieldDeclList = QuasiQuoter (quoteHaskellExp "tok_FieldDeclList_dummy_97" getFieldDeclList ) (quoteHaskellPat "tok_FieldDeclList_dummy_97" getFieldDeclList ) quoteHaskellType quoteHaskellDecs

getFixity ( Ctr__Haskell__26 s) = s

fixity :: QuasiQuoter
fixity = QuasiQuoter (quoteHaskellExp "tok_Fixity_dummy_96" getFixity ) (quoteHaskellPat "tok_Fixity_dummy_96" getFixity ) quoteHaskellType quoteHaskellDecs

getFunLhs ( Ctr__Haskell__27 s) = s

funLhs :: QuasiQuoter
funLhs = QuasiQuoter (quoteHaskellExp "tok_FunLhs_dummy_95" getFunLhs ) (quoteHaskellPat "tok_FunLhs_dummy_95" getFunLhs ) quoteHaskellType quoteHaskellDecs

getGTyCon ( Ctr__Haskell__28 s) = s

gTyCon :: QuasiQuoter
gTyCon = QuasiQuoter (quoteHaskellExp "tok_GTyCon_dummy_94" getGTyCon ) (quoteHaskellPat "tok_GTyCon_dummy_94" getGTyCon ) quoteHaskellType quoteHaskellDecs

getGd ( Ctr__Haskell__29 s) = s

gd :: QuasiQuoter
gd = QuasiQuoter (quoteHaskellExp "tok_Gd_dummy_93" getGd ) (quoteHaskellPat "tok_Gd_dummy_93" getGd ) quoteHaskellType quoteHaskellDecs

getGdRhs ( Ctr__Haskell__30 s) = s

gdRhs :: QuasiQuoter
gdRhs = QuasiQuoter (quoteHaskellExp "tok_GdRhs_dummy_92" getGdRhs ) (quoteHaskellPat "tok_GdRhs_dummy_92" getGdRhs ) quoteHaskellType quoteHaskellDecs

getGenDecl ( Ctr__Haskell__31 s) = s

genDecl :: QuasiQuoter
genDecl = QuasiQuoter (quoteHaskellExp "tok_GenDecl_dummy_91" getGenDecl ) (quoteHaskellPat "tok_GenDecl_dummy_91" getGenDecl ) quoteHaskellType quoteHaskellDecs

getImpDecl ( Ctr__Haskell__32 s) = s

impDecl :: QuasiQuoter
impDecl = QuasiQuoter (quoteHaskellExp "tok_ImpDecl_dummy_90" getImpDecl ) (quoteHaskellPat "tok_ImpDecl_dummy_90" getImpDecl ) quoteHaskellType quoteHaskellDecs

getImpDeclList ( Ctr__Haskell__33 s) = s

impDeclList :: QuasiQuoter
impDeclList = QuasiQuoter (quoteHaskellExp "tok_ImpDeclList_dummy_89" getImpDeclList ) (quoteHaskellPat "tok_ImpDeclList_dummy_89" getImpDeclList ) quoteHaskellType quoteHaskellDecs

getImport ( Ctr__Haskell__34 s) = s

__import :: QuasiQuoter
__import = QuasiQuoter (quoteHaskellExp "tok_Import_dummy_88" getImport ) (quoteHaskellPat "tok_Import_dummy_88" getImport ) quoteHaskellType quoteHaskellDecs

getImportList ( Ctr__Haskell__35 s) = s

importList :: QuasiQuoter
importList = QuasiQuoter (quoteHaskellExp "tok_ImportList_dummy_87" getImportList ) (quoteHaskellPat "tok_ImportList_dummy_87" getImportList ) quoteHaskellType quoteHaskellDecs

getModId ( Ctr__Haskell__36 s) = s

modId :: QuasiQuoter
modId = QuasiQuoter (quoteHaskellExp "tok_ModId_dummy_86" getModId ) (quoteHaskellPat "tok_ModId_dummy_86" getModId ) quoteHaskellType quoteHaskellDecs

getModIdList ( Ctr__Haskell__37 s) = s

modIdList :: QuasiQuoter
modIdList = QuasiQuoter (quoteHaskellExp "tok_ModIdList_dummy_85" getModIdList ) (quoteHaskellPat "tok_ModIdList_dummy_85" getModIdList ) quoteHaskellType quoteHaskellDecs

getModule ( Ctr__Haskell__38 s) = s

__module :: QuasiQuoter
__module = QuasiQuoter (quoteHaskellExp "tok_Module_dummy_84" getModule ) (quoteHaskellPat "tok_Module_dummy_84" getModule ) quoteHaskellType quoteHaskellDecs

getOp ( Ctr__Haskell__39 s) = s

op :: QuasiQuoter
op = QuasiQuoter (quoteHaskellExp "tok_Op_dummy_83" getOp ) (quoteHaskellPat "tok_Op_dummy_83" getOp ) quoteHaskellType quoteHaskellDecs

getOps ( Ctr__Haskell__40 s) = s

ops :: QuasiQuoter
ops = QuasiQuoter (quoteHaskellExp "tok_Ops_dummy_82" getOps ) (quoteHaskellPat "tok_Ops_dummy_82" getOps ) quoteHaskellType quoteHaskellDecs

getOptContext ( Ctr__Haskell__41 s) = s

optContext :: QuasiQuoter
optContext = QuasiQuoter (quoteHaskellExp "tok_OptContext_dummy_81" getOptContext ) (quoteHaskellPat "tok_OptContext_dummy_81" getOptContext ) quoteHaskellType quoteHaskellDecs

getOptDeriving ( Ctr__Haskell__42 s) = s

optDeriving :: QuasiQuoter
optDeriving = QuasiQuoter (quoteHaskellExp "tok_OptDeriving_dummy_80" getOptDeriving ) (quoteHaskellPat "tok_OptDeriving_dummy_80" getOptDeriving ) quoteHaskellType quoteHaskellDecs

getOptExpTypeSignature ( Ctr__Haskell__43 s) = s

optExpTypeSignature :: QuasiQuoter
optExpTypeSignature = QuasiQuoter (quoteHaskellExp "tok_OptExpTypeSignature_dummy_79" getOptExpTypeSignature ) (quoteHaskellPat "tok_OptExpTypeSignature_dummy_79" getOptExpTypeSignature ) quoteHaskellType quoteHaskellDecs

getOptGdRhs ( Ctr__Haskell__44 s) = s

optGdRhs :: QuasiQuoter
optGdRhs = QuasiQuoter (quoteHaskellExp "tok_OptGdRhs_dummy_78" getOptGdRhs ) (quoteHaskellPat "tok_OptGdRhs_dummy_78" getOptGdRhs ) quoteHaskellType quoteHaskellDecs

getOptImpSpec ( Ctr__Haskell__45 s) = s

optImpSpec :: QuasiQuoter
optImpSpec = QuasiQuoter (quoteHaskellExp "tok_OptImpSpec_dummy_77" getOptImpSpec ) (quoteHaskellPat "tok_OptImpSpec_dummy_77" getOptImpSpec ) quoteHaskellType quoteHaskellDecs

getOptInteger ( Ctr__Haskell__46 s) = s

optInteger :: QuasiQuoter
optInteger = QuasiQuoter (quoteHaskellExp "tok_OptInteger_dummy_76" getOptInteger ) (quoteHaskellPat "tok_OptInteger_dummy_76" getOptInteger ) quoteHaskellType quoteHaskellDecs

getOptQualified ( Ctr__Haskell__47 s) = s

optQualified :: QuasiQuoter
optQualified = QuasiQuoter (quoteHaskellExp "tok_OptQualified_dummy_75" getOptQualified ) (quoteHaskellPat "tok_OptQualified_dummy_75" getOptQualified ) quoteHaskellType quoteHaskellDecs

getOptQualifiedAs ( Ctr__Haskell__48 s) = s

optQualifiedAs :: QuasiQuoter
optQualifiedAs = QuasiQuoter (quoteHaskellExp "tok_OptQualifiedAs_dummy_74" getOptQualifiedAs ) (quoteHaskellPat "tok_OptQualifiedAs_dummy_74" getOptQualifiedAs ) quoteHaskellType quoteHaskellDecs

getOptWhere ( Ctr__Haskell__49 s) = s

optWhere :: QuasiQuoter
optWhere = QuasiQuoter (quoteHaskellExp "tok_OptWhere_dummy_73" getOptWhere ) (quoteHaskellPat "tok_OptWhere_dummy_73" getOptWhere ) quoteHaskellType quoteHaskellDecs

getPat ( Ctr__Haskell__50 s) = s

pat :: QuasiQuoter
pat = QuasiQuoter (quoteHaskellExp "tok_Pat_dummy_72" getPat ) (quoteHaskellPat "tok_Pat_dummy_72" getPat ) quoteHaskellType quoteHaskellDecs

getQOp ( Ctr__Haskell__51 s) = s

qOp :: QuasiQuoter
qOp = QuasiQuoter (quoteHaskellExp "tok_QOp_dummy_71" getQOp ) (quoteHaskellPat "tok_QOp_dummy_71" getQOp ) quoteHaskellType quoteHaskellDecs

getQTyCls ( Ctr__Haskell__52 s) = s

qTyCls :: QuasiQuoter
qTyCls = QuasiQuoter (quoteHaskellExp "tok_QTyCls_dummy_70" getQTyCls ) (quoteHaskellPat "tok_QTyCls_dummy_70" getQTyCls ) quoteHaskellType quoteHaskellDecs

getQTyCon ( Ctr__Haskell__53 s) = s

qTyCon :: QuasiQuoter
qTyCon = QuasiQuoter (quoteHaskellExp "tok_QTyCon_dummy_69" getQTyCon ) (quoteHaskellPat "tok_QTyCon_dummy_69" getQTyCon ) quoteHaskellType quoteHaskellDecs

getQVar ( Ctr__Haskell__54 s) = s

qVar :: QuasiQuoter
qVar = QuasiQuoter (quoteHaskellExp "tok_QVar_dummy_68" getQVar ) (quoteHaskellPat "tok_QVar_dummy_68" getQVar ) quoteHaskellType quoteHaskellDecs

getQVarId ( Ctr__Haskell__55 s) = s

qVarId :: QuasiQuoter
qVarId = QuasiQuoter (quoteHaskellExp "tok_QVarId_dummy_67" getQVarId ) (quoteHaskellPat "tok_QVarId_dummy_67" getQVarId ) quoteHaskellType quoteHaskellDecs

getQVarList ( Ctr__Haskell__56 s) = s

qVarList :: QuasiQuoter
qVarList = QuasiQuoter (quoteHaskellExp "tok_QVarList_dummy_66" getQVarList ) (quoteHaskellPat "tok_QVarList_dummy_66" getQVarList ) quoteHaskellType quoteHaskellDecs

getRhs ( Ctr__Haskell__57 s) = s

rhs :: QuasiQuoter
rhs = QuasiQuoter (quoteHaskellExp "tok_Rhs_dummy_65" getRhs ) (quoteHaskellPat "tok_Rhs_dummy_65" getRhs ) quoteHaskellType quoteHaskellDecs

getSimpleType ( Ctr__Haskell__58 s) = s

simpleType :: QuasiQuoter
simpleType = QuasiQuoter (quoteHaskellExp "tok_SimpleType_dummy_64" getSimpleType ) (quoteHaskellPat "tok_SimpleType_dummy_64" getSimpleType ) quoteHaskellType quoteHaskellDecs

getTopDecl ( Ctr__Haskell__59 s) = s

topDecl :: QuasiQuoter
topDecl = QuasiQuoter (quoteHaskellExp "tok_TopDecl_dummy_63" getTopDecl ) (quoteHaskellPat "tok_TopDecl_dummy_63" getTopDecl ) quoteHaskellType quoteHaskellDecs

getTopDecls ( Ctr__Haskell__60 s) = s

topDecls :: QuasiQuoter
topDecls = QuasiQuoter (quoteHaskellExp "tok_TopDecls_dummy_62" getTopDecls ) (quoteHaskellPat "tok_TopDecls_dummy_62" getTopDecls ) quoteHaskellType quoteHaskellDecs

getTyCls ( Ctr__Haskell__61 s) = s

tyCls :: QuasiQuoter
tyCls = QuasiQuoter (quoteHaskellExp "tok_TyCls_dummy_61" getTyCls ) (quoteHaskellPat "tok_TyCls_dummy_61" getTyCls ) quoteHaskellType quoteHaskellDecs

getTyCon ( Ctr__Haskell__62 s) = s

tyCon :: QuasiQuoter
tyCon = QuasiQuoter (quoteHaskellExp "tok_TyCon_dummy_60" getTyCon ) (quoteHaskellPat "tok_TyCon_dummy_60" getTyCon ) quoteHaskellType quoteHaskellDecs

getTyVar ( Ctr__Haskell__63 s) = s

tyVar :: QuasiQuoter
tyVar = QuasiQuoter (quoteHaskellExp "tok_TyVar_dummy_59" getTyVar ) (quoteHaskellPat "tok_TyVar_dummy_59" getTyVar ) quoteHaskellType quoteHaskellDecs

getTyVars ( Ctr__Haskell__64 s) = s

tyVars :: QuasiQuoter
tyVars = QuasiQuoter (quoteHaskellExp "tok_TyVars_dummy_58" getTyVars ) (quoteHaskellPat "tok_TyVars_dummy_58" getTyVars ) quoteHaskellType quoteHaskellDecs

getType ( Ctr__Haskell__65 s) = s

__type :: QuasiQuoter
__type = QuasiQuoter (quoteHaskellExp "tok_Type_dummy_57" getType ) (quoteHaskellPat "tok_Type_dummy_57" getType ) quoteHaskellType quoteHaskellDecs

getTypeList ( Ctr__Haskell__66 s) = s

typeList :: QuasiQuoter
typeList = QuasiQuoter (quoteHaskellExp "tok_TypeList_dummy_56" getTypeList ) (quoteHaskellPat "tok_TypeList_dummy_56" getTypeList ) quoteHaskellType quoteHaskellDecs

getVar ( Ctr__Haskell__67 s) = s

var :: QuasiQuoter
var = QuasiQuoter (quoteHaskellExp "tok_Var_dummy_55" getVar ) (quoteHaskellPat "tok_Var_dummy_55" getVar ) quoteHaskellType quoteHaskellDecs

getVars ( Ctr__Haskell__68 s) = s

vars :: QuasiQuoter
vars = QuasiQuoter (quoteHaskellExp "tok_Vars_dummy_54" getVars ) (quoteHaskellPat "tok_Vars_dummy_54" getVars ) quoteHaskellType quoteHaskellDecs

