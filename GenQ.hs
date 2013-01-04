{-# LANGUAGE QuasiQuotes #-}
module GenQ (genQ)
    where

import Parser
import Text.PrettyPrint
import Grammar
import qualified Data.Char as C
import qualified Data.Map as M
import Data.Maybe
import StrQuote

shortCutImports :: Doc
shortCutImports = vcat $ map text $ lines [str|
import Text.Regex.Posix
import Text.Regex.Base
import qualified Data.Map as M
import Data.List
import Data.Maybe
|]

shortCutDefs :: Doc
shortCutDefs = vcat $ map text $ lines [str|
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

|]

genQ :: NormalGrammar -> String
genQ g@(NormalGrammar name synRuleGs _ antiRules shortcuts _ info) =
    render $ vcat [
                   header,
                   nl,
                   shortCutDefs,
                   qqShortCutsMapDef,
                   nl,
                   qqFuns,
                   nl,
                   qqFunType,
                   qqFunDecs,
                   nl,
                   qqParseFuns,
                   nl
                  ]
    where header = vcat [text "{-# LANGUAGE TemplateHaskell #-}",
                         text "module" <+> text name <> text "QQ",
                         text "where ",
                         nl,
                         text "import Data.Generics",
                         text "import Data.Data",
                         text "import qualified Language.Haskell.TH as TH",
                         text "import Language.Haskell.TH.Quote",
                         shortCutImports,
                         nl,
                         text "import" <+> text name <> text "Lexer",
                         text "import" <+> text name <> text "Parser"
                        ]
          pat = "Pat"
          exp = "Exp"
          qqFunName typ = text "quote" <> text name <> text typ
          -- typeNames = map getSDataTypeName $ filter (\srg -> case srg of
          --                                                      SyntaxRuleGroup _ [SyntaxRule _ (STMany _ _ _)] -> False
          --                                                      SyntaxRuleGroup _ [SyntaxRule _ (STOpt _ )] -> False
          --                                                      _ -> True)
          --                                           $ tail synRuleGs
          typeNames = map arQQName antiRules
          antiNameGen typ name = "anti" ++ name ++ typ
          antiTermGen typ = map (antiNameGen typ) typeNames
          antiExprsGen typ = foldr (\antiTerm res -> res <+> text "`extQ`" <+> text antiTerm) (text "const Nothing") $ antiTermGen typ
          antiFunsGen typ = map (\(AntiRule name qqName consName isList) -> 
                                        let antiName = (antiNameGen typ qqName)
                                            antiElName = antiNameGen typ name
                                            varConstructor = case typ of
                                                                "Pat" -> text "TH.varP"
                                                                "Exp" -> text "TH.varE"
                                            listPatGen =  vcat [text antiName <+> text ":: [" <+> text name 
                                                                             <+> text "] -> Maybe (TH.Q TH.Pat)",
                                                               text antiName <+> text "[" <> text consName
                                                                             <+> text "v] = Just $" <+> varConstructor 
                                                                             <+> text "(TH.mkName v)",
                                                               text antiName <+> text "_ = Nothing"]
                                            listExpGen = vcat [text antiName <+> text ":: [" <+> text name 
                                                                             <+> text "] -> Maybe (TH.Q TH.Exp)",
                                                               text antiName <+> text "((" <> text consName
                                                                             <+> text "v) :rest ) =" 
                                                                             <+> vcat [ text "let" <+> vcat [ text "restExp =" 
                                                                                                                   <+> dataToExpCall "Exp" "rest",
                                                                                                              text "lvar = TH.varE $ TH.mkName v"
                                                                                                            ],
                                                                                        text "in Just [| $lvar ++ $restExp |]" ],
                                                               text antiName <+> text "_ = Nothing"]
                                            nonListGen = vcat [text antiName <+> text "::" <+> text name 
                                                                             <+> text "-> Maybe (TH.Q TH." <> text typ <> text ")",
                                                               text antiName <+> text "(" <> text consName
                                                                             <+> text "v) = Just $" <+> varConstructor 
                                                                             <+> text "(TH.mkName v)",
                                                               text antiName <+> text "_ = Nothing"]

                                        in
                                          if isList
                                             then if typ == "Pat"
                                                    then listPatGen
                                                    else listExpGen
                                             else nonListGen )
                                antiRules
          qqFunProtoGen typ = qqFunName typ <+> text ":: Data a => String -> ("
                                 <+> text (fromJust $ getStartRuleName info) <+> text "-> a) -> String -> TH." <> text typ <> text "Q"
          dataToExpCall typ var = text "  dataTo" <> text typ <> text "Q (" <> antiExprsGen typ <> text ") " <> text var
          qqFunImplGen typ = vcat [qqFunName typ <+> text "dummy func s = do",
                                   text "  let s1 = replaceAllPatterns s",
                                   text "      expr = func $ parse" <> text name <+> text "$ alexScanTokens (dummy ++ \" \" ++ s1 ++ \" \" ++ dummy)",
                                   dataToExpCall typ "expr"]
          qqFunType = qqFunName "Type" <+> text "s = return TH.ListT"
          qqFunDecs = qqFunName "Decs" <+> text "s = return []"
          qqFuns = vcat [qqFunProtoGen exp,
                         qqFunImplGen exp,
                         qqFunProtoGen pat,
                         qqFunImplGen pat,
                         vcat $ antiFunsGen exp,
                         vcat $ antiFunsGen pat]
          typeNameToConstructor = M.fromList $ map (\(STSeq cName lst) ->
                                        case lst of
                                          [_, SSId typeName, _] -> (typeName, cName)
                                          _ -> ("", ""))
                                      (getAltOfSeq $ getSClause $ head $ getSRules $ head synRuleGs)
          qqParseFuns = vcat $ map (\SyntaxRuleGroup { getSDataTypeName = typeName@(s : rest)} ->
                                       let sortFunName = text (C.toLower s : rest)
                                           dummy = "\"" ++ (fromJust $ M.lookup typeName $ getRuleToStartInfo info) ++ "\""
                                           getFun = "get" ++ typeName
                                           dataConstructor = fromJust $ M.lookup typeName typeNameToConstructor
                                       in
                                         vcat [ text getFun <+> text "(" <+> text dataConstructor <+> text "s) = s",
                                                nl,
                                                sortFunName <+> text ":: QuasiQuoter",
                                                sortFunName <+> text "= QuasiQuoter (" <> qqFunName exp <+> text dummy <+> text getFun <> text ") ("
                                                    <> qqFunName pat <+> text dummy <+> text getFun <> text ")" <+> qqFunName "Type" <+> qqFunName "Decs",
                                                nl])
                                 $ tail synRuleGs
          nl = text ""
          shortCutTypes = map (\SyntaxRuleGroup { getSDataTypeName = typeName@(s : rest)} -> ((C.toLower s : rest), typeName)) $ tail synRuleGs
          qqShortCutsMapDef = text "qqShortcuts = M.fromList [ " <> vcat (punctuate (text ",") $ 
                                                                            map (\(s, r) -> text "(\"" <> text s 
                                                                                                       <> text "\",\"" 
                                                                                                       <> text r 
                                                                                                       <> text "\")") 
                                                                                (shortCutTypes ++ shortcuts))
                              <> text "]"
