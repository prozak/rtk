module GenQ (genQ)
    where

import Parser
import Text.PrettyPrint
import Grammar
import qualified Data.Char as C
import qualified Data.Map as M
import Data.Maybe

genQ :: NormalGrammar -> String
genQ g@(NormalGrammar name synRuleGs lexRules) =
    render $ vcat [
                   header,
                   nl,
                   qqFuns,
                   nl,
                   qqFunType,
                   qqFunDecs,
                   nl,
                   qqParseFuns,
                   nl
                  ]
    where header = vcat [text "module" <+> text name <> text "QQ",
                         text "where ",
                         nl,
                         text "import Data.Generics",
                         text "import Data.Data",
                         text "import qualified Language.Haskell.TH as TH",
                         text "import Language.Haskell.TH.Quote",
                         nl,
                         text "import" <+> text name <> text "Lexer",
                         text "import" <+> text name <> text "Parser"
                        ]
          pat = "Pat"
          exp = "Exp"
          qqFunName typ = text "quote" <> text name <> text typ
          antiTermGen typ = map (\SyntaxRuleGroup { getSDataTypeName = name} -> "anti" ++ name ++ typ) $ tail synRuleGs
          antiExprsGen typ = foldr (\antiTerm res -> res <+> text "`extQ`" <+> text antiTerm) (text "const Nothing") [] {--$ antiTermGen typ--}
          qqFunProtoGen typ = qqFunName typ <+> text ":: Data a => String -> ("
                                 <+> text "Start" <+> text "-> a) -> String -> TH." <> text typ <> text "Q"
          qqFunImplGen typ = vcat [qqFunName typ <+> text "dummy func s = do",
                                   text "  let expr = func $ parse" <> text name <+> text "$ alexScanTokens (dummy ++ \" \" ++ s ++ \" \" ++ dummy)",
                                   text "  dataTo" <> text typ <> text "Q (" <> antiExprsGen typ <> text ") expr"]
          qqFunType = qqFunName "Type" <+> text "s = return TH.ListT"
          qqFunDecs = qqFunName "Decs" <+> text "s = return []"
          qqFuns = vcat [qqFunProtoGen exp,
                         qqFunImplGen exp,
                         qqFunProtoGen pat,
                         qqFunImplGen pat,
                         vcat $ map (\antiTerm -> text antiTerm <+> text "_ = Nothing")$ antiTermGen exp,
                         vcat $ map (\antiTerm -> text antiTerm <+> text "_ = Nothing")$ antiTermGen pat]
          typeNameToConstructor = M.fromList $ map (\(STSeq cName lst) ->
                                        case lst of
                                          [SSId typeName] -> (typeName, cName)
                                          [_, SSId typeName, _] -> (typeName, cName))
                                      (getAltOfSeq $ getSClause $ head $ getSRules $ head synRuleGs)
          qqParseFuns = vcat $ map (\SyntaxRuleGroup { getSDataTypeName = typeName@(s : rest)} ->
                                       let sortFunName = text (C.toLower s : rest)
                                           dummy = "\"" ++ typeName ++ "__dummy\""
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
