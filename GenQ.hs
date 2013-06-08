{-# LANGUAGE QuasiQuotes #-}
module GenQ (genQ)
    where

import Parser
import Text.PrettyPrint
import Grammar
import qualified Data.Char as C
import qualified Data.Map as M
import Data.Maybe
import Data.List
import StrQuote

sortNameToHaskellName :: String -> String
sortNameToHaskellName "import" = "__import"
sortNameToHaskellName "module" = "__module"
sortNameToHaskellName "type" = "__type"
sortNameToHaskellName "class" = "__class"
sortNameToHaskellName "deriving" = "__deriving"
sortNameToHaskellName str = str

genQ :: NormalGrammar -> String
genQ g@(NormalGrammar name synRuleGs _ antiRules shortcuts _ info) = [str|{-# LANGUAGE TemplateHaskell #-}
module ?name~QQ
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
import ?name~Lexer
import ?name~Parser

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

?qqShortCutsMapDef

?(qqFunProtoGen exp)
?(qqFunImplGen exp)
?(qqFunProtoGen pat)
?(qqFunImplGen pat)
?antiFunsGenExp
?antiFunsGenPat

?qqFunType
?qqFunDecs

?qqParseFuns
|]
    where pat = "Pat"
          exp = "Exp"
          qqFunName typ = [str|quote?name~?typ|]
          -- typeNames = map getSDataTypeName $ filter (\srg -> case srg of
          --                                                      SyntaxRuleGroup _ [SyntaxRule _ (STMany _ _ _)] -> False
          --                                                      SyntaxRuleGroup _ [SyntaxRule _ (STOpt _ )] -> False
          --                                                      _ -> True)
          --                                           $ tail synRuleGs
          typeNames = map arQQName antiRules
          antiNameGen typ name = "anti" ++ name ++ typ
          antiTermGen typ = map (antiNameGen typ) typeNames
          antiExprsGen typ = foldr (\antiTerm res -> [str|?res `Generics.extQ` ?antiTerm|]) "const Nothing" $ antiTermGen typ
          antiFunsGen typ = map (\(AntiRule name qqName consName isList) -> 
                                        let antiName = antiNameGen typ qqName
                                            antiElName = antiNameGen typ name
                                            varConstructor = case typ of
                                                                "Pat" -> "TH.varP"
                                                                "Exp" -> "TH.varE"
                                            listPatGen = [str|
?antiName :: [ ?name ] -> Maybe (TH.Q TH.Pat)
?antiName [?consName v] = Just $ ?varConstructor (TH.mkName v)
?antiName _ = Nothing
|]
                                            listExpGen = [str|
?antiName :: [ ?name ] -> Maybe (TH.Q TH.Exp)
?antiName ((?consName v):rest) =
 let restExp = ?(dataToExpCall "Exp" "rest")
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |~]
?antiName _ = Nothing
|]
                                            nonListGen = [str|
?antiName :: ?name -> Maybe (TH.Q TH.?typ )
?antiName ( ?consName v) = Just $ ?varConstructor (TH.mkName v)
?antiName _ = Nothing
|]
                                        in
                                          if isList
                                             then if typ == "Pat"
                                                    then listPatGen
                                                    else listExpGen
                                             else nonListGen )
                                antiRules
          qqFunProtoGen typ = [str|?(qqFunName typ) :: Data.Data a => String -> (?(fromJust $ getStartRuleName info) -> a) -> String -> TH.?typ~Q|]
          dataToExpCall typ var = [str|  dataTo?typ~Q (?(antiExprsGen typ)) ?var|]
          qqFunImplGen typ = [str|?(qqFunName typ ) dummy func s = do
  let s1 = replaceAllPatterns s
      expr = func $ parse?name $ alexScanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy)
|] ++ dataToExpCall typ "expr"
          qqFunType = qqFunName "Type" ++ " s = return TH.ListT"
          qqFunDecs = qqFunName "Decs" ++ " s = return []"
          antiFunsGenExp = unlines $ antiFunsGen exp
          antiFunsGenPat = unlines $ antiFunsGen pat
          typeNameToConstructor = M.fromList $ map (\(STSeq cName lst) ->
                                        case lst of
                                          [_, SSId typeName, _] -> (typeName, cName)
                                          _ -> ("", ""))
                                      (getAltOfSeq $ getSClause $ head $ getSRules $ head synRuleGs)
          qqParseFuns =  intercalate "\n" 
                            $ map (\SyntaxRuleGroup { getSDataTypeName = typeName@(s : rest)} ->
                                       let sortFunName = sortNameToHaskellName $ C.toLower s : rest
                                           dummy = "\"" ++ (fromJust $ M.lookup typeName $ getRuleToStartInfo info) ++ "\""
                                           getFun = "get" ++ typeName
                                           dataConstructor = fromJust $ M.lookup typeName typeNameToConstructor
                                       in
                                         [str|?getFun ( ?dataConstructor s) = s

?sortFunName :: QuasiQuoter
?sortFunName = QuasiQuoter (?(qqFunName exp) ?dummy ?getFun ) (?(qqFunName pat) ?dummy ?getFun ) ?(qqFunName "Type") ?(qqFunName "Decs")
|])
                                 $ tail synRuleGs
          shortCutTypes = map (\SyntaxRuleGroup { getSDataTypeName = typeName@(s : rest)} -> ((C.toLower s : rest), typeName)) $ tail synRuleGs
          qqShortCutsMapDef = "qqShortcuts = M.fromList [ " ++ (intercalate ","
                                                                          (map (\(s, r) -> [str|("?s~","?r~")|])
                                                                                (shortCutTypes ++ shortcuts)))
                              ++ "]"
