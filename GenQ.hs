{-# LANGUAGE QuasiQuotes #-}
module GenQ (genQ)
    where

import Parser
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
sortNameToHaskellName s = s

genQ :: NormalGrammar -> String
genQ (NormalGrammar name rules _ antiRules shortcuts _ info) = [str|{-# LANGUAGE TemplateHaskell #-}
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

?(qqFunProtoGen expType)
?(qqFunImplGen expType)
?(qqFunProtoGen pat)
?(qqFunImplGen pat)
?antiFunsGenExp
?antiFunsGenPat

?qqFunType
?qqFunDecs

?qqParseFuns
|]
    where pat = "Pat"
          expType = "Exp"
          proxyRules = getProxyRules info
          qqFunName typ = [str|quote?name~?typ|]
          typeNames = map arQQName antiRules
          antiNameGen typ n isList = "anti" ++ n ++ (if isList then "List" else "") ++ typ
          antiTermGen typ = map (\ar -> antiNameGen typ (arQQName ar) (arIsList ar)) antiRules
          antiExprsGen typ = foldr (\antiTerm res -> [str|?res `Generics.extQ` ?antiTerm|]) "const Nothing" $ antiTermGen typ
          antiFunsGen typ = map (\(AntiRule tdName qqName consName isList) ->
                                        let antiName = antiNameGen typ qqName isList
                                            varConstructor = case typ of
                                                                "Pat" -> "TH.varP"
                                                                "Exp" -> "TH.varE"
                                                                _ -> "TH.varE"  -- default case
                                            listPatGen = [str|
?antiName :: [ ?tdName ] -> Maybe (TH.Q TH.Pat)
?antiName [?consName v] = Just $ ?varConstructor (TH.mkName v)
?antiName _ = Nothing
|]
                                            listExpGen = [str|
?antiName :: [ ?tdName ] -> Maybe (TH.Q TH.Exp)
?antiName ((?consName v):rest) =
 let restExp = ?(dataToExpCall "Exp" "rest")
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |~]
?antiName _ = Nothing
|]
                                            nonListGen = [str|
?antiName :: ?tdName -> Maybe (TH.Q TH.?typ )
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
          antiFunsGenExp = unlines $ antiFunsGen expType
          antiFunsGenPat = unlines $ antiFunsGen pat
          typeNameToConstructor = M.fromList $ map (\(STSeq cName lst) ->
                                        case lst of
                                          [_, SSId typeName, _] -> (typeName, cName)
                                          _ -> ("", ""))
                                      (case rules of
                                         (firstRule:_) -> case getSRules firstRule of
                                           (firstSRule:_) -> getAltOfSeq $ getSClause firstSRule
                                           [] -> []
                                         [] -> [])
          rulesWithoutProxies = filterProxyRules proxyRules rules
          qqParseFuns =  intercalate "\n"
                            $ map (\ruleGroup ->
                                       case getSDataTypeName ruleGroup of
                                         typeName@(s : rest) ->
                                           let sortFunName = sortNameToHaskellName $ C.toLower s : rest
                                               dummy = "\"" ++ (fromJust $ M.lookup typeName $ getRuleToStartInfo info) ++ "\""
                                               getFun = "get" ++ typeName
                                               dataConstructor = fromJust $ M.lookup typeName typeNameToConstructor
                                           in
                                             [str|?getFun ( ?dataConstructor s) = s

?sortFunName :: QuasiQuoter
?sortFunName = QuasiQuoter (?(qqFunName expType) ?dummy ?getFun ) (?(qqFunName pat) ?dummy ?getFun ) ?(qqFunName "Type") ?(qqFunName "Decs")
|]
                                         _ -> "")
                                rulesWithoutProxies
          shortCutTypes = map (\ruleGroup ->
                                case getSDataTypeName ruleGroup of
                                  typeName@(s : rest) -> ((C.toLower s : rest), typeName)
                                  _ -> ("", ""))
                            rulesWithoutProxies
          qqShortCutsMapDef = "qqShortcuts = M.fromList [ " ++ (intercalate ","
                                                                          (map (\(s, r) -> [str|("?s~","?r~")|])
                                                                                (shortCutTypes ++ shortcuts)))
                              ++ "]"
