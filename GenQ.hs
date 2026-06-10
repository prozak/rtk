{-# LANGUAGE QuasiQuotes #-}
module GenQ (genQ)
    where

import Syntax
import Diagnostics (Diagnostic)
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

-- genQ has no user-reachable error paths today (its remaining guards are
-- internal invariants); the Either signature keeps it uniform with genX/genY.
genQ :: NormalGrammar -> Either Diagnostic String
genQ (NormalGrammar name rules _ antiRules shortcuts _ info) = Right [str|?banner
{-# LANGUAGE TemplateHaskell #-}
module ?name~QQ
where

import qualified Data.Char as C
import qualified Data.Map as M
import Data.List
import Data.Maybe
import qualified Data.Generics as Generics
import qualified Data.Data as Data
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import ?name~Lexer
import ?name~Parser

qqShortcuts :: M.Map String String

-- A $name metavariable is rewritten to $Type:name using the qqShortcuts
-- table below. The rewrite is purely textual, so it would also fire inside
-- the quoted language's own string literals: write $$name there to escape
-- it and get the literal text $name. Each '$$' pair directly before a
-- metavariable stands for one literal '$' (so $$$x is a literal '$'
-- followed by the metavariable $x). A '$' not followed by an identifier is
-- never rewritten and needs no escape, and an explicit $Type:name antiquote
-- (the character after the name is ':') passes through untouched.
-- A plain character scan rather than a regex: metavariables are recognized
-- regardless of what follows the name - including a newline or the end of
-- the quote, which the previous regex's terminator class missed.
replaceAllPatterns :: String -> Either String String
replaceAllPatterns [] = Right []
replaceAllPatterns s@('$' : _) =
  let (dollars, rest) = span (== '$') s
  in case rest of
       (c1 : _) | C.isAlpha c1 || c1 == '_' ->
         let (varName, post) = span (\ ch -> C.isAlphaNum ch || ch == '_') rest
             escCount = length dollars - 1
             keptPre = replicate (div escCount 2) '$'
         in case post of
              (':' : _) -> ((dollars ++ varName) ++) <$> replaceAllPatterns post
              _ | odd escCount -> ((keptPre ++ '$' : varName) ++) <$> replaceAllPatterns post
              _ -> case catMaybes $ map (\ prefix -> M.lookup prefix qqShortcuts) $ reverse $ inits varName of
                     [] -> Left $ unlines
                             [ "Unknown metavariable $" ++ varName ++ " in quasi-quote:"
                             , "no prefix of '" ++ varName ++ "' is a known shortcut. Known shortcuts:"
                             , "  " ++ intercalate ", " (M.keys qqShortcuts)
                             , "To include the literal text $" ++ varName ++ " in the quoted code"
                             , "(e.g. inside a string literal), escape it as $$" ++ varName ++ "." ]
                     (rule : _) -> ((keptPre ++ '$' : rule ++ ":" ++ varName) ++) <$> replaceAllPatterns post
       _ -> (dollars ++) <$> replaceAllPatterns rest
replaceAllPatterns (c : rest) = (c :) <$> replaceAllPatterns rest

-- The generated lexer and parser encode error positions as "LINE:COL:message"
-- so structured-diagnostic callers can split them; render them back
-- human-readably for quasi-quote compile errors. Positions refer to the quote
-- body (padded with a start token in front).
rtkRenderError :: String -> String
rtkRenderError err =
    case span (/= ':') err of
        (l, ':' : rest1) | [(line, "")] <- (reads l :: [(Int, String)]) ->
            case span (/= ':') rest1 of
                (c, ':' : msg) | [(col, "")] <- (reads c :: [(Int, String)]) ->
                    "line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg
                _ -> err
        _ -> err

?qqShortCutsMapDef

-- A quasi-quote pattern must match an AST parsed from anywhere in a source
-- file, while the pattern itself was parsed from the quote body - so every
-- RtkPos position field becomes a wildcard in generated patterns.
-- (Expressions need no special case: the compile-time position they embed
-- is equality-transparent.)
rtkPosWildPat :: RtkPos -> Maybe (TH.Q TH.Pat)
rtkPosWildPat _ = Just TH.wildP

?(qqFunProtoGen expType)
?(qqFunImplGen expType)
?(qqFunProtoGen pat)
?(qqFunImplGen pat)
?antiFunsGenExp
?antiFunsGenPat

-- This grammar's quasi-quoters work in expression and pattern contexts only
?qqFunType
?qqFunDecs

?qqParseFuns
|]
    where banner = provenanceBanner name
          pat = "Pat"
          expType = "Exp"
          proxyRules = getProxyRules info
          qqFunName typ = [str|quote?name~?typ|]
          typeNames = map arQQName antiRules
          antiNameGen typ n = "anti" ++ n ++ typ
          antiTermGen typ = map (antiNameGen typ) typeNames
          -- Patterns additionally wildcard every RtkPos field (see
          -- rtkPosWildPat in the generated module)
          extBase "Pat" = "const Nothing `Generics.extQ` rtkPosWildPat"
          extBase _     = "const Nothing"
          antiExprsGen typ = foldr (\antiTerm res -> [str|?res `Generics.extQ` ?antiTerm|]) (extBase typ) $ antiTermGen typ
          antiFunsGen typ = map (\(AntiRule tdName qqName consName isList) ->
                                        let antiName = antiNameGen typ qqName
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
          startRuleName = fromMaybe (error "Internal error (GenQ): start rule name is not set in grammar info")
                                    (getStartRuleName info)
          startTokenFor typeName = fromMaybe (error $ "Internal error (GenQ): no start token recorded for type '" ++ typeName ++ "'")
                                             (M.lookup typeName $ getRuleToStartInfo info)
          constructorFor typeName = fromMaybe (error $ "Internal error (GenQ): no start constructor found for type '" ++ typeName ++ "'")
                                              (M.lookup typeName typeNameToConstructor)
          qqFunProtoGen typ = [str|?(qqFunName typ) :: Data.Data a => String -> (?startRuleName -> a) -> String -> TH.?typ~Q|]
          dataToExpCall typ var = [str|  dataTo?typ~Q (?(antiExprsGen typ)) ?var|]
          qqFunImplGen typ = [str|?(qqFunName typ ) dummy func s = do
  s1 <- either fail return (replaceAllPatterns s)
  ast <- case scanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy) >>= parse?name of
           Left err -> fail (rtkRenderError err)
           Right a -> return a
  let expr = func ast
|] ++ dataToExpCall typ "expr"
          -- quoteType/quoteDec used to return dummies (TH.ListT / []),
          -- silently compiling a quote in the wrong context into nonsense;
          -- failing in TH.Q makes it a compile error at the splice site
          qqFunType = qqFunName "Type"
                      ++ " _ = fail \"this quasi-quoter cannot be used in a type context\""
          qqFunDecs = qqFunName "Decs"
                      ++ " _ = fail \"this quasi-quoter cannot be used in a declaration context\""
          antiFunsGenExp = unlines $ antiFunsGen expType
          antiFunsGenPat = unlines $ antiFunsGen pat
          typeNameToConstructor = M.fromList $ map (\(STSeq cName lst) ->
                                        case lst of
                                          [_, SSId typeName, _] -> (typeName, cName)
                                          _ -> ("", ""))
                                      (case rules of
                                         (firstRule:_) -> case getSRules firstRule of
                                           (firstSRule:_) -> case getSClause firstSRule of
                                                               STAltOfSeq alts -> alts
                                                               _ -> []
                                           [] -> []
                                         [] -> [])
          rulesWithoutProxies = filterProxyRules proxyRules rules
          -- Only types wrapped by the synthesized start rule can have a
          -- top-level quoter: quoting parses the dummy-padded fragment
          -- through the start symbol and projects it back out by matching
          -- the wrapper constructor. When the start group is a type alias
          -- no wrappers exist (see Normalize.addStartGroup) and no quoters
          -- are emitted; for a data start group every public type is
          -- wrapped, so nothing is filtered.
          quotableRules = filter ((`M.member` getRuleToStartInfo info) . getSDataTypeName)
                                 rulesWithoutProxies
          qqParseFuns =  intercalate "\n"
                            $ map (\ruleGroup ->
                                       case getSDataTypeName ruleGroup of
                                         typeName@(s : rest) ->
                                           let sortFunName = sortNameToHaskellName $ C.toLower s : rest
                                               dummy = "\"" ++ startTokenFor typeName ++ "\""
                                               getFun = "get" ++ typeName
                                               dataConstructor = constructorFor typeName
                                           in
                                             [str|?getFun ( ?dataConstructor _ s) = s

?sortFunName :: QuasiQuoter
?sortFunName = QuasiQuoter (?(qqFunName expType) ?dummy ?getFun ) (?(qqFunName pat) ?dummy ?getFun ) ?(qqFunName "Type") ?(qqFunName "Decs")
|]
                                         _ -> "")
                                quotableRules
          shortCutTypes = map (\ruleGroup ->
                                case getSDataTypeName ruleGroup of
                                  typeName@(s : rest) -> ((C.toLower s : rest), typeName)
                                  _ -> ("", ""))
                            rulesWithoutProxies
          qqShortCutsMapDef = "qqShortcuts = M.fromList [ " ++ (intercalate ","
                                                                          (map (\(s, r) -> [str|("?s~","?r~")|])
                                                                                (shortCutTypes ++ shortcuts)))
                              ++ "]"
