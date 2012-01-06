{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Grammar where

import Parser
import Language.Haskell.TH hiding (Clause)
import Data.Char
import Data.Generics
import Data.Data
import qualified Data.Map as Map
import System.IO(hGetContents, openFile, IOMode(WriteMode), hClose)
import Control.Exception(bracket)
import Debug.Trace
import Text.Printf

import Control.Monad.State.Strict hiding (lift)

{-
generateConstructor :: String -> [ClauseItem] -> Q Con
generateConstructor cname items = normalC (mkName cname) elements
    where elements = map (\item -> (strictType notStrict (conT (mkName (itemName item)))))
                         (filter (\item -> case item of
                                                Id _ -> True
                                                _    -> False)
                                 items)

generateData :: Rule -> Q Dec
generateData (Rule (Id left) clauses b) =
  dataD (cxt []) (mkName left) []
    (map (\(cl, inf) -> generateConstructor (clauseName inf) cl) clauses) []

generateAST :: Grammar -> Q String
generateAST (Grammar _ rules) = 
    do
        res <- mapM (runQ . generateData) (filter (isUpper . head . getIdStr . getRuleName) rules)
        return $ foldl ((++) . (++ "\n")) "" $ map pprint res

itemName item = case item of
                    Id "str" -> "String"
                    Id "id" -> "Id"
                    Id name -> name
                    StrLit str -> translateStrLiteral str
                    _ -> ""

translateStrLiteral :: String -> String
translateStrLiteral str = foldr (++) "" (map (\chr -> case chr of
                                                '+' -> "_plus_"
                                                '-' -> "_minus_"
                                                '.' -> "_dot_"
                                                '[' -> "_sq_bkt_l_"
                                                ']' -> "_sq_bkt_r_"
                                                ';' -> "_semi_"
                                                ':' -> "_colon_"
                                                '=' -> "_eql_"
                                                '*' -> "_star_"
                                                '|' -> "_pipe_"
                                                '$' -> "_dollar_"
                                                _   -> [chr])
                                             str)

annotateGrammarWithNames :: Grammar -> Grammar
annotateGrammarWithNames grammar =
    let clauseT (items, info) = (items, info{clauseName = "Node__" ++ (foldr (++) (ruleName info) (map itemName items))})
      in everywhere (mkT clauseT) grammar

emitLoopsInGrammar :: Grammar -> Grammar
emitLoopsInGrammar grammar =
    let clauseT self@(items, info) =
                                     if (not $ null $ ruleName info) && (isUpper $ head $ ruleName info)
                                       then
                                         case items of
                                           [ci1, op, ci2] ->
                                                 case op of
                                                   Plus -> ([LoopPlus ci1 $ Just ci2], info)
                                                   Star -> ([LoopStar ci1 $ Just ci2], info)
                                                   _ -> self
                                           [ci1, op] ->
                                                 case op of
                                                   Plus -> ([LoopPlus ci1 Nothing], info)
                                                   Star -> ([LoopStar ci1 Nothing], info)
                                                   _ -> self
                                           _ -> self
                                       else self
      in everywhere (mkT clauseT) grammar

writeHaskellFile fileName contents = writeFile (fileName ++ ".hs") ((generateHaskellFileHeader fileName) ++ contents)

genASTAdd =
  do
    let dataName = mkName "Id"
    idDef <-runQ $ dataD (cxt []) dataName [] [normalC dataName [strictType notStrict (conT (mkName "String"))]] []
    return $ pprint idDef

generateHaskellFileHeader fileName = "module " ++ fileName ++ " where\n"

generateHaskellFileImports :: [String] -> String
generateHaskellFileImports moduleNames = generateHaskellFileImports' moduleNames ""

generateHaskellFileImports' (moduleName : rest) result = generateHaskellFileImports' rest (result ++ "\nimport " ++ moduleName ++ "\n")
generateHaskellFileImports' [] result = result

generateASTFile :: String -> Grammar -> IO ()
generateASTFile fileName grammar = 
  do
    astStr::String <- runQ $ generateAST grammar
    astAddDef::String <- runQ $ genASTAdd 
    writeHaskellFile fileName $ astStr ++ "\n" ++ astAddDef ++ "\n"

generateQuotationFunctions :: Grammar -> Q String
generateQuotationFunctions (Grammar _ rules) =
  do
    let genQFRule rule =
                        let ruleNameStr = getIdStr $ getRuleName rule
                            funNameStr = (toLower $ head ruleNameStr) : (tail ruleNameStr)
                            funName = mkName $ funNameStr 
                            accessorName = mkName $ "get" ++ ruleNameStr
                            varStrLit = (litE (stringL $ "$" ++ funNameStr))
                            quoteExpr = (appE (appE (conE (mkName "QuoteExprExp")) varStrLit) (varE accessorName))
                            quotePat = (appE (appE (conE (mkName "QuoteExprPat")) varStrLit) (varE accessorName))
                            funExpr = (normalB (appE (appE (conE (mkName "QuasiQuoter")) quoteExpr) quotePat))
                          in funD funName [clause [] funExpr []]
    res <- mapM (runQ . genQFRule) rules
    return $ foldl ((++) . (++ "\n")) "" $ map pprint res

generateQQFile :: String -> Grammar -> IO ()
generateQQFile fileName grammar =
  do
    quoteFuns::String <- runQ $ generateQuotationFunctions grammar
    let imports = generateHaskellFileImports ["Data.Generics", "Data.Data", "Language.Haskell.TH", "Language.Haskell.TH.Quote"]
    writeHaskellFile fileName $ imports ++ quoteFuns ++ "\n"
    return ()

--------------------- start rule generation ---------------------------

addStartRule :: Grammar -> Grammar
addStartRule (Grammar name rules) = Grammar name ((makeStartRule rules) : rules)
    where makeStartRule rules = Rule (Id "start") (map makeRuleAlt rules) True
          makeRuleAlt (Rule (Id name) _ _) = (([ marker, (Id name), marker ]), mkGInfo)
                                             where marker = StrLit ("$" ++ name)

--------------------- parser specification generation -----------------

glue :: [String] -> String
glue strs = foldl (++) "" strs

glueD :: String -> [String] -> String
glueD delimiter strs = foldl (\x accum -> x ++ delimiter ++ accum) "" strs

glueDL :: String -> String -> [String] -> String
glueDL delimiter last_delimiter strs = case strs of
                                            []     -> last_delimiter
                                            (s:[]) -> s ++ last_delimiter
                                            (s:xs) -> s ++ delimiter ++ (glueDL delimiter last_delimiter xs)


data TokenInfo = TokenInfo ParserId LexerASTId
type ParserId = String
type LexerASTId = String

type StringLiteralsMap = Map.Map String TokenInfo

makeStringLiteralsMap :: Grammar -> StringLiteralsMap
makeStringLiteralsMap g = Map.fromList (everything (++) ([] `mkQ` f) g)
    where f (StrLit str) = [(str, TokenInfo parser_id ast_id)]
            where ast_id = "Tk_" ++ translateStrLiteral str
                  parser_id = "'" ++ str ++ "'"
          f _ = []

generateParserSpec grammar = "%token\n" ++
                             (glueD "\n" tokens) ++
                             "\n\n%%\n" ++
                             (glueD "\n" rules)
    where literals_map = makeStringLiteralsMap grammar
          tokens = map (\(TokenInfo parser_id ast_id) -> printf "%-20s { L.%s }" parser_id ast_id)
                       (Map.elems literals_map)
          rules = map genRule (getRules grammar)
          genRule (Rule (Id name) clauses _) = name ++ " :\n" ++ (glueDL " |\n" ";\n" (map genClause clauses))
          genClause (items, info) = printf "    %-40s { %s }" (genParserAlt items) (genConstructor items info)
          genParserAlt items = glueD " " (map (\item -> case item of
                                                            Id name -> name
                                                            StrLit str -> case Map.lookup str literals_map of
                                                                            Just (TokenInfo parser_id _) -> parser_id
                                                                            Nothing -> error "that's the shit"
                                                            _ -> "")
                                              items)
          genConstructor items (GInfo name _) = name ++ glueD " " (enumerateItems 1 items [])
                         where enumerateItems count (x:xs) accum =
                                              case x of
                                                   Id _ -> enumerateItems (count + 1) xs (("$" ++ (show count)) : accum)
                                                   _    -> enumerateItems (count + 1) xs accum
                               enumerateItems _ [] accum = reverse accum
-}
{-
type DataTypeName = String
type ConstructorName = String
data RuleASTNode = RuleASTNode DataTypeName [(ConstructorName, NodeType)]
data NodeType

data RuleParser = RuleParser  [ParseInfo]
-}

type DataTypeName = String
type ConstructorName = String

data ASTNode = ASTNode DataTypeName [ASTAlternative]
data ASTAlternative = ASTAlternative ConstructorName [NodeType]
data NodeType = NTList NodeType
              | NTNamedType String

--data RuleParser = RuleParser  [ParseInfo]

addDefaults :: InitialGrammar -> NormalGrammar
addDefaults (Grammar str rules) = Grammar str $ map addInRule rules
    where addInRule (Rule Nothing    Nothing     rname clause) | isLexicalRule rname = Rule "String" "id" rname clause
                                                               | otherwise           = Rule rname "" rname clause
          addInRule (Rule Nothing    (Just func) rname clause) | isLexicalRule rname = Rule "String" func rname clause
                                                               | otherwise           = error $ "Data transformation function specified for non-lexical rule " ++ rname ++ " : " ++ func
          addInRule (Rule (Just dtn) Nothing     rname clause) | isLexicalRule rname = Rule dtn "read" rname clause
                                                               | otherwise           = Rule dtn "" rname clause
          addInRule (Rule (Just dtn) (Just func) rname clause) | isLexicalRule rname = Rule dtn func rname clause
                                                               | otherwise           = error $ "Data transformation function specified for non-lexical rule " ++ rname ++ " : " ++ func
{-
type StringLiteralsMap = Map.Map String 

makeStringLiteralsMap :: NormalGrammar -> StringLiteralsMap
makeStringLiteralsMap g = Map.fromList (everything (++) ([] `mkQ` f) $ filter (not.isLexicalRule.getRuleName) $ getRules g)
    where f (StrLit str) = [(str, TokenInfo parser_id ast_id)]
            where ast_id = "Tk_" ++ translateStrLiteral str
                  parser_id = "'" ++ str ++ "'"
          f _ = []
-}

