{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Grammar where

import Parser
import Language.Haskell.TH
import Data.Char
import IO(hGetContents, bracket, openFile, IOMode(WriteMode), hClose)

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

generateConstructor :: String -> [ClauseItem] -> Q Con
generateConstructor cname items = normalC (mkName cname) elements
    where elements = map (\item -> (strictType notStrict (conT (mkName (itemName item)))))
                         (filter (\item -> case item of
                                                Id _ -> True
                                                _    -> False)
                                 items)

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
                                                _   -> [chr])
                                             str)

annotateClauseWithNames :: String -> ([ClauseItem], GInfo) -> ([ClauseItem], GInfo)
annotateClauseWithNames base_name (items, info) =
    (items, info{clauseName = "Node__" ++ (foldr (++) base_name (map itemName items))})

annotateRulesWithNames :: Rule -> Rule
annotateRulesWithNames rule = rule{getClauses = map (annotateClauseWithNames $ getIdStr $ getRuleName rule) $ getClauses rule}
    
annotateGrammarWithNames :: Grammar -> Grammar
annotateGrammarWithNames (Grammar name rules) = Grammar name (map annotateRulesWithNames rules)

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
