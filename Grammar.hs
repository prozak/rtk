{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Grammar where

import Parser
import Language.Haskell.TH
import Data.Char
import IO(hGetContents, bracket, openFile, IOMode(WriteMode), hClose)

--generateConstructor :: [ClauseItem] -> Q Con
--generateConstructor clauseItems = normalC (mkName "TT") []

generateData :: Rule -> Q Dec
generateData (Rule (Id left) clauses b) =
  dataD (cxt []) (mkName left) []
    (map (\(cl, inf) -> generateConstructor (clauseName inf) cl) clauses) []

generateAST :: Grammar -> Q String
generateAST (Grammar _ rules) = 
    do
        res <- mapM (runQ . generateData) (filter (isUpper . head . getIdStr . getRuleName) rules)
        return $ foldl (\prev str -> prev ++ str ++ "\n") "" $ map pprint res


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
annotateClauseWithNames base_name (items, info) = (items, info{clauseName = "Node__" ++ (foldr (++) base_name (map itemName items))})

annotateRulesWithNames :: Rule -> Rule
annotateRulesWithNames rule = rule{getClauses = map (annotateClauseWithNames $ getIdStr $ getRuleName rule) $ getClauses rule}
    
annotateGrammarWithNames :: Grammar -> Grammar
annotateGrammarWithNames (Grammar name rules) = Grammar name (map annotateRulesWithNames rules)

writeHaskellFile fileName contents = writeFile (fileName ++ ".hs") contents

genASTAdd =
  do
    idDef <-runQ $ dataD (cxt []) (mkName "Id") [] [normalC (mkName "Id") [strictType notStrict (conT (mkName "String"))]] []
    return $ pprint idDef

generateASTFile :: String -> Grammar -> IO()
generateASTFile fileName grammar = 
  do
    astStr::String <- runQ $ generateAST grammar
    astAddDef::String <- runQ $ genASTAdd 
    writeHaskellFile fileName $ "module " ++ fileName ++ " where\n" ++ astStr ++ "\n" ++ astAddDef ++ "\n"
