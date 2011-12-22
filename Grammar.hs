{-# LANGUAGE TemplateHaskell #-}

module Grammar where

import Parser
import Language.Haskell.TH
import Data.Char

--generateConstructor :: [ClauseItem] -> Q Con
--generateConstructor clauseItems = normalC (mkName "TT") []

generateData :: Rule -> Q Dec
generateData (Rule (Id left) clauses b) = dataD (cxt []) (mkName left) [] (map (\cl -> generateConstructor left cl) clauses) []

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
generateConstructor base_name items = normalC (mkName ("Node__" ++ name)) elements
    where name = foldr (++) base_name (map itemName items)
          elements = map (\item -> (strictType notStrict (conT (mkName (itemName item)))))
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
                                                _   -> [chr])
                                             str)
