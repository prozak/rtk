{-# LANGUAGE TemplateHaskell #-}

module Grammar where

import Parser
import Language.Haskell.TH
import Data.Char

generateConstructor :: [ClauseItem] -> Q Con
generateConstructor clauseItems = normalC (mkName "TT") []

generateData :: Rule -> Q Dec
generateData (Rule (Id left) clauses b) = dataD (cxt []) (mkName left) [] (map generateConstructor clauses) []

generateAST :: Grammar -> Q String
generateAST (Grammar _ rules) = 
    do
        res <- mapM (runQ . generateData) (filter (isUpper . head . getIdStr . getRuleName) rules)
        return $ foldl (\prev str -> prev ++ str ++ "\n") "" $ map pprint res
