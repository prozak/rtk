{- Taken from http://www.haskell.org/haskellwiki/Poor_man's_here_document -}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module StrQuote(str) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Data.String.Utils
import Data.List

getCodeToRunHelper :: (Eq t1, Num t1, Num t2) => [Char] -> t1 -> t2 -> Maybe t2
getCodeToRunHelper [] _ _ = Nothing
getCodeToRunHelper (')' : _) 0 result = Just $ result + 1
getCodeToRunHelper (')' : rest) bcounter result = getCodeToRunHelper rest (bcounter - 1) (1 + result)
getCodeToRunHelper ('(' : rest) bcounter result = getCodeToRunHelper rest (bcounter + 1) (1 + result)
getCodeToRunHelper (_ : rest) bcounter result = getCodeToRunHelper rest bcounter (1 + result)

getCodeToRun :: Num a => String -> Maybe a
getCodeToRun "" = Nothing
getCodeToRun ('(' : rest) = getCodeToRunHelper rest 0 2
getCodeToRun _ = Nothing

processVarAndAfter :: [Char] -> ([Char], String)
processVarAndAfter varAndAfter =
  case getCodeToRun $ drop 1 varAndAfter of
    Nothing ->
      let mbSpaceInd = findIndex (\c -> (c == ' ') || (c == '\n')) varAndAfter
          mbTildInd = findIndex ((==) '~') varAndAfter
      in
        case mbSpaceInd of
          Nothing ->
            case mbTildInd of
              Nothing -> (varAndAfter, "")
              Just tildInd ->
                  let (var, tildAndAfter) = splitAt tildInd varAndAfter in (var, drop 1 tildAndAfter)
          Just spaceInd ->
            case mbTildInd of
              Nothing -> splitAt spaceInd varAndAfter
              Just tildInd ->
                if tildInd < spaceInd
                  then let (var, tildAndAfter) = splitAt tildInd varAndAfter in (var, drop 1 tildAndAfter)
                  else splitAt spaceInd varAndAfter
    Just ind -> splitAt ind varAndAfter

splitStringByVarsHelper :: String -> [[Char]] -> [[Char]]
splitStringByVarsHelper [] result = result
splitStringByVarsHelper inputStr result =
  case elemIndex '?' inputStr of
    Nothing -> inputStr : result
    Just ind ->
      let (beforeVar, varAndAfter) = splitAt ind inputStr
          (var, afterVar) = processVarAndAfter varAndAfter
        in splitStringByVarsHelper afterVar (var : beforeVar : result)

translateQName :: QName () -> Language.Haskell.TH.Name
translateQName qname =
    case qname of
      UnQual () (Ident () s) -> mkName s
      UnQual () (Symbol () s) -> mkName s
      Qual () _ (Ident () s) -> mkName s
      Qual () _ (Symbol () s) -> mkName s
      Special () (UnitCon ()) -> mkName "()"
      Special () (ListCon ()) -> mkName "[]"
      Special () (FunCon ()) -> mkName "->"
      Special () (TupleCon () _ n) -> mkName $ "(" ++ replicate (n-1) ',' ++ ")"
      Special () (Cons ()) -> mkName ":"
      Special () (UnboxedSingleCon ()) -> mkName "(# #)"
      Special () (ExprHole ()) -> mkName "_"

translateQOp :: Quote m => QOp () -> m Language.Haskell.TH.Exp
translateQOp qop =
    case qop of
      QVarOp () qname -> varE $ translateQName qname
      QConOp () qname -> conE $ translateQName qname

convertToTHHelper :: Quote m => Language.Haskell.Exts.Syntax.Exp () -> m Language.Haskell.TH.Exp
convertToTHHelper (Paren () e) = convertToTHHelper e
convertToTHHelper (App () e1 e2) = appE (convertToTHHelper e1) (convertToTHHelper e2)
convertToTHHelper (InfixApp () e1 qop e2) = uInfixE (convertToTHHelper e1) (translateQOp qop) (convertToTHHelper e2)
convertToTHHelper (Var () qnm) = varE $ translateQName qnm
convertToTHHelper (Lit () (String () s _)) = stringE s
convertToTHHelper e = stringE ("<unsupported expression: " ++ show e ++ ">")

convertToTH :: Quote m => ParseResult (Language.Haskell.Exts.Syntax.Exp ()) -> m Language.Haskell.TH.Exp
convertToTH result =
    case result of
      ParseOk expr -> convertToTHHelper expr
      ParseFailed loc errMsg -> stringE ("<" ++ errMsg ++ ":" ++ show loc ++ ">")

splitStringByVars :: Quote m => [Char] -> m Language.Haskell.TH.Exp
splitStringByVars inputString =
  let result = splitStringByVarsHelper (replace "|~]" "|]" inputString) []
  in foldr (\s prevExpr ->
             case s of
               "?()" -> [|$prevExpr ++ $(stringE "<empty expr>")|]
               '?' : '(' : rest ->
                      let lifted = convertToTH $ fmap (fmap (const ())) $ parseExp $ '(' : rest
                        in [|$prevExpr ++ $lifted|]
               "?" -> [|$prevExpr ++ $(stringE "<empty var name>")|]
               '?' : rest ->
                      [|$prevExpr ++ $(varE $ mkName rest)|]
               _ -> [|$prevExpr ++ $(stringE s)|])
           (stringE "") result

str :: QuasiQuoter
str = QuasiQuoter { quoteExp = splitStringByVars, quotePat = litP . stringL, quoteType = \_ -> return ListT, quoteDec = \_ -> return [] }
