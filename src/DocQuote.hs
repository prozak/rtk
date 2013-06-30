{- Taken from http://www.haskell.org/haskellwiki/Poor_man's_here_document -}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module DocQuote(doc) where
 
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Data.Generics
import Data.Data
import Text.PrettyPrint
import Data.Maybe
import Data.String.Utils
import Data.List
import Text.PrettyPrint

getCodeToRunHelper [] _ result = Nothing
getCodeToRunHelper (')' : rest) 0 result = Just $ result + 1
getCodeToRunHelper (')' : rest) bcounter result = getCodeToRunHelper rest (bcounter - 1) (1 + result)
getCodeToRunHelper ('(' : rest) bcounter result = getCodeToRunHelper rest (bcounter + 1) (1 + result)
getCodeToRunHelper (c : rest) bcounter result = getCodeToRunHelper rest bcounter (1 + result)

getCodeToRun "" = Nothing
getCodeToRun ('?' : '(' : rest) = getCodeToRunHelper rest 0 3
getCodeToRun ('(' : rest) = getCodeToRunHelper rest 0 2
getCodeToRun _ = Nothing

processVarAndAfter varAndAfter =
  case getCodeToRun $ tail varAndAfter of
    Nothing ->
      let mbSpaceInd = findIndex (\c -> (c == ' ') || (c == '\n')) varAndAfter
          mbTildInd = findIndex ((==) '~') varAndAfter
      in
        case mbSpaceInd of
          Nothing ->
            case mbTildInd of
              Nothing -> (varAndAfter, "")
              Just tildInd ->
                  let (var, tildAndAfter) = splitAt tildInd varAndAfter in (var, tail tildAndAfter)
          Just spaceInd ->
            case mbTildInd of
              Nothing -> splitAt spaceInd varAndAfter
              Just tildInd ->
                if tildInd < spaceInd
                  then let (var, tildAndAfter) = splitAt tildInd varAndAfter in (var, tail tildAndAfter)
                  else splitAt spaceInd varAndAfter
    Just ind -> splitAt ind varAndAfter

splitStringByVarsHelper [] result = result
splitStringByVarsHelper str result =
  case elemIndex '?' str of
    Nothing -> str : result
    Just ind ->
      let (beforeVar, varAndAfter) = splitAt ind str
          (var, afterVar) = processVarAndAfter varAndAfter
        in splitStringByVarsHelper afterVar (var : beforeVar : result)

translateQName qname =
    case qname of
      UnQual (Ident str) -> mkName str
      UnQual (Symbol str) -> mkName str

translateQOp qop =
    case qop of
      QVarOp qname -> global $ translateQName qname

convertToTHHelper (Paren e) = convertToTHHelper e
convertToTHHelper (App e1 e2) = appE (convertToTHHelper e1) (convertToTHHelper e2)
convertToTHHelper (InfixApp e1 qop e2) = uInfixE (convertToTHHelper e1) (translateQOp qop) (convertToTHHelper e2)
convertToTHHelper (Var qnm) = global $ translateQName qnm
convertToTHHelper (Lit (String str)) = stringE str
convertToTHHelper (Lit (Int i)) = litE $ integerL $ toInteger i
convertToTHHelper (Lit (Char c)) = litE $ charL c
convertToTHHelper (List es) = listE $ map convertToTHHelper es 

convertToTH result = 
    case result of
      ParseOk expr -> convertToTHHelper expr
      ParseFailed loc str -> stringE ("<" ++ str ++ ":" ++ show (srcColumn loc) ++ ">")

splitStringByVars str =
  let vStrs = lines str
      makeHDoc str prevExpr =
             case str of
               "?()" -> [|$prevExpr <> text $(stringE "<empty expr>")|]
               '?' : '?' : '(' : rest ->
                      let lifted = convertToTH $ parseExp $ '(' : rest--(appE (global (mkName "liftString")) (stringE $ '(' : rest))
                        in [|$prevExpr <> $lifted|]
               '?' : '(' : rest ->
                      let lifted = convertToTH $ parseExp $ '(' : rest--(appE (global (mkName "liftString")) (stringE $ '(' : rest))
                        in [|$prevExpr <> text $lifted|]
               "?" -> [|$prevExpr <> text $(stringE "<empty var name>")|]
               '?' : '?' : rest ->
                      [|$prevExpr <> $(varE $ mkName rest)|]
               '?' : rest ->
                      [|$prevExpr <> text $(varE $ mkName rest)|]
               _ -> [|$prevExpr <> text $(stringE str)|]
      makeVDoc str prevExpr = 
          let result = splitStringByVarsHelper (replace "|~]" "|]" str) []
              e = foldr makeHDoc
                  [| text "" |] result 
          in [| $e $$ $prevExpr |]
  in foldr makeVDoc
           [| Text.PrettyPrint.empty |] vStrs
    
doc = QuasiQuoter { quoteExp = splitStringByVars, quotePat = litP . stringL, quoteType = \s -> return ListT, quoteDec = \s -> return [] }
