{-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, RecursiveDo, QuasiQuotes, GeneralizedNewtypeDeriving,
   StandaloneDeriving #-}
module GenXY(XYGen, runParserGen, runParserGenRec)
    where

import GenClasses
import Parser

import Control.Monad.State.Lazy
import Data.Lens.Common
import Data.Lens.Template
import MonadFuture
import Data.Maybe
import Debug.Trace
import DocQuote
import Text.PrettyPrint
import Utils
import Control.Monad.Trans

import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M
import qualified RefTable as R

type RuleRef = R.Ref YParser

data TokenDef = TokenDef String IClause LexRuleAction
    deriving Show

deriving instance Show LexRuleAction

data MacroDef = MacroDef String IClause 
    deriving Show

data RuleDef = Alt String Bool [Seq]
    deriving Show

data Seq = Seq [RuleRef] Action
    deriving Show

data Action = Cons Int Int
            | Nil
            | AList Int
            | Simple Elem
            | AJust Elem
            | ANothing
            | Constructor ConstructorName [Elem]
    deriving Show

data Elem = Var Int
          | List Int
    deriving Show

data YParser = YPToken TokenDef
             | YPMacro MacroDef
             | YPRule RuleDef
    deriving Show

data XYState = XYState {
                        _tokDefs :: [TokenDef],
                        _macroDefs :: [MacroDef],
                        _tokenMap :: M.Map String RuleRef,
                        _rules :: [RuleDef],
                        _ruleTab :: R.RefTable YParser,
                        _nameMap :: M.Map String RuleRef,
                        _nameCounter :: Int
                       }

$(makeLens ''XYState)

newtype XYGen m a = XYGen { fromXYGen :: FutureT XYState m a }
    deriving (Monad, MonadFuture XYState, MonadCond, MonadTrans)

deriving instance MonadFix m => MonadFix (XYGen m)

instance Monad m => NameGen (XYGen m) where
    newName = newNameWithCounter nameCounter

yName :: YParser -> String
yName (YPToken (TokenDef n _ _)) = n
yName (YPMacro (MacroDef n _)) = n
yName (YPRule (Alt n _ _)) = n

addParser :: Monad m => YParser -> XYGen m ()
addParser (YPToken tok) = do
  tokDefs %= (tok :)
  return ()
addParser (YPMacro mac) = do
  macroDefs %= (mac :)
  return ()
addParser (YPRule rule) = do
  rules %= (rule :)
  return ()

newRuleRef :: (Monad m, MonadGenError m) => Bool -> YParser -> XYGen m RuleRef
newRuleRef addName parser = do
  let name = yName parser
  addParser parser
  ref <- R.newRef ruleTab parser
  if addName
     then do
           nmap <- present nameMap
           logError $ case M.lookup (yName parser) nmap of
                        Just _ -> Just $ "Rule " ++ (yName parser) ++ " is redefined"
                        Nothing -> Nothing
           nameMap %= M.insert (yName parser) ref
     else return () 
  return ref

parserDefToRule :: (Monad m, MonadFix m, MonadGenError m) => RuleRef -> String -> ParserDef (XYGen m) -> XYGen m RuleDef
parserDefToRule rulePs name (PAlt alts) = do
  newAlts <- mapM pSeqToSeq alts
  return $ Alt name False newAlts
parserDefToRule rulePs name (POpt parser) = do
  elem <- elemForParser 0 parser
  return $ Alt name False [Seq [parser] $ AJust elem, Seq [] $ ANothing]
parserDefToRule rulePs name (PMany parser PStar Nothing) = do
  return $ Alt name True [Seq [rulePs, parser] $ Cons 1 0, Seq [] $ Nil]  
parserDefToRule rulePs name (PMany parser PStar (Just sepP)) = do
  plusRule <- addClause Nothing (PMany parser PPlus (Just sepP))
  return $ Alt name True [Seq [plusRule] $ Simple $ Var 0, Seq [] $ Nil]  
parserDefToRule rulePs name (PMany parser PPlus Nothing) = do
  return $ Alt name True [Seq [rulePs, parser] $ Cons 1 0, Seq [parser] $ AList 0]
parserDefToRule rulePs name (PMany parser PPlus (Just sepP)) = do
  return $ Alt name True [Seq [rulePs, sepP, parser] $ Cons 2 0, Seq [parser] $ AList 0]

elemForParser :: (Monad m, MonadFix m, MonadGenError m) => Int -> RuleRef -> XYGen m Elem
elemForParser num ref = do
  ~(Just parser) <- R.lookup future ref ruleTab
  return $ case parser of
             (YPRule (Alt _ True _)) -> List num
             _ -> Var num

pSeqToSeq :: (Monad m, MonadFix m, MonadGenError m) => PSeq (XYGen m) -> XYGen m Seq
pSeqToSeq (POne parsers ind) = do
  elem <- elemForParser ind (parsers !! ind)
  return $ Seq parsers (Simple elem)
pSeqToSeq (PSeq parsers constrName inds) = do
  elems <- mapM (\ i -> elemForParser i (parsers !! i)) inds
  return $ Seq parsers (Constructor constrName elems)

runParserGen :: (Monad m) => XYState -> XYGen m a -> m a
runParserGen st xyGen = runFutureT st (fromXYGen xyGen)

runParserGenRec :: (MonadFix m, Monad m, MonadGenError m) => XYGen m a -> m (a, XYState)
runParserGenRec xyGen = runFutureTRec startXYState (fromXYGen xyGen)
 where
   startXYState = XYState {
                           _tokDefs = [],
                           _macroDefs = [],
                           _tokenMap = M.empty,
                           _rules = [],
                           _ruleTab = R.emptyWithError (YPToken $ TokenDef "Error" (IStrLit "Error") LIgnore),
                           _nameMap = M.empty,
                           _nameCounter = 0
                          }

instance (Monad m, ContentGen m) => ContentGen (XYGen m) where
    generateContent nm = do
      cont <- lift $ generateContent nm
      sortTokenDefs
      xText <- genX nm
      yText <- genY nm
      return $ (nm ++ "Parser.y", yText) : (nm ++ "Lexer.x", xText) : cont

instance (Monad m, MonadFix m, MonadGenError m) => ParserGen (XYGen m) where
    type Parser (XYGen m) = RuleRef 
        
    --addClause :: Maybe RuleName -> ParserDef p -> p (Parser p)
    addClause maybeN def = do
      name <- ensureName maybeN "Rule_"
      rec ruleDef <- parserDefToRule rulePs name def
          rulePs <- newRuleRef (isJust maybeN) $ YPRule ruleDef
      return rulePs

    -- addLexRule :: Maybe RuleName -> LexRuleAction -> IClause -> p (Parser p)
    addLexRule maybeN lact clause = do
      name <- ensureName maybeN "tok_"
      newRuleRef (isJust maybeN) $ YPToken $ TokenDef name clause lact
      
    --addLexMacro :: Maybe RuleName -> IClause -> p (Parser p)
    addLexMacro maybeN clause = do
      name <- ensureName maybeN "macro_"
      newRuleRef (isJust maybeN) $ YPMacro $ MacroDef name clause

    --addToken :: String -> p (Parser p)
    addToken str = do
      oldTok <- present tokenMap >>= return . M.lookup str
      case oldTok of
        Just r -> return r
        Nothing -> do
                    name <- newName "kw_"
                    res <- newRuleRef False $ YPToken $ TokenDef name (IStrLit str) LNoData
                    tokenMap %= M.insert str res
                    return res

    --getParser :: RuleName -> p (Parser p)
    getParser name = do
      -- TODO: Add error handling
      mref <- future nameMap >>= return . (M.lookup name)
      checkMaybeRef mref R.errorRef ("Rule " ++ name ++ " is undefined")

instance (ASTGen m, MonadFix m) => ASTGen (XYGen m) where
    type ASTType (XYGen m) = ASTType m
    type ASTConstructor (XYGen m) = ASTConstructor m

    --addASTType :: Maybe RuleName -> ASTTypeDecl a -> a (ASTType a)
    addASTType mrn tp = lift $ addASTType mrn tp

    --addSeqToASTType :: ASTType a -> Maybe ConstructorName -> [ASTType a] -> a (ASTConstructor a)
    addSeqToASTType tp maybeN typeRefs = lift $ addSeqToASTType tp maybeN typeRefs

    --getRuleASTType :: RuleName -> a (Maybe (ASTType a))
    getRuleASTType = lift . getRuleASTType

    --getASTType :: RuleName -> a (ASTType a)
    getASTType = lift . getASTType

    --getConstructorName :: ASTConstructor a -> a ConstructorName
    getConstructorName = lift . getConstructorName

    --getASTTypeDecl :: (ASTType a) -> a (ASTTypeDecl (ASTType a))
    getASTTypeDecl = lift . getASTTypeDecl     

instance (MonadGenError m) => MonadGenError (XYGen m) where
    logError mErr = lift $ logError mErr
    getErrors = lift getErrors

----- X File Generation routines

tokenName :: String -> String
tokenName name = "Tk__" ++ name

genX :: Monad m => String -> XYGen m String
genX grammarName = do
  tokens <- genTokens
  macros <- genMacros
  adt <- genADT
  return $ render [doc|{
module ?grammarName~Lexer(alexScanTokens, Token(..))
    where

}
%wrapper "monad"

??macros

??tokens

{
??adt

alexEOF = return EndOfFile
alexScanTokens :: String -> [Token]
alexScanTokens str =
               case alexScanTokens1 str of
                  Right toks -> toks
                  Left err -> error err

alexScanTokens1 str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                       EndOfFile -> return $ reverse toks
                       _ -> let toks' = tok : toks
                            in toks' `seq` loop toks'
  loop []
simple1 :: (String -> Token) -> AlexInput -> Int -> Alex Token
simple1 t (_, _, _, str) len = return $ t (take len str)

simple :: Token -> AlexInput -> Int -> Alex Token
simple t input len = return t

rtkError ((AlexPn _ line column), _, _, str) len = alexError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column" ++ ". Following chars :" ++ (take 10 str)

}
|]

genTokens :: Monad m => XYGen m Doc
genTokens = do
  toks <- present tokDefs
  tokenDefs <- mapM genTokDef toks
  return $ text "tokens :- " <+> vcat (tokenDefs ++ [text ". {rtkError}"])

genTokDef :: Monad m => TokenDef -> XYGen m Doc
genTokDef (TokenDef name clause lra) = do
  clauseText <- translateClause clause
  let tname = tokenName name
  case lra of
    LIgnore -> return $ [doc|??clauseText ; |]
    LNoData -> return $ [doc|??clauseText { simple ?tname } |]
    LData func tp -> return $ [doc|??clauseText { simple1 $ ?tname . (?func~) } |]

genMacros :: Monad m => XYGen m Doc
genMacros = do
  macros <- present macroDefs
  macroDefTexts <- mapM genMacroDef macros
  return $ vcat macroDefTexts

genMacroDef :: Monad m => MacroDef -> XYGen m Doc
genMacroDef (MacroDef name clause) = do
  clauseText <- translateClauseForMacro clause
  let tname = tokenName name
  return [doc|$?tname = ??clauseText|]

genADT :: Monad m => XYGen m Doc
genADT = do
  toks <- present tokDefs
  tokConstrs <- mapM genTokConstr toks
  return $ text "data Token = " <+> ((joinAlts ([text "EndOfFile"] ++ tokConstrs)) $$ text "deriving Show")

genTokConstr :: Monad m => TokenDef -> XYGen m Doc
genTokConstr (TokenDef name _ lra) = do
  let tname = tokenName name
  case lra of
    LIgnore -> return $ [doc|?tname |]
    LNoData -> return $ [doc|?tname |]
    LData func tp -> return $ [doc|?tname ?tp |]

backquoteStr :: String -> String
backquoteStr str = concat (map (\chr -> if (case chr of
                                                 '"'  -> True
                                                 _    -> False)
                                          then ['\\', chr]
                                          else [chr] )
                                  str)

backquoteStrInBrackets :: String -> String
backquoteStrInBrackets str = concat (map (\chr -> if (case chr of
                                                        '[' -> True
                                                        ']' -> True
                                                        ' ' -> True
                                                        '*' -> True
                                                        '/' -> True
                                                        '{' -> True
                                                        '}' -> True
                                                        '$' -> True
                                                        _   -> False)
                                          then ['\\', chr]
                                          else [chr] )
                                  str)

                                          
translateClauseForMacro :: Monad m => IClause -> XYGen m Doc
translateClauseForMacro (IStrLit s) = return $ text s
translateClauseForMacro (IRegExpLit re) = return $ brackets $ text $ backquoteStrInBrackets re
translateClauseForMacro (ISeq cls) = do
  trs <- mapM translateClauseForMacro cls
  return $ hsep $ punctuate (text " ") trs
translateClauseForMacro (IAlt clauses) = do
  trs <- mapM translateClauseForMacro clauses
  return $ hsep $ punctuate (text "|") trs
translateClauseForMacro cl = return $ text $ "Cannot translate to macro def " ++ (show cl)

translateClause :: Monad m => IClause -> XYGen m Doc
translateClause (IId tname) = do
  Just ref <- present nameMap >>= return . (M.lookup tname)
  Just yparser <- R.lookup present ref ruleTab
  let name = tokenName tname
  case yparser of
    YPToken _ -> return [doc| @?name |]
    YPMacro _ -> return [doc| $?name |]
translateClause (IStrLit s)         = return $ doubleQuotes $ text $ backquoteStr s
translateClause (IDot)              = return $ text "."
translateClause (IRegExpLit re)     = return $ brackets $ text $ backquoteStrInBrackets re
translateClause (IStar cl Nothing)  = do
  cl <- translateClause cl 
  return [doc| ??cl * |]
translateClause (IStar cl (Just _)) = error $ "Star (*) clauses with delimiters are not supported in lexical rules"
translateClause (IPlus cl Nothing)  = do
  cl <- translateClause cl
  return [doc| ??cl + |]
translateClause (IPlus cl (Just _)) = error $ "Plus (+) clauses with delimiters are not supported in lexical rules"
translateClause (IAlt clauses)      = do
  docs <- mapM translateClause clauses
  return $ parens $ hsep $ punctuate (text "|") docs
translateClause (ISeq clauses)    = do
  docs <- mapM translateClause clauses
  return $ hsep $ punctuate (text " ") docs
translateClause (IOpt clause)       = do 
  cl <- translateClause clause
  return [doc| ??cl ? |]
translateClause cl                 = error $ "Can't translate to lexer spec: " ++ (show cl)

joinAlts :: [Doc] -> Doc
joinAlts alts = vcat $ punctuate (text " |") (filter (not.isEmpty) alts)

----- Y File Generation routines

genY :: Monad m => String -> XYGen m String
genY name = do
  tokensDoc <- genYTokens
  rulesDoc <- genYRules
  return $ render [doc|{
{-# LANGUAGE DeriveDataTypeable #-}
module ?name~Parser
    where

import qualified Data.Generics as Gen
import qualified ?name~Lexer as L
import ?name~AST
}

%name parse?name
%tokentype {L.Token}
%error { \rest -> error $ "Parse error " ++ show rest}

%token
??tokensDoc

%%
??rulesDoc

{
}
|]

tokDefSimple :: TokenDef -> Bool
tokDefSimple (TokenDef _ _ LIgnore) = True
tokDefSimple (TokenDef _ _ LNoData) = True
tokDefSimple _ = False

sortTokenDefs' :: [TokenDef] -> [TokenDef]
sortTokenDefs' toks' = let toks = reverse toks'
                       in filter tokDefSimple toks ++ filter (not . tokDefSimple) toks

sortTokenDefs :: Monad m => XYGen m ()
sortTokenDefs = tokDefs %= sortTokenDefs'

genYTokens :: Monad m => XYGen m Doc
genYTokens = do
  toks <- present tokDefs
  defs <- mapM genYTokDef toks
  return $ vcat $ defs

genYTokDef :: Monad m => TokenDef -> XYGen m Doc
genYTokDef (TokenDef name clause lra) = do
  let tname = tokenName name
  case lra of
    LIgnore -> return $ empty
    LNoData -> return $ [doc|?name { L.?tname } |]
    LData func tp -> return $ [doc|?name { L.?tname $$ } |]

genYRules :: Monad m => XYGen m Doc
genYRules = do
  rls <- present rules
  defs <- mapM genYRule $ reverse rls
  return $ vcat $ punctuate (text "") $ defs

genYRule :: Monad m => RuleDef -> XYGen m Doc
genYRule (Alt rName _ seqs) = do
  seqDefs <- mapM genYSeq seqs
  return $ [doc|?rName : |] <> (joinAlts seqDefs)

elemStr :: Elem -> String
elemStr (Var i) = "$" ++ show (i + 1)
elemStr (List i) = "(reverse $" ++ show (i + 1) ++ ")"

genYSeq :: Monad m => Seq -> XYGen m Doc
genYSeq (Seq ruleRefs act) = do
  ruleNames <- mapM (\ref -> R.lookup present ref ruleTab >>= return . yName . fromJust) ruleRefs
  let parseDoc = hsep $ map text ruleNames
  let actionDoc = case act of
                    Constructor nm elems -> text nm <+> hsep ( map (text . elemStr) elems )
                    Simple elem -> text $ elemStr elem
                    Nil -> text "[]"
                    Cons a b -> [doc|$?(show $ a + 1) : $?(show $ b + 1)|]
                    AList i -> [doc|[$?(show $ i + 1)]|]
                    AJust elem -> [doc|Just ?(elemStr elem)|]
                    ANothing -> text "Nothing"
  return [doc|??parseDoc { ??actionDoc }|]
