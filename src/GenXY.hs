{-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, RecursiveDo, QuasiQuotes #-}
module GenXY(XYGen, runParserGen, runParserGenRec)
    where

import GenClasses
import Parser

import Control.Monad.State.Lazy
import Data.Lens.Common
import Data.Lens.Template
import LensStateWrapper
import Data.Maybe
import Debug.Trace
import DocQuote
import Text.PrettyPrint

import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M

newtype RuleRef = RuleRef { fromRuleRef :: Int }
    deriving Show

data TokenDef = TokenDef String IClause LexRuleAction
    deriving Show

instance Show LexRuleAction

data MacroDef = MacroDef String IClause 
    deriving Show

data RuleDef = Alt String Bool [Seq]
    deriving Show

data Seq = Seq [RuleRef] Action
    deriving Show

data Action = Cons Int Int
            | Nil
            | Simple Elem
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
                        _curRef :: Int,
                        _rules :: [RuleDef],
                        _ruleMap :: IM.IntMap YParser,
                        _nameMap :: M.Map String RuleRef,
                        _nameCounter :: Int
                       }

$(makeLens ''XYState)

newtype XYGen m a = XYGen { fromXYGen :: StateT (XYState, XYState) m a }

instance Monad m => Monad (XYGen m) where
    return = XYGen . return
    a >>= b = XYGen (fromXYGen a >>= fromXYGen . b) 

instance Monad m => MonadState (XYState, XYState) (XYGen m) where
    get = XYGen get
    put = XYGen . put
    state = XYGen . state

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

newRuleRef :: Monad m => Bool -> YParser -> XYGen m RuleRef
newRuleRef addName parser = do
  ref <- access' curRef
  curRef %= (1 +)
  let name = yName parser
  addParser parser
  ruleMap %= IM.insert ref parser
  if addName
     then nameMap %= (trace ("add rule " ++ yName parser) $ M.insert (yName parser) (RuleRef ref))
     else return () 
  return $ RuleRef ref

newRuleName :: Monad m => RuleName -> XYGen m RuleName
newRuleName str = do
  ind <- access' nameCounter
  nameCounter %= (1 +)
  return $ str ++ show ind

parserDefToRule :: (Monad m, ASTGen m) => String -> ParserDef (XYGen m) -> XYGen m RuleDef
parserDefToRule name (PAlt alts) = do
  newAlts <- mapM pSeqToSeq alts
  return $ Alt name False newAlts

elemForParser :: (Monad m) => Int -> RuleRef -> XYGen m Elem
elemForParser num ref = do
  ~(Just parser) <- access' ruleMap >>= return . IM.lookup (fromRuleRef ref)
  return $ case parser of
             (YPRule (Alt _ True _)) -> List num
             _ -> Var num

pSeqToSeq :: (Monad m, ASTGen m) => PSeq (XYGen m) -> XYGen m Seq
pSeqToSeq (POne parsers ind) = do
  elem <- elemForParser ind (parsers !! ind)
  return $ Seq parsers (Simple elem)
pSeqToSeq (PSeq parsers constr inds) = do
  elems <- mapM (\ i -> elemForParser i (parsers !! i)) inds
  constrName <- liftAST $ getConstructorName constr
  return $ Seq parsers (Constructor constrName elems)

ensureRuleName :: Monad m => Maybe RuleName -> RuleName -> XYGen m RuleName
ensureRuleName maybeN str = case maybeN of
                              Just n -> return n
                              Nothing -> newRuleName str

runParserGen :: (Monad m) => XYState -> XYGen m a -> m a
runParserGen st xyGen = do
  ~(res, _) <- runStateT (fromXYGen xyGen) (st, st)
  return res

runParserGenRec :: (MonadFix m, Monad m) => XYGen m a -> m (a, XYState)
runParserGenRec xyGen = do
  rec ~(res, ~(_, outState)) <- runStateT (fromXYGen xyGen) (outState, startXYState)
  return (res, outState)
 where
   startXYState = XYState {
                           _tokDefs = [],
                           _macroDefs = [],
                           _curRef = 0,
                           _rules = [],
                           _ruleMap = IM.empty,
                           _nameMap = M.empty,
                           _nameCounter = 0
                          }

instance (Monad m, ASTGen m) => ContentGen (XYGen m) where
    generateContent nm = do
      cont <- liftAST $ generateContent nm
      xText <- genX nm
      yText <- genY nm
      return $ (nm ++ "Parser.y", yText) : (nm ++ "Lexer.x", xText) : cont

instance (Monad m, ASTGen m) => ParserGen (XYGen m) where
    type Parser (XYGen m) = RuleRef 
    type ParserAST (XYGen m) = m 
        
    --addClause :: ASTGen a => Maybe RuleName -> ParserDef p a -> p (Parser p a)
    addClause maybeN def = do
      name <- ensureRuleName maybeN "Rule_"
      ruleDef <- parserDefToRule name def
      newRuleRef (isJust maybeN) $ YPRule ruleDef

    -- addLexRule :: ASTGen a => Maybe RuleName -> LexRuleAction -> IClause -> p (Parser p a)
    addLexRule maybeN lact clause = do
      name <- ensureRuleName maybeN "tok_"
      newRuleRef (isJust maybeN) $ YPToken $ TokenDef name clause lact
      
    --addLexMacro :: ASTGen a => Maybe RuleName -> IClause -> p (Parser p a)
    addLexMacro maybeN clause = do
      name <- ensureRuleName maybeN "macro_"
      newRuleRef (isJust maybeN) $ YPMacro $ MacroDef name clause

    --addToken :: ASTGen a => String -> p (Parser p a)
    addToken str = do
      name <- newRuleName "kw_"
      newRuleRef False $ YPToken $ TokenDef name (IStrLit str) LNoData

    --getParser :: ASTGen a => RuleName -> p (Parser p a)
    getParser name = do
      -- TODO: Add error handling
      access nameMap >>= return . trace ("lookup " ++ name) . (M.lookup name)

    --liftAST :: ASTGen a => a c -> p c
    liftAST = XYGen . lift


instance (Monad m, ASTGen m) => ASTGen (XYGen m) where
    type ASTType (XYGen m) = ASTType m
    type ASTConstructor (XYGen m) = ASTConstructor m

    --addASTType :: Maybe ASTTypeName -> a (ASTType a)
    addASTType = liftAST . addASTType 

    --addPrimitiveType :: ASTTypeName -> a (ASTType a)
    addPrimitiveType = liftAST . addPrimitiveType

    --addListType :: ASTType a -> a (ASTType a)
    addListType = liftAST . addListType

    --addSeqToASTType :: ASTType a -> Maybe ConstructorName -> [ASTType a] -> a (ASTConstructor a)
    addSeqToASTType tp maybeN typeRefs = liftAST $ addSeqToASTType tp maybeN typeRefs

    --getASTType :: RuleName -> a (ASTType a)
    getASTType = liftAST . getASTType

    --getConstructorName :: ASTConstructor a -> a ConstructorName
    getConstructorName = liftAST . getConstructorName

    --getConstructorParams :: ASTConstructor a -> a [ASTType a]
    getConstructorParams = liftAST . getConstructorParams
  
    --getConstructorType :: ASTConstructor a -> a (ASTType a)
    getConstructorType = liftAST . getConstructorType


----- X File Generation routines

tokenName :: String -> String
tokenName name = "Tk__" ++ name

genX :: Monad m => String -> XYGen m String
genX grammarName = do
  tokens <- genTokens
  adt <- genADT
  return $ render [doc|{
module ?grammarName~Lexer(alexScanTokens, Token(..))
    where

}
%wrapper monad

??tokens

{
??adt

alexEOF = return EndOfFile
alexScanTokens :: String -> [Token]
alexScanTokens str =.
               case alexScanTokens1 str of
                  Right toks -> toks
                  Left err -> error err

alexScanTokens1 str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                       EndOfFile -> return $ reverse toks
                       _ -> let toks' = tok : toks.
                            in toks' `seq` loop toks'
  loop []
simple1 :: (String -> Token) -> AlexInput -> Int -> Alex Token
simple1 t (_, _, _, str) len = return $ t (take len str)

simple t input len = return t

rtkError ((AlexPn _ line column), _, _, str) len = alexError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column" ++ ". Following chars :" ++ (take 10 str)

}
|]

genTokens :: Monad m => XYGen m Doc
genTokens = do
  toks <- access' tokDefs
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

genADT :: Monad m => XYGen m Doc
genADT = do
  toks <- access' tokDefs
  tokConstrs <- mapM genTokConstr toks
  return $ text "data Token = " <+> joinAlts ([text "EndOfFile"] ++ tokConstrs ++ [text "deriving Show" ])

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

{-                                          
translateClauseForMacro (IStrLit s) = text s
translateClauseForMacro (IRegExpLit re) = brackets $ text $ backquoteStrInBrackets re
translateClauseForMacro (ISeq cls) = hsep $ punctuate (text " ") (map translateClauseForMacro cls)
translateClauseForMacro (IAlt clauses) = hsep $ punctuate (text "|") (map translateClauseForMacro clauses)
translateClauseForMacro cl = text $ "Cannot translate to macro def " ++ (show cl)
-}

translateClause :: Monad m => IClause -> XYGen m Doc
translateClause (IId name) = return [doc| $?name |]
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
improt ?name~AST
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

genYTokens :: Monad m => XYGen m Doc
genYTokens = do
  toks <- access' tokDefs
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
  rls <- access' rules
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
  ruleNames <- mapM (\ref -> access' ruleMap >>= return . yName . fromJust . IM.lookup (fromRuleRef ref)) ruleRefs
  let parseDoc = hsep $ map text ruleNames
  let actionDoc = case act of
                    Constructor nm elems -> text nm <+> hsep ( map (text . elemStr) elems )
                    Simple elem -> text $ elemStr elem
                    Nil -> empty
  return [doc|??parseDoc { ??actionDoc }|]