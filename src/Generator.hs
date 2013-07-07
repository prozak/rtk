{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module Generator
    where

import GenClasses
import Data.List
import Parser
import Data.Maybe
import MonadFuture

type Generator a = (Monad a, ParserGen a, ASTGen a, ASTConstructor (ParserAST a) ~ ASTConstructor a, MonadCond a)

data SeqElem a = SNormal (Parser a) (ASTType a)
               | SIgnore (Parser a)
               | SLifted (Parser a) (ASTType a)

isLifted :: SeqElem a -> Bool
isLifted (SLifted _ _) = True
isLifted _ = False

isNormal :: SeqElem a -> Bool
isNormal (SNormal _ _) = True
isNormal _ = False

seqEParser :: SeqElem a -> Parser a
seqEParser (SNormal p _) = p
seqEParser (SIgnore p) = p
seqEParser (SLifted p _) = p

data Seq a = Seq (Maybe (ASTConstructor a)) [SeqElem a]

generateGrammar :: Generator a => InitialGrammar -> a ()
generateGrammar (InitialGrammar name imports rules) = mapM_ generateRule rules

generateRule :: Generator a => IRule -> a ()
generateRule (IRule mDTName mDTFunc ruleName clause options) | isLexicalRule ruleName = do
  if mDTName == Just "Ignore"
     then do
           addLexRule (Just ruleName) LIgnore clause
           tp <- addASTType (ASTPrimitive "String")
           setRuleType tp ruleName
     else do
           if (elem OSymmacro options)
              then addLexMacro (Just ruleName) clause
              else addLexRule (Just ruleName) (LData (maybe "id" id mDTFunc) (maybe "String" id mDTName)) clause
           tp <- addASTType (ASTPrimitive (maybe "String" id mDTName))
           setRuleType tp ruleName
  return ()
generateRule (IRule mDTName mDTFunc ruleName clause options) = do
  generateClause (maybe (Just ruleName) Just mDTName) (Just ruleName) clause
  return ()

generateClause :: Generator a => Maybe String -> Maybe String -> IClause -> a (Parser a, ASTType a)
generateClause maybeTypeName maybeName (IAlt [ISeq [clause]]) | not (isSimpleClause clause) = generateClause maybeTypeName maybeName clause
generateClause maybeTypeName maybeName (IAlt alts) = do
  tp <- addASTType (ASTData maybeTypeName)
  case maybeName of
    Just rn -> setRuleType tp rn
    Nothing -> return ()
  seqs <- mapM (generateSeq tp) alts
  let parsers = map seqToPseq seqs
  parser <- addClause maybeName (PAlt parsers)
  return (parser, tp)
generateClause maybeTypeName maybeName (IStar clause maybeSep) =
  generateMany PStar clause maybeSep maybeTypeName maybeName
generateClause maybeTypeName maybeName (IPlus clause maybeSep) =
  generateMany PPlus clause maybeSep maybeTypeName maybeName
generateClause maybeTypeName maybeName (IOpt clause) = do
  case maybeName of
    Just ruleName -> do
        generateClause maybeTypeName maybeName (IAlt [ISeq [clause], ISeq []])
    Nothing -> do
        ~(SNormal subP subTp) <- generateSubClause clause
        tp <- addASTType (ASTMaybe subTp)
        parser <- addClause maybeName (POpt subP)
        return (parser, tp)
generateClause _ _ cl = error $ "Do not know how to handle" ++ show cl

isASTData :: ASTTypeDecl a -> Bool
isASTData ASTData{} = True
isASTData _ = False

generateMany :: Generator a => PManyOp -> IClause -> (Maybe IClause) -> Maybe String -> Maybe String -> a (Parser a, ASTType a)
generateMany manyOp clause maybeSep maybeTypeName maybeName = do
  ~(SNormal subP subTp) <- generateSubClause clause
  maybeSepParser <- maybe (return Nothing) (\ a -> generateSubClause a >>= return . Just . seqEParser) maybeSep
  tp <- addASTType (ASTList subTp)
  case maybeName of
    Just rn -> setRuleType tp rn
    Nothing -> return ()
  parser <- addClause maybeName (PMany subP manyOp maybeSepParser)
  td <- getASTTypeDecl subTp
  --ifE (isASTData td)
  --    (do
         -- stub for future quasiquotation actions
  --       addSeqToASTType subTp Nothing []
  --       return ())
  --    (return ())
  return (parser, tp)

generateSeq :: Generator a => (ASTType a) -> IClause -> a (Seq a)
generateSeq tp (ISeq clauses) = do
  seqElems <- mapM generateSubClause clauses
  case typesOfSeqElems seqElems of
    Nothing -> do
      return $ Seq Nothing seqElems
    Just tps -> do
      constr <- addSeqToASTType tp Nothing tps
      return $ Seq (Just constr) seqElems

hasLiftedElem :: Generator a => [SeqElem a] -> Bool
hasLiftedElem seqElems = any isLifted seqElems

typesOfSeqElems :: Generator a => [SeqElem a] -> Maybe [ASTType a]
typesOfSeqElems seqElems = if hasLiftedElem seqElems
                             then Nothing
                             else Just $ map (\ (SNormal p a) -> a) $ filter isNormal seqElems

seqToPseq :: Generator a => Seq a -> PSeq a
seqToPseq (Seq mc seqElems) = case find (isLifted . fst) (zip seqElems [0..]) of
                                Just (SLifted p tp, ind) -> POne (map seqEParser seqElems) ind
                                Nothing -> let inds = map snd (filter (isNormal . fst) (zip seqElems [0..]))
                                           in PSeq (map seqEParser seqElems) (fromJust mc) inds

generateSubClause :: Generator a => IClause -> a (SeqElem a)
generateSubClause (IId id) = do
  ~(Just parser) <- getParser id
  ~(Just tp) <- getRuleASTType id
  return $ SNormal parser tp
generateSubClause (ILifted clause) = do
  (SNormal parser tp) <- generateSubClause clause
  return $ SLifted parser tp
generateSubClause (IIgnore clause) = do
  (SNormal parser _) <- generateSubClause clause
  return $ SIgnore parser 
generateSubClause (IStrLit lit) = do
  parser <- addToken lit
  return $ SIgnore parser
generateSubClause cl = do
  (parser, tp) <- generateClause Nothing Nothing cl
  return $ SNormal parser tp