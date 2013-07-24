{-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, RecursiveDo, QuasiQuotes,
  GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module GenAST(SimpleASTGen, runASTGen, runASTGenRec)
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

import qualified RefTable as R

import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M

type TypeRef = R.Ref AType

data TypeDef = TypeDef ASTTypeName [DataDef]
             deriving Show

data DataDef = DataDef ConstructorName TypeRef [TypeRef]
             deriving Show

data AType = ATypeDef TypeDef
           | APrimType ASTTypeName
           | AListType TypeRef
           | AMaybeType TypeRef
             deriving Show

data ASTState = ASTState {
                          _typeTab :: R.RefTable AType,
                          _nameCounter :: Int,
                          _nameMap :: M.Map ASTTypeName TypeRef,
                          _ruleMap :: M.Map RuleName TypeRef,
                          _errors :: [Maybe String]
                         }

$(makeLens ''ASTState)

newtype SimpleASTGen m a = SimpleASTGen { fromASTGen :: FutureT ASTState m a }
    deriving (Monad, MonadFuture ASTState, MonadCond)

deriving instance MonadFix m => MonadFix (SimpleASTGen m)

instance Monad m => NameGen (SimpleASTGen m) where
    newName = newNameWithCounter nameCounter

aName :: AType -> Maybe String
aName (ATypeDef (TypeDef n _)) = Just n
aName (APrimType n) = Just n
aName _ = Nothing

isTypeDef :: AType -> Bool
isTypeDef (ATypeDef _) = True
isTypeDef _ = False

newTypeRef :: Monad m => Bool -> AType -> SimpleASTGen m TypeRef
newTypeRef addToNameMap theType = do
  let name = aName theType
  ref <- R.newRef typeTab theType
  case name of 
    Just nm -> if addToNameMap
               then nameMap %= M.insert nm ref
               else return ()
    Nothing -> return ()
  return ref

addConstrToAType :: DataDef -> AType -> AType
addConstrToAType def (ATypeDef (TypeDef nm datas)) =
    ATypeDef (TypeDef nm (def : datas))

runASTGen :: (Monad m) => ASTState -> SimpleASTGen m a -> m a
runASTGen st astGen = runFutureT st (fromASTGen astGen)

addASTType' :: (Monad m) => ASTTypeDecl TypeRef -> SimpleASTGen m TypeRef
addASTType' (ASTData maybeN) = do
  name <- ensureName maybeN "Data"
  case maybeN of
    Just n -> do
      oldRef <- present nameMap >>= return . M.lookup n
      case oldRef of
        Just tr -> return tr
        Nothing -> newTypeRef True (ATypeDef (TypeDef name []))
    Nothing -> newTypeRef False (ATypeDef (TypeDef name []))
addASTType' (ASTPrimitive tn) = do
  newTypeRef False (APrimType tn)
addASTType' (ASTList tp) = do
  newTypeRef False (AListType tp)
addASTType' (ASTMaybe tp) = do
  newTypeRef False (AMaybeType tp)

setRuleType :: (Monad m) => TypeRef -> Maybe RuleName -> SimpleASTGen m ()
setRuleType tp mrn =
  case mrn of 
    Just rn -> ruleMap %= M.insert rn tp
    Nothing -> return ()

runASTGenRec :: (MonadFix m, Monad m) => SimpleASTGen m a -> m (a, ASTState)
runASTGenRec astGen = runFutureTRec startASTState (fromASTGen astGen)
 where
   startASTState = ASTState {
                             _typeTab = R.emptyWithError (APrimType "Error"),
                             _nameCounter = 0,
                             _nameMap = M.empty,
                             _ruleMap = M.empty,
                             _errors = []
                            }

instance (Monad m) => ContentGen (SimpleASTGen m) where
    generateContent nm = do
      astText <- genAST nm
      return [(nm ++ "AST.hs", astText)]

instance (Monad m, MonadFix m) => ASTGen (SimpleASTGen m) where
    type ASTType (SimpleASTGen m) = TypeRef
    type ASTConstructor (SimpleASTGen m) = DataDef

    --addASTType :: Maybe RuleName -> ASTTypeDecl a -> a (ASTType a)
    addASTType mRuleName tp = do
      ref <- addASTType' tp
      setRuleType ref mRuleName
      return ref

    --addSeqToASTType :: ASTType a -> Maybe ConstructorName -> [ASTType a] -> a (ASTConstructor a)
    addSeqToASTType tp maybeN typeRefs = do
      name <- ensureName maybeN "Constr"
      let constr = DataDef name tp typeRefs
      R.update tp (addConstrToAType constr) typeTab 
      return constr

    --getASTType :: ASTTypeName -> a (Maybe (ASTType a))
    getASTType nm = do
      theMap <- future nameMap
      return $ M.lookup nm theMap

    --getRuleASTType :: RuleName -> a (ASTType a)
    getRuleASTType nm = do
      theMap <- future ruleMap
      let mref = M.lookup nm theMap
      silentCheckMaybeRef mref R.errorRef 

    --getConstructorName :: ASTConstructor a -> a ConstructorName
    getConstructorName (DataDef name _ _) = return name

    --getASTTypeDecl :: (ASTType a) -> a (ASTTypeDecl (ASTType a))
    getASTTypeDecl tp = do
      ~(Just at) <- R.lookup future tp typeTab
      return $ case at of
                 ATypeDef (TypeDef nm _) -> ASTData (Just nm)
                 APrimType nm -> ASTPrimitive nm
                 AListType tp -> ASTList tp
                 AMaybeType tp -> ASTMaybe tp
                
instance (Monad m) => MonadGenError (SimpleASTGen m) where
    logError mErr = errors %= (mErr :)
    getErrors = present errors >>= return . catMaybes

-- haskell AST generation routines

genAST :: Monad m => String -> SimpleASTGen m String
genAST name = do
  dataDecls <- genASTDatas
  return $ render [doc|{-# LANGUAGE DeriveDataTypeable #-}
module ?name~AST
    where
import qualified Data.Generics as Gen

??dataDecls

|]

genASTDatas :: Monad m => SimpleASTGen m Doc
genASTDatas = do
  types <- R.values present typeTab
  let typeDefs = filter isTypeDef types
  typeDefs <- mapM genASTDef typeDefs
  return $ vcat typeDefs

genASTDef :: Monad m => AType -> SimpleASTGen m Doc
genASTDef (ATypeDef (TypeDef name datas)) = do
  dataDefs <- mapM genDataDef datas
  return $ [doc|data ?name = |] <> (vcat (punctuate (text " |") dataDefs) $$ [doc|deriving (Ord,Eq,Show,Gen.Data,Gen.Typeable)|])

typeRefStr :: Monad m => TypeRef -> SimpleASTGen m String
typeRefStr tr = do
  Just aType <- R.lookup present tr typeTab
  case aType of
    AListType tr2 -> do
                  tn2 <- typeRefStr tr2
                  return $ "[" ++ tn2 ++ "]"
    AMaybeType tr2 -> do
                  tn2 <- typeRefStr tr2
                  return $ "(Maybe " ++ tn2 ++ ")"
    _ -> return $ fromJust $ aName aType
  --return $ fromJust $ aName aType

genDataDef :: Monad m => DataDef -> SimpleASTGen m Doc
genDataDef (DataDef constr _ typeRefs) = do
  trNames <- mapM typeRefStr typeRefs
  return $ hsep (text constr : map text trNames)