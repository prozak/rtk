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
             deriving Show

data ASTState = ASTState {
                          _typeTab :: R.RefTable AType,
                          _nameCounter :: Int,
                          _nameMap :: M.Map ASTTypeName TypeRef,
                          _ruleMap :: M.Map RuleName TypeRef
                         }

$(makeLens ''ASTState)

newtype SimpleASTGen m a = SimpleASTGen { fromASTGen :: FutureT ASTState m a }
    deriving (Monad, MonadFuture ASTState)

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

runASTGenRec :: (MonadFix m, Monad m) => SimpleASTGen m a -> m (a, ASTState)
runASTGenRec astGen = runFutureTRec startASTState (fromASTGen astGen)
 where
   startASTState = ASTState {
                             _typeTab = R.empty,
                             _nameCounter = 0,
                             _nameMap = M.empty,
                             _ruleMap = M.empty
                            }

instance (Monad m) => ContentGen (SimpleASTGen m) where
    generateContent nm = do
      astText <- genAST nm
      return [(nm ++ "AST.hs", astText)]

instance (Monad m, MonadFix m) => ASTGen (SimpleASTGen m) where
    type ASTType (SimpleASTGen m) = TypeRef
    type ASTConstructor (SimpleASTGen m) = DataDef

    --addASTType :: Maybe ASTTypeName -> a (ASTType a)
    addASTType maybeN = do
      name <- ensureName maybeN "Data"
      case maybeN of
        Just n -> do
          oldRef <- present nameMap >>= return . M.lookup n
          case oldRef of
            Just tr -> return tr
            Nothing -> newTypeRef True (ATypeDef (TypeDef name []))
        Nothing -> newTypeRef False (ATypeDef (TypeDef name []))

    --addPrimitiveType :: ASTTypeName -> a (ASTType a)
    addPrimitiveType tn = do
       newTypeRef False (APrimType tn)

    --addListType :: ASTType a -> a (ASTType a)
    addListType tp = do
      newTypeRef False (AListType tp)

    --setRuleType :: ASTType a -> RuleName -> a ()
    setRuleType tp rn = do
      ruleMap %= M.insert rn tp
      return ()

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

    --getRuleASTType :: RuleName -> a (Maybe (ASTType a))
    getRuleASTType nm = do
      theMap <- future ruleMap
      return $ M.lookup nm theMap

    --getConstructorName :: ASTConstructor a -> a ConstructorName
    getConstructorName (DataDef name _ _) = return name

    --getConstructorParams :: ASTConstructor a -> a [ASTType a]
    getConstructorParams (DataDef _ _ params) = return params
  
    --getConstructorType :: ASTConstructor a -> a (ASTType a)
    getConstructorType (DataDef _ tp _) = return tp


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
    _ -> return $ fromJust $ aName aType
  --return $ fromJust $ aName aType

genDataDef :: Monad m => DataDef -> SimpleASTGen m Doc
genDataDef (DataDef constr _ typeRefs) = do
  trNames <- mapM typeRefStr typeRefs
  return $ hsep (text constr : map text trNames)