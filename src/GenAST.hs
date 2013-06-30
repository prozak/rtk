{-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, RecursiveDo, QuasiQuotes  #-}
module GenAST(SimpleASTGen, runASTGen, runASTGenRec)
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

newtype TypeRef = TypeRef { fromTypeRef :: Int }
             deriving Show

data TypeDef = TypeDef ASTTypeName [DataDef]
             deriving Show

data DataDef = DataDef ConstructorName TypeRef [TypeRef]
             deriving Show

data AType = ATypeDef TypeDef
           | APrimType ASTTypeName
           | AListType TypeRef
             deriving Show

data ASTState = ASTState {
                          _typeMap :: IM.IntMap AType,
                          _nameCounter :: Int,
                          _nameMap :: M.Map ASTTypeName TypeRef,
                          _ruleMap :: M.Map RuleName TypeRef,
                          _curRef :: Int
                         }

$(makeLens ''ASTState)

newtype SimpleASTGen m a = SimpleASTGen { fromASTGen :: StateT (ASTState, ASTState) m a }

instance Monad m => Monad (SimpleASTGen m) where
    return = SimpleASTGen . return
    a >>= b = SimpleASTGen (fromASTGen a >>= fromASTGen . b) 

instance Monad m => MonadState (ASTState, ASTState) (SimpleASTGen m) where
    get = SimpleASTGen get
    put = SimpleASTGen . put
    state = SimpleASTGen . state

instance MonadFix m => MonadFix (SimpleASTGen m) where
    mfix f = SimpleASTGen $ mfix (fromASTGen . f)

newName :: Monad m => String -> SimpleASTGen m String
newName str = do
  ind <- access' nameCounter
  nameCounter %= (1 +)
  return $ str ++ show ind

ensureName :: Monad m => Maybe String -> String -> SimpleASTGen m String
ensureName maybeN str = case maybeN of
                          Just n -> return n
                          Nothing -> newName str

aName :: AType -> Maybe String
aName (ATypeDef (TypeDef n _)) = Just n
aName (APrimType n) = Just n
aName _ = Nothing

isTypeDef :: AType -> Bool
isTypeDef (ATypeDef _) = True
isTypeDef _ = False

newTypeRef :: Monad m => Bool -> AType -> SimpleASTGen m TypeRef
newTypeRef addToNameMap theType = do
  ref <- access' curRef
  curRef %= (1 +)
  let name = aName theType
  typeMap %= IM.insert ref theType
  case name of 
    Just nm -> if addToNameMap
               then nameMap %= M.insert nm (TypeRef ref)
               else return ()
    Nothing -> return ()
  return $ TypeRef ref

addConstrToAType :: DataDef -> AType -> AType
addConstrToAType def (ATypeDef (TypeDef nm datas)) =
    ATypeDef (TypeDef nm (def : datas))

runASTGen :: (Monad m) => ASTState -> SimpleASTGen m a -> m a
runASTGen st astGen = do
  ~(res, _) <- runStateT (fromASTGen astGen) (st, st)
  return res

runASTGenRec :: (MonadFix m, Monad m) => SimpleASTGen m a -> m (a, ASTState)
runASTGenRec astGen = do
  rec ~(res, ~(_, outState)) <- runStateT (fromASTGen astGen) (outState, startASTState)
  return (res, outState)
 where
   startASTState = ASTState {
                             _typeMap = IM.empty,
                             _nameCounter = 0,
                             _nameMap = M.empty,
                             _curRef = 0,
                             _ruleMap = M.empty
                            }

instance (Monad m) => ContentGen (SimpleASTGen m) where
    generateContent nm = do
      astText <- genAST nm
      return [(nm ++ "AST.hs", astText)]

instance (Monad m) => ASTGen (SimpleASTGen m) where
    type ASTType (SimpleASTGen m) = TypeRef
    type ASTConstructor (SimpleASTGen m) = DataDef

    --addASTType :: Maybe ASTTypeName -> a (ASTType a)
    addASTType maybeN = do
      name <- ensureName maybeN "Data"
      case maybeN of
        Just n -> do
          oldRef <- access' nameMap >>= return . M.lookup n
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
      typeMap %= IM.update (Just . addConstrToAType constr) (fromTypeRef tp)
      return constr

    --getASTType :: ASTTypeName -> a (Maybe (ASTType a))
    getASTType nm = do
      theMap <- access nameMap
      return $ M.lookup nm theMap

    --getRuleASTType :: RuleName -> a (Maybe (ASTType a))
    getRuleASTType nm = do
      theMap <- access ruleMap
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
  types <- access' typeMap >>= return . (map snd) . IM.toList
  let typeDefs = filter isTypeDef types
  typeDefs <- mapM genASTDef typeDefs
  return $ vcat typeDefs

genASTDef :: Monad m => AType -> SimpleASTGen m Doc
genASTDef (ATypeDef (TypeDef name datas)) = do
  dataDefs <- mapM genDataDef datas
  return $ [doc|data ?name = |] <> (vcat (punctuate (text " |") dataDefs) $$ [doc|deriving (Ord,Eq,Show,Gen.Data,Gen.Typeable)|])

typeRefStr :: Monad m => TypeRef -> SimpleASTGen m String
typeRefStr tr = do
  aType <- access' typeMap >>= return . fromJust . IM.lookup (fromTypeRef tr)
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