{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module GenClasses
    where

import Parser
import Control.Monad.State.Lazy

type Content = String

class Monad a => ContentGen a where
    -- grammarName -> list of (fileName, content)
    generateContent :: String -> a [(String, Content)]

type RuleName = String

data ParserDef p = PAlt [PSeq p]
                 | PMany (Parser p) PManyOp (Maybe (Parser p))
                 | POpt (Parser p)

data PSeq p = PSeq [(Parser p)] ConstructorName [Int]
            | POne [(Parser p)] Int

data PManyOp = PStar
             | PPlus

data LexRuleAction = LIgnore
                   | LNoData
                   | LData LexRuleFunc LexRuleType

type LexRuleType = String
type LexRuleFunc = String

class (Monad p, MonadFix p) => ParserGen p where
    type Parser p
        
    addClause :: Maybe RuleName -> ParserDef p -> p (Parser p)
    addLexRule :: Maybe RuleName -> LexRuleAction -> IClause -> p (Parser p)
    addLexMacro :: Maybe RuleName -> IClause -> p (Parser p)
    addToken :: String -> p (Parser p)
    getParser :: RuleName -> p (Parser p)

type ConstructorName = String

type ASTTypeName = String

data ASTTypeDecl a = ASTData (Maybe ASTTypeName)
                   | ASTList a
                   | ASTPrimitive ASTTypeName
                   | ASTMaybe a

class (Monad a, MonadFix a) => ASTGen a where
    type ASTType a
    type ASTConstructor a

    addASTType :: Maybe RuleName -> ASTTypeDecl (ASTType a) -> a (ASTType a)
    addSeqToASTType :: ASTType a -> Maybe ConstructorName -> [ASTType a] -> a (ASTConstructor a)
    getASTType :: ASTTypeName -> a (Maybe (ASTType a))
    getRuleASTType :: RuleName -> a (ASTType a)
    getConstructorName :: ASTConstructor a -> a ConstructorName
    getASTTypeDecl :: (ASTType a) -> a (ASTTypeDecl (ASTType a))
    