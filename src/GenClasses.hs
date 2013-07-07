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

data PSeq p = PSeq [(Parser p)] (ASTConstructor (ParserAST p)) [Int]
            | POne [(Parser p)] Int

data PManyOp = PStar
             | PPlus

data LexRuleAction = LIgnore
                   | LNoData
                   | LData LexRuleFunc LexRuleType

type LexRuleType = String
type LexRuleFunc = String

class (Monad p, ContentGen p, ASTGen (ParserAST p)) => ParserGen p where
    type Parser p
    type ParserAST p
        
    addClause :: Maybe RuleName -> ParserDef p -> p (Parser p)
    addLexRule :: Maybe RuleName -> LexRuleAction -> IClause -> p (Parser p)
    addLexMacro :: Maybe RuleName -> IClause -> p (Parser p)
    addToken :: String -> p (Parser p)
    getParser :: RuleName -> p (Maybe (Parser p))
    liftAST :: ParserAST p c -> p c

type ConstructorName = String

type ASTTypeName = String

data ASTTypeDecl a = ASTData (Maybe ASTTypeName)
                   | ASTList a
                   | ASTPrimitive ASTTypeName
                   | ASTMaybe a

class (Monad a, MonadFix a, ContentGen a) => ASTGen a where
    type ASTType a
    type ASTConstructor a

    addASTType :: ASTTypeDecl (ASTType a) -> a (ASTType a)
    setRuleType :: ASTType a -> RuleName -> a ()
    addSeqToASTType :: ASTType a -> Maybe ConstructorName -> [ASTType a] -> a (ASTConstructor a)
    getASTType :: ASTTypeName -> a (Maybe (ASTType a))
    getRuleASTType :: RuleName -> a (Maybe (ASTType a))
    getConstructorName :: ASTConstructor a -> a ConstructorName
    getASTTypeDecl :: (ASTType a) -> a (ASTTypeDecl (ASTType a))