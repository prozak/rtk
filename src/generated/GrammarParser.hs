-- STUB MODULE - Replaced by generated version when running 'make generate-bootstrap'
--
-- This stub provides all type definitions that ASTAdapter needs,
-- allowing the initial build to succeed before generated modules exist.
--
-- Run 'make generate-bootstrap' to replace this with the actual generated parser.

module GrammarParser
    ( parseGrammar
    , Grammar(..)
    , Clause(..)
    , IdList
    , ImportsOpt(..)
    , Name(..)
    , OptDelim(..)
    , Option(..)
    , OptionList
    , Rule(..)
    , RuleList
    , Rule_2(..)
    , Rule_20(..)
    , StrLit(..)
    ) where

-- Type definitions matching the generated parser interface
data Grammar = Ctr__Grammar__0 Grammar |
               Ctr__Grammar__1 Clause |
               Ctr__Grammar__2 IdList |
               Ctr__Grammar__3 ImportsOpt |
               Ctr__Grammar__4 Name |
               Ctr__Grammar__5 OptDelim |
               Ctr__Grammar__6 Option |
               Ctr__Grammar__7 OptionList |
               Ctr__Grammar__8 Rule |
               Ctr__Grammar__9 RuleList |
               Ctr__Grammar__10 StrLit |
               Anti_Grammar1 String |
               Ctr__Grammar__11 StrLit ImportsOpt RuleList
               deriving (Ord, Eq, Show)

data Clause = Anti_Clause19 String |
              Ctr__Clause__1 Name |
              Ctr__Clause__2 StrLit |
              Ctr__Clause__3 |
              Ctr__Clause__4 String |
              Ctr__Clause__5 Clause OptDelim |
              Ctr__Clause__6 Clause OptDelim |
              Ctr__Clause__7 Clause |
              Ctr__Clause__9 Clause |
              Ctr__Clause__10 Clause |
              Ctr__Clause__12 Clause Clause |
              Ctr__Clause__14 Clause Clause
              deriving (Ord, Eq, Show)

type IdList = [Name]

data ImportsOpt = Anti_ImportsOpt4 String |
                  Ctr__ImportsOpt__0 |
                  Ctr__ImportsOpt__1 Rule_2
                  deriving (Ord, Eq, Show)

data Name = Anti_Name26 String |
            Ctr__Name__0 String |
            Anti_IdList17 String
            deriving (Ord, Eq, Show)

data OptDelim = Anti_OptDelim22 String |
                Ctr__OptDelim__0 |
                Ctr__OptDelim__1 Rule_20
                deriving (Ord, Eq, Show)

data Option = Anti_Option14 String |
              Ctr__Option__0 IdList |
              Ctr__Option__1 |
              Anti_OptionList12 String
              deriving (Ord, Eq, Show)

type OptionList = [Option]

data Rule = Anti_Rule9 String |
            Ctr__Rule__0 Name Clause |
            Ctr__Rule__1 Name Name Clause |
            Ctr__Rule__2 Name Name Name Clause |
            Ctr__Rule__3 Name Name Clause |
            Ctr__Rule__4 OptionList Rule |
            Anti_RuleList7 String
            deriving (Ord, Eq, Show)

type RuleList = [Rule]

data Rule_2 = Ctr__Rule_2__0 String
              deriving (Ord, Eq, Show)

data Rule_20 = Ctr__Rule_20__0 Clause
               deriving (Ord, Eq, Show)

data StrLit = Anti_StrLit24 String |
              Ctr__StrLit__0 String
              deriving (Ord, Eq, Show)

-- Stub parser function - errors if actually called
parseGrammar :: [a] -> Grammar
parseGrammar _ = error "GrammarParser stub: Run 'make generate-bootstrap' to generate the actual parser"
