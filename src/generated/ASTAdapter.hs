{-# LANGUAGE DeriveDataTypeable #-}
-- | AST Adapter Module
--
-- This module converts the auto-generated AST from GrammarParser.hs
-- to the hand-written AST format used by the rest of RTK.
--
-- Generated AST (GrammarParser.hs):
--   Grammar, Rule, Clause, Name, StrLit, Option, etc.
--
-- Hand-written AST (Parser.hs):
--   InitialGrammar, IRule, IClause, IOption
--
module ASTAdapter
    ( convertGrammar
    , GenGrammar
    ) where

import qualified GrammarParser as Gen
import qualified Parser as Hand
import Data.Generics (Data, Typeable)

-- | Type alias for the generated Grammar type
type GenGrammar = Gen.Grammar

-- | Convert generated Grammar to hand-written InitialGrammar
convertGrammar :: Gen.Grammar -> Hand.InitialGrammar
convertGrammar gram = case gram of
    -- Main grammar production: grammar 'name' ; imports rules
    Gen.Ctr__Grammar__11 strLit importsOpt ruleList ->
        Hand.InitialGrammar
            { Hand.getIGrammarName = extractStrLit strLit
            , Hand.getImports = extractImports importsOpt
            , Hand.getIRules = map convertRule ruleList
            }

    -- Handle the wrapper constructors (for quasi-quotation entry points)
    Gen.Ctr__Grammar__0 innerGram -> convertGrammar innerGram

    -- For anti-quotation (shouldn't happen during normal parsing)
    Gen.Anti_Grammar str -> error $ "Unexpected anti-quotation in Grammar: " ++ str

    -- Other constructors are for sub-grammar entry points (shouldn't be top-level)
    _ -> error $ "Unexpected Grammar constructor: " ++ show gram

-- | Extract string from StrLit
-- The lexer includes the surrounding quotes, so we need to strip them
extractStrLit :: Gen.StrLit -> String
extractStrLit (Gen.Ctr__StrLit__0 s) = stripQuotes s
extractStrLit (Gen.Anti_StrLit s) = error $ "Unexpected anti-quotation in StrLit: " ++ s

-- | Strip surrounding single quotes from a string literal
stripQuotes :: String -> String
stripQuotes s = case s of
    ('\'':rest) -> case reverse rest of
        ('\'':inner) -> reverse inner
        _ -> s
    _ -> s

-- | Extract imports string from ImportsOpt
extractImports :: Gen.ImportsOpt -> String
extractImports Gen.Ctr__ImportsOpt__0 = ""  -- No imports
extractImports (Gen.Ctr__ImportsOpt__1 (Gen.Ctr__Rule_0__0 bigstr)) = bigstr
extractImports (Gen.Anti_ImportsOpt s) = error $ "Unexpected anti-quotation in ImportsOpt: " ++ s

-- | Extract name string from Name
extractName :: Gen.Name -> String
extractName (Gen.Ctr__Name__0 s) = s
extractName (Gen.Anti_Name s) = error $ "Unexpected anti-quotation in Name: " ++ s

-- | Convert generated Rule to hand-written IRule
convertRule :: Gen.Rule -> Hand.IRule
convertRule rule = case rule of
    -- Simple rule: Name = Clause ;
    Gen.Ctr__Rule__0 name clause ->
        Hand.IRule
            { Hand.getIDataTypeName = Nothing
            , Hand.getIDataFunc = Nothing
            , Hand.getIRuleName = extractName name
            , Hand.getIClause = convertClause clause
            , Hand.getIRuleOptions = []
            }

    -- Typed rule: Type : Name = Clause ;
    Gen.Ctr__Rule__1 typeName name clause ->
        Hand.IRule
            { Hand.getIDataTypeName = Just (extractName typeName)
            , Hand.getIDataFunc = Nothing
            , Hand.getIRuleName = extractName name
            , Hand.getIClause = convertClause clause
            , Hand.getIRuleOptions = []
            }

    -- Typed rule with function: Type . Func : Name = Clause ;
    Gen.Ctr__Rule__2 typeName funcName name clause ->
        Hand.IRule
            { Hand.getIDataTypeName = Just (extractName typeName)
            , Hand.getIDataFunc = Just (extractName funcName)
            , Hand.getIRuleName = extractName name
            , Hand.getIClause = convertClause clause
            , Hand.getIRuleOptions = []
            }

    -- Typed rule without data type: . Func : Name = Clause ;
    Gen.Ctr__Rule__3 funcName name clause ->
        Hand.IRule
            { Hand.getIDataTypeName = Nothing
            , Hand.getIDataFunc = Just (extractName funcName)
            , Hand.getIRuleName = extractName name
            , Hand.getIClause = convertClause clause
            , Hand.getIRuleOptions = []
            }

    -- Rule with options: @options Rule1
    Gen.Ctr__Rule__4 optionList innerRule ->
        let baseRule = convertRule innerRule
            opts = map convertOption optionList
        in baseRule { Hand.getIRuleOptions = opts ++ Hand.getIRuleOptions baseRule }

    -- Anti-quotation
    Gen.Anti_Rule s -> error $ "Unexpected anti-quotation in Rule: " ++ s

-- | Convert generated Option to hand-written IOption
convertOption :: Gen.Option -> Hand.IOption
convertOption opt = case opt of
    Gen.Ctr__Option__0 idList -> Hand.OShortcuts (map extractName idList)
    Gen.Ctr__Option__1 -> Hand.OSymmacro
    Gen.Anti_Option s -> error $ "Unexpected anti-quotation in Option: " ++ s

-- | Convert generated Clause to hand-written IClause
convertClause :: Gen.Clause -> Hand.IClause
convertClause clause = case clause of
    -- Name reference
    Gen.Ctr__Clause__1 name -> Hand.IId (extractName name)

    -- String literal
    Gen.Ctr__Clause__2 strLit -> Hand.IStrLit (extractStrLit strLit)

    -- Dot (any character)
    Gen.Ctr__Clause__3 -> Hand.IDot

    -- Regex literal [...]
    Gen.Ctr__Clause__4 regex -> Hand.IRegExpLit regex

    -- Star: Clause * OptDelim
    Gen.Ctr__Clause__5 innerClause optDelim ->
        Hand.IStar (convertClause innerClause) (extractOptDelim optDelim)

    -- Plus: Clause + OptDelim
    Gen.Ctr__Clause__6 innerClause optDelim ->
        Hand.IPlus (convertClause innerClause) (extractOptDelim optDelim)

    -- Optional: Clause ?
    Gen.Ctr__Clause__7 innerClause ->
        Hand.IOpt (convertClause innerClause)

    -- Lifted/Comma: , Clause
    Gen.Ctr__Clause__9 innerClause ->
        Hand.ILifted (convertClause innerClause)

    -- Ignore/Exclamation: ! Clause
    Gen.Ctr__Clause__10 innerClause ->
        Hand.IIgnore (convertClause innerClause)

    -- Sequence: Clause Clause (binary, need to flatten)
    Gen.Ctr__Clause__12 left right ->
        flattenSeq left right

    -- Alternation: Clause | Clause (binary, need to flatten)
    Gen.Ctr__Clause__14 left right ->
        flattenAlt left right

    -- Anti-quotation
    Gen.Anti_Clause s -> error $ "Unexpected anti-quotation in Clause: " ++ s

-- | Extract optional delimiter from OptDelim
extractOptDelim :: Gen.OptDelim -> Maybe Hand.IClause
extractOptDelim optDelim = case optDelim of
    Gen.Ctr__OptDelim__0 -> Nothing  -- No delimiter
    Gen.Ctr__OptDelim__1 (Gen.Ctr__Rule_4__0 delimClause) ->
        Just (convertClause delimClause)
    Gen.Anti_OptDelim s -> error $ "Unexpected anti-quotation in OptDelim: " ++ s

-- | Flatten binary sequence into list-based ISeq
-- The generated AST uses binary Ctr__Clause__12 for sequences
-- The hand-written AST uses ISeq [IClause] for sequences
flattenSeq :: Gen.Clause -> Gen.Clause -> Hand.IClause
flattenSeq left right =
    let leftClauses = collectSeq left
        rightClauses = collectSeq right
    in case leftClauses ++ rightClauses of
        [single] -> single
        multiple -> Hand.ISeq multiple

-- | Collect sequence elements from binary representation
collectSeq :: Gen.Clause -> [Hand.IClause]
collectSeq clause = case clause of
    Gen.Ctr__Clause__12 l r -> collectSeq l ++ collectSeq r
    other -> [convertClause other]

-- | Flatten binary alternation into list-based IAlt
-- The generated AST uses binary Ctr__Clause__14 for alternation
-- The hand-written AST uses IAlt [IClause] for alternation
flattenAlt :: Gen.Clause -> Gen.Clause -> Hand.IClause
flattenAlt left right =
    let leftAlts = collectAlt left
        rightAlts = collectAlt right
    in case leftAlts ++ rightAlts of
        [single] -> single
        multiple -> Hand.IAlt multiple

-- | Collect alternation elements from binary representation
collectAlt :: Gen.Clause -> [Hand.IClause]
collectAlt clause = case clause of
    Gen.Ctr__Clause__14 l r -> collectAlt l ++ collectAlt r
    other -> [convertClause other]
