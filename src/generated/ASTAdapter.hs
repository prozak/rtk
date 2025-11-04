{-# LANGUAGE PatternGuards #-}
module ASTAdapter
    ( convertGrammar
    ) where

-- This module converts the auto-generated AST from GrammarParser.hs
-- to the hand-written AST format used by the rest of RTK.
--
-- Implementation uses traditional pattern matching on generated constructors.
-- See docs/reference-generated/AST-MAPPING.md for detailed mapping guide.

import qualified Parser as Hand
import qualified GrammarParser as Gen

-- | Convert generated Grammar to hand-written InitialGrammar
convertGrammar :: Gen.Grammar -> Hand.InitialGrammar
convertGrammar (Gen.Ctr__Grammar__11 strLit importsOpt ruleList) =
    Hand.InitialGrammar
        { Hand.getIGrammarName = convertStrLit strLit
        , Hand.getImports = convertImports importsOpt
        , Hand.getIRules = map convertRule ruleList
        }
convertGrammar other =
    error $ "ASTAdapter.convertGrammar: unexpected Grammar constructor: " ++ show other

-- | Convert StrLit to String
-- Note: Generated lexer includes quotes, hand-written strips them
convertStrLit :: Gen.StrLit -> String
convertStrLit (Gen.Ctr__StrLit__0 str) = stripQuotes str
convertStrLit (Gen.Anti_StrLit24 qqVar) =
    error $ "QuasiQuoted StrLit not supported in concrete grammar: $StrLit:" ++ qqVar

-- | Strip surrounding single quotes from string literals
-- The generated lexer includes quotes, but hand-written lexer strips them
stripQuotes :: String -> String
stripQuotes ('\'':rest) = reverse (drop 1 (reverse rest))  -- Remove leading and trailing '
stripQuotes str = str  -- No quotes to strip

-- | Convert Name to String
convertName :: Gen.Name -> String
convertName (Gen.Ctr__Name__0 str) = str
convertName (Gen.Anti_Name26 qqVar) =
    error $ "QuasiQuoted Name not supported in concrete grammar: $Name:" ++ qqVar
convertName (Gen.Anti_IdList17 qqVar) =
    error $ "QuasiQuoted IdList not supported in concrete grammar: $IdList:" ++ qqVar

-- | Convert ImportsOpt to String
convertImports :: Gen.ImportsOpt -> String
convertImports Gen.Ctr__ImportsOpt__0 = ""
convertImports (Gen.Ctr__ImportsOpt__1 (Gen.Ctr__Rule_2__0 bigstr)) = bigstr
convertImports (Gen.Anti_ImportsOpt4 qqVar) =
    error $ "QuasiQuoted ImportsOpt not supported: $ImportsOpt:" ++ qqVar

-- | Convert OptDelim to Maybe IClause
convertOptDelim :: Gen.OptDelim -> Maybe Hand.IClause
convertOptDelim Gen.Ctr__OptDelim__0 = Nothing
convertOptDelim (Gen.Ctr__OptDelim__1 (Gen.Ctr__Rule_20__0 clause)) =
    Just (convertClause clause)
convertOptDelim (Gen.Anti_OptDelim22 qqVar) =
    error $ "QuasiQuoted OptDelim not supported: $OptDelim:" ++ qqVar

-- | Convert Rule to IRule
convertRule :: Gen.Rule -> Hand.IRule

-- id = clause ;
convertRule (Gen.Ctr__Rule__0 name clause) =
    Hand.IRule
        { Hand.getIDataTypeName = Nothing
        , Hand.getIDataFunc = Nothing
        , Hand.getIRuleName = convertName name
        , Hand.getIClause = convertClause clause
        , Hand.getIRuleOptions = []
        }

-- id : id = clause ;
convertRule (Gen.Ctr__Rule__1 name1 name2 clause) =
    Hand.IRule
        { Hand.getIDataTypeName = Just (convertName name1)
        , Hand.getIDataFunc = Nothing
        , Hand.getIRuleName = convertName name2
        , Hand.getIClause = convertClause clause
        , Hand.getIRuleOptions = []
        }

-- id . id : id = clause ;
convertRule (Gen.Ctr__Rule__2 name1 name2 name3 clause) =
    Hand.IRule
        { Hand.getIDataTypeName = Just (convertName name1)
        , Hand.getIDataFunc = Just (convertName name2)
        , Hand.getIRuleName = convertName name3
        , Hand.getIClause = convertClause clause
        , Hand.getIRuleOptions = []
        }

-- . id : id = clause ;
convertRule (Gen.Ctr__Rule__3 name1 name2 clause) =
    Hand.IRule
        { Hand.getIDataTypeName = Nothing
        , Hand.getIDataFunc = Just (convertName name1)
        , Hand.getIRuleName = convertName name2
        , Hand.getIClause = convertClause clause
        , Hand.getIRuleOptions = []
        }

-- options + rule
convertRule (Gen.Ctr__Rule__4 opts rule) =
    let baseRule = convertRule rule
    in baseRule { Hand.getIRuleOptions = map convertOption opts ++ Hand.getIRuleOptions baseRule }

convertRule (Gen.Anti_Rule9 qqVar) =
    error $ "QuasiQuoted Rule not supported: $Rule:" ++ qqVar

convertRule (Gen.Anti_RuleList7 qqVar) =
    error $ "QuasiQuoted RuleList not supported: $RuleList:" ++ qqVar

-- | Convert Option to IOption
convertOption :: Gen.Option -> Hand.IOption
convertOption (Gen.Ctr__Option__0 names) =
    Hand.OShortcuts (map convertName names)
convertOption Gen.Ctr__Option__1 =
    Hand.OSymmacro
convertOption (Gen.Anti_Option14 qqVar) =
    error $ "QuasiQuoted Option not supported: $Option:" ++ qqVar
convertOption (Gen.Anti_OptionList12 qqVar) =
    error $ "QuasiQuoted OptionList not supported: $OptionList:" ++ qqVar

-- | Convert Clause to IClause
convertClause :: Gen.Clause -> Hand.IClause

-- id
convertClause (Gen.Ctr__Clause__1 name) =
    Hand.IId (convertName name)

-- 'str'
convertClause (Gen.Ctr__Clause__2 strLit) =
    Hand.IStrLit (convertStrLit strLit)

-- .
convertClause Gen.Ctr__Clause__3 =
    Hand.IDot

-- [regexp]
convertClause (Gen.Ctr__Clause__4 regexp) =
    Hand.IRegExpLit regexp

-- clause * (with optional delimiter)
convertClause (Gen.Ctr__Clause__5 clause optDelim) =
    Hand.IStar (convertClause clause) (convertOptDelim optDelim)

-- clause + (with optional delimiter)
convertClause (Gen.Ctr__Clause__6 clause optDelim) =
    Hand.IPlus (convertClause clause) (convertOptDelim optDelim)

-- clause ?
convertClause (Gen.Ctr__Clause__7 clause) =
    Hand.IOpt (convertClause clause)

-- , clause (lifted)
convertClause (Gen.Ctr__Clause__9 clause) =
    Hand.ILifted (convertClause clause)

-- ! clause (ignored)
convertClause (Gen.Ctr__Clause__10 clause) =
    Hand.IIgnore (convertClause clause)

-- clause clause ... (sequence)
-- Need to flatten nested Ctr__Clause__12 into a list
convertClause (Gen.Ctr__Clause__12 c1 c2) =
    Hand.ISeq (flattenSeq c1 ++ flattenSeq c2)
  where
    flattenSeq :: Gen.Clause -> [Hand.IClause]
    flattenSeq (Gen.Ctr__Clause__12 a b) = flattenSeq a ++ flattenSeq b
    flattenSeq c = [convertClause c]

-- clause | clause ... (alternation)
-- Need to flatten nested Ctr__Clause__14 into a list
convertClause (Gen.Ctr__Clause__14 c1 c2) =
    Hand.IAlt (flattenAlt c1 ++ flattenAlt c2)
  where
    flattenAlt :: Gen.Clause -> [Hand.IClause]
    flattenAlt (Gen.Ctr__Clause__14 a b) = flattenAlt a ++ flattenAlt b
    flattenAlt c = [convertClause c]

-- Anti-quotation (used in quasi-quotation patterns)
convertClause (Gen.Anti_Clause19 qqVar) =
    error $ "QuasiQuoted Clause not supported: $Clause:" ++ qqVar
