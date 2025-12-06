{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | AST Adapter Module
--
-- This module converts the auto-generated AST from GrammarParser.hs
-- to the hand-written AST format used by the rest of RTK.
--
-- Uses quasi-quotation patterns extensively for elegant pattern matching
-- on the generated AST - dogfooding RTK's own QQ features!
--
module ASTAdapter
    ( convertGrammar
    , GenGrammar
    ) where

import qualified GrammarParser as Gen
import qualified Parser as Hand
import GrammarQQ (grammar, clause, option, optDelim)

-- | Type alias for the generated Grammar type
type GenGrammar = Gen.Grammar

-- | Convert generated Grammar to hand-written InitialGrammar
-- Using QQ pattern matching for the main grammar structure
convertGrammar :: Gen.Grammar -> Hand.InitialGrammar
convertGrammar [grammar| grammar $strLit ; $importsOpt $ruleList |] =
    Hand.InitialGrammar
        { Hand.getIGrammarName = extractStrLit strLit
        , Hand.getImports = extractImports importsOpt
        , Hand.getIRules = map convertRule ruleList
        }
convertGrammar gram = error $ "Unexpected Grammar constructor: " ++ show gram

-- | Extract string from StrLit
-- Note: StrLit is a simple terminal (str token), so QQ patterns don't apply here
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
-- Note: ImportsOpt contains a bigstr terminal, so using traditional patterns
extractImports :: Gen.ImportsOpt -> String
extractImports Gen.Ctr__ImportsOpt__0 = ""  -- No imports (empty optional)
extractImports (Gen.Ctr__ImportsOpt__1 (Gen.Ctr__Rule_0__0 bigstr)) = bigstr
extractImports (Gen.Anti_ImportsOpt s) = error $ "Unexpected anti-quotation in ImportsOpt: " ++ show s

-- | Extract name string from Name
-- Note: Name is a simple terminal (id token), so QQ patterns don't apply here
extractName :: Gen.Name -> String
extractName (Gen.Ctr__Name__0 s) = s
extractName (Gen.Anti_Name s) = error $ "Unexpected anti-quotation in Name: " ++ s

-- | Convert generated Rule to hand-written IRule
-- Note: Rule patterns contain single Name positions which don't have QQ anti-quotation
-- support (only [Name] lists are supported), so using traditional pattern matching
convertRule :: Gen.Rule -> Hand.IRule
-- Simple rule: Name = Clause ;
convertRule (Gen.Ctr__Rule__0 nm cl) =
    Hand.IRule
        { Hand.getIDataTypeName = Nothing
        , Hand.getIDataFunc = Nothing
        , Hand.getIRuleName = extractName nm
        , Hand.getIClause = convertClause cl
        , Hand.getIRuleOptions = []
        }
-- Typed rule: Type : Name = Clause ;
convertRule (Gen.Ctr__Rule__1 typNm nm cl) =
    Hand.IRule
        { Hand.getIDataTypeName = Just (extractName typNm)
        , Hand.getIDataFunc = Nothing
        , Hand.getIRuleName = extractName nm
        , Hand.getIClause = convertClause cl
        , Hand.getIRuleOptions = []
        }
-- Typed rule with function: Type . Func : Name = Clause ;
convertRule (Gen.Ctr__Rule__2 typNm funcNm nm cl) =
    Hand.IRule
        { Hand.getIDataTypeName = Just (extractName typNm)
        , Hand.getIDataFunc = Just (extractName funcNm)
        , Hand.getIRuleName = extractName nm
        , Hand.getIClause = convertClause cl
        , Hand.getIRuleOptions = []
        }
-- Rule without data type: . Func : Name = Clause ;
convertRule (Gen.Ctr__Rule__3 funcNm nm cl) =
    Hand.IRule
        { Hand.getIDataTypeName = Nothing
        , Hand.getIDataFunc = Just (extractName funcNm)
        , Hand.getIRuleName = extractName nm
        , Hand.getIClause = convertClause cl
        , Hand.getIRuleOptions = []
        }
-- Rule with options: @options Rule1
convertRule (Gen.Ctr__Rule__4 optList innerRule) =
    let baseRule = convertRule innerRule
        opts = map convertOption optList
    in baseRule { Hand.getIRuleOptions = opts ++ Hand.getIRuleOptions baseRule }
-- Anti-quotation (shouldn't happen)
convertRule (Gen.Anti_Rule s) = error $ "Unexpected anti-quotation in Rule: " ++ s

-- | Convert generated Option to hand-written IOption using QQ patterns
convertOption :: Gen.Option -> Hand.IOption
convertOption [option| @shortcuts ( $idList ) |] = Hand.OShortcuts (map extractName idList)
convertOption [option| @symmacro |] = Hand.OSymmacro
convertOption (Gen.Anti_Option s) = error $ "Unexpected anti-quotation in Option: " ++ s

-- | Convert generated Clause to hand-written IClause using QQ patterns
-- where possible. Some patterns use traditional matching due to QQ limitations.
convertClause :: Gen.Clause -> Hand.IClause
-- Name reference (QQ doesn't support single Name anti-quotation)
convertClause (Gen.Ctr__Clause__1 nm) = Hand.IId (extractName nm)
-- String literal (QQ doesn't support single StrLit anti-quotation in this context)
convertClause (Gen.Ctr__Clause__2 strlit) = Hand.IStrLit (extractStrLit strlit)
-- Dot (any character) - using QQ pattern
convertClause [clause| . |] = Hand.IDot
-- Regex literal [...]
convertClause (Gen.Ctr__Clause__4 regex) = Hand.IRegExpLit regex
-- Star: Clause * OptDelim - using QQ pattern
convertClause [clause| $cl * $optDelim1 |] =
    Hand.IStar (convertClause cl) (extractOptDelim optDelim1)
-- Plus: Clause + OptDelim - using QQ pattern
convertClause [clause| $cl + $optDelim2 |] =
    Hand.IPlus (convertClause cl) (extractOptDelim optDelim2)
-- Optional: Clause ? - using QQ pattern
convertClause [clause| $cl ? |] =
    Hand.IOpt (convertClause cl)
-- Lifted/Comma: , Clause - using QQ pattern
convertClause [clause| , $cl |] =
    Hand.ILifted (convertClause cl)
-- Ignore/Exclamation: ! Clause - using QQ pattern
convertClause [clause| ! $cl |] =
    Hand.IIgnore (convertClause cl)
-- Sequence: Clause Clause (binary, need to flatten)
convertClause (Gen.Ctr__Clause__12 left right) =
    flattenSeq left right
-- Alternation: Clause | Clause (binary, need to flatten)
convertClause (Gen.Ctr__Clause__14 left right) =
    flattenAlt left right
-- Anti-quotation
convertClause (Gen.Anti_Clause s) = error $ "Unexpected anti-quotation in Clause: " ++ s

-- | Extract optional delimiter from OptDelim using QQ pattern
extractOptDelim :: Gen.OptDelim -> Maybe Hand.IClause
extractOptDelim [optDelim| |] = Nothing  -- No delimiter (empty optional)
extractOptDelim [optDelim| ~ $cl |] = Just (convertClause cl)
extractOptDelim (Gen.Anti_OptDelim s) = error $ "Unexpected anti-quotation in OptDelim: " ++ s

-- | Flatten binary sequence into list-based ISeq
flattenSeq :: Gen.Clause -> Gen.Clause -> Hand.IClause
flattenSeq left right =
    let leftClauses = collectSeq left
        rightClauses = collectSeq right
    in case leftClauses ++ rightClauses of
        [single] -> single
        multiple -> Hand.ISeq multiple

-- | Collect sequence elements from binary representation
collectSeq :: Gen.Clause -> [Hand.IClause]
collectSeq cl = case cl of
    Gen.Ctr__Clause__12 l r -> collectSeq l ++ collectSeq r
    other -> [convertClause other]

-- | Flatten binary alternation into list-based IAlt
flattenAlt :: Gen.Clause -> Gen.Clause -> Hand.IClause
flattenAlt left right =
    let leftAlts = collectAlt left
        rightAlts = collectAlt right
    in case leftAlts ++ rightAlts of
        [single] -> single
        multiple -> Hand.IAlt multiple

-- | Collect alternation elements from binary representation
collectAlt :: Gen.Clause -> [Hand.IClause]
collectAlt cl = case cl of
    Gen.Ctr__Clause__14 l r -> collectAlt l ++ collectAlt r
    other -> [convertClause other]
