{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module ASTAdapter
    ( convertGrammar
    ) where

-- This module converts the auto-generated AST from GrammarParser.hs
-- to the hand-written AST format used by the rest of RTK.
--
-- APPROACH: Use RTK's own quasi-quotation features to pattern match
-- on the generated AST. This demonstrates dogfooding and makes the
-- conversion code more declarative and readable.

import qualified Parser as Hand
-- Once generated files exist, uncomment:
-- import qualified GrammarParser as Gen
-- import qualified GrammarQQ as QQ

-- | Convert generated Grammar to hand-written InitialGrammar
convertGrammar :: Hand.InitialGrammar  -- Placeholder signature
convertGrammar = error "ASTAdapter.convertGrammar not yet implemented - waiting for generated parser files"

{- TODO: Implement after generating files

Once GrammarParser.y, GrammarLexer.x, and GrammarQQ.hs are generated,
we can use RTK's own quasi-quotation to elegantly pattern match on the
generated AST.

## APPROACH 1: Using QuasiQuotation for Pattern Matching

```haskell
{-# LANGUAGE QuasiQuotes #-}
import qualified GrammarParser as Gen
import qualified GrammarQQ as QQ

-- Top-level conversion using quasi-quotation
convertGrammar :: Gen.Grammar -> Hand.InitialGrammar
convertGrammar [QQ.grammar| grammar $name ';' $imports $rules |] =
    Hand.InitialGrammar
        { Hand.getIGrammarName = name
        , Hand.getImports = imports
        , Hand.getIRules = map convertRule rules
        }

-- Convert a rule using pattern matching with quasi-quotation
convertRule :: Gen.Rule -> Hand.IRule

-- Simple rule: Name = Clause ;
convertRule [QQ.rule| $name = $clause ; |] =
    Hand.IRule
        { Hand.getIDataTypeName = Nothing
        , Hand.getIDataFunc = Nothing
        , Hand.getIRuleName = name
        , Hand.getIClause = convertClause clause
        , Hand.getIRuleOptions = []
        }

-- Typed rule: Type : Name = Clause ;
convertRule [QQ.rule| $dtype : $name = $clause ; |] =
    Hand.IRule
        { Hand.getIDataTypeName = Just dtype
        , Hand.getIDataFunc = Nothing
        , Hand.getIRuleName = name
        , Hand.getIClause = convertClause clause
        , Hand.getIRuleOptions = []
        }

-- Typed rule with function: Type . Func : Name = Clause ;
convertRule [QQ.rule| $dtype . $func : $name = $clause ; |] =
    Hand.IRule
        { Hand.getIDataTypeName = Just dtype
        , Hand.getIDataFunc = Just func
        , Hand.getIRuleName = name
        , Hand.getIClause = convertClause clause
        , Hand.getIRuleOptions = []
        }

-- Typed rule without data type: . Func : Name = Clause ;
convertRule [QQ.rule| . $func : $name = $clause ; |] =
    Hand.IRule
        { Hand.getIDataTypeName = Nothing
        , Hand.getIDataFunc = Just func
        , Hand.getIRuleName = name
        , Hand.getIClause = convertClause clause
        , Hand.getIRuleOptions = []
        }

-- Convert clauses using quasi-quotation
convertClause :: Gen.Clause -> Hand.IClause

-- Identifier: Name
convertClause [QQ.clause| $name:id |] =
    Hand.IId name

-- String literal: 'string'
convertClause [QQ.clause| $str:str |] =
    Hand.IStrLit str

-- Dot: .
convertClause [QQ.clause| . |] =
    Hand.IDot

-- Regex: [pattern]
convertClause [QQ.clause| $regex:regex |] =
    Hand.IRegExpLit regex

-- Star: Clause *
convertClause [QQ.clause| $clause * |] =
    Hand.IStar (convertClause clause) Nothing

-- Star with delimiter: Clause * ~ Delimiter
convertClause [QQ.clause| $clause * ~ $delim |] =
    Hand.IStar (convertClause clause) (Just $ convertClause delim)

-- Plus: Clause +
convertClause [QQ.clause| $clause + |] =
    Hand.IPlus (convertClause clause) Nothing

-- Plus with delimiter: Clause + ~ Delimiter
convertClause [QQ.clause| $clause + ~ $delim |] =
    Hand.IPlus (convertClause clause) (Just $ convertClause delim)

-- Optional: Clause ?
convertClause [QQ.clause| $clause ? |] =
    Hand.IOpt (convertClause clause)

-- Alternation: Clause | Clause | ...
convertClause [QQ.clause| $clauses:alt |] =
    Hand.IAlt (map convertClause clauses)

-- Sequence: Clause Clause ...
convertClause [QQ.clause| $clauses:seq |] =
    Hand.ISeq (map convertClause clauses)

-- Lifted: , Clause
convertClause [QQ.clause| , $clause |] =
    Hand.ILifted (convertClause clause)

-- Ignored: ! Clause
convertClause [QQ.clause| ! $clause |] =
    Hand.IIgnore (convertClause clause)

-- Parenthesized: ( Clause )
convertClause [QQ.clause| ( $clause ) |] =
    convertClause clause  -- Unwrap parentheses
```

## APPROACH 2: Hybrid - QuasiQuotation + Traditional Pattern Matching

If quasi-quotation for pattern matching isn't fully supported, we can use QQ
for building/matching at a high level and traditional pattern matching for details:

```haskell
import qualified GrammarQQ as QQ
import Language.Haskell.TH.Quote (QuasiQuoter(..))

convertGrammar :: Gen.Grammar -> Hand.InitialGrammar
convertGrammar genGrammar =
    case genGrammar of
        -- Try to match using QQ pattern
        (matchGrammarPattern -> Just (name, imports, rules)) ->
            Hand.InitialGrammar
                { Hand.getIGrammarName = name
                , Hand.getImports = imports
                , Hand.getIRules = map convertRule rules
                }
        -- Fallback to constructor matching
        Gen.Grammar strLit importsOpt ruleList ->
            Hand.InitialGrammar
                { Hand.getIGrammarName = extractStr strLit
                , Hand.getImports = extractImports importsOpt
                , Hand.getIRules = map convertRule (extractRules ruleList)
                }

-- Helper to match grammar pattern using QQ
matchGrammarPattern :: Gen.Grammar -> Maybe (String, String, [Gen.Rule])
matchGrammarPattern gram =
    -- Use GrammarQQ to parse and extract components
    case QQ.parseGrammar gram of
        Just components -> Just components
        Nothing -> Nothing
```

## APPROACH 3: String-Based QuasiQuotation (Pragmatic)

If generated QQ doesn't support pattern matching, use it for validation/parsing:

```haskell
convertGrammar :: Gen.Grammar -> Hand.InitialGrammar
convertGrammar genGrammar =
    let grammarStr = Gen.prettyPrint genGrammar  -- Convert AST back to string
        parsed = QQ.parseWithGrammarQQ grammarStr  -- Re-parse with QQ
    in case parsed of
        Just [QQ.grammarMatch| grammar $name ';' $imports $rules |] ->
            -- Build hand-written AST from matched components
            Hand.InitialGrammar name imports (map convertRule rules)
        Nothing ->
            error $ "Failed to parse generated grammar: " ++ grammarStr
```

## Benefits of QuasiQuotation Approach

1. **Dogfooding**: RTK uses its own features to parse its own grammar
2. **Declarative**: Pattern matching reads like the grammar itself
3. **Maintainable**: Changes to grammar.pg automatically update patterns
4. **Self-Documenting**: The QQ patterns show the grammar structure clearly
5. **Type-Safe**: QQ ensures we're matching valid grammar constructs

## Implementation Strategy

1. Generate GrammarQQ.hs: `rtk test-grammars/grammar.pg src/generated/`
2. Inspect GrammarQQ.hs to see what quasi-quoters are available
3. Check if QQ supports pattern matching (quotePat implementation)
4. If yes: Use Approach 1 (full QQ pattern matching)
5. If partial: Use Approach 2 (hybrid)
6. If no pattern support: Use Approach 3 (string-based) or fallback to traditional

## Next Steps

After generating files, check GrammarQQ.hs for:
```haskell
-- Look for these exports:
module GrammarQQ where

grammar :: QuasiQuoter  -- For matching Grammar
rule :: QuasiQuoter     -- For matching Rule
clause :: QuasiQuoter   -- For matching Clause

-- Check if quotePat is implemented:
grammar = QuasiQuoter
    { quoteExp = ...
    , quotePat = ...  -- If this exists, Approach 1 works!
    , quoteType = ...
    , quoteDec = ...
    }
```

If quotePat is defined, we can use quasi-quotation for pattern matching.
If not, we'll fall back to traditional pattern matching but can still use
QQ for validation and testing.

-}

