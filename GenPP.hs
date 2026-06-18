-- | The pretty-printer generator (task 9): the "emit" third of "Rewrite
-- ToolKit". @--generate-pp@ runs this over a 'NormalGrammar' to produce
-- @\<Name\>PP.hs@, a module of @pp\<Type\>@ functions that turn the AST RTK
-- generates for a grammar back into source text.
--
-- v1 is correctness-first, NOT pretty. The only promise is the semantic
-- round-trip @parse (print ast) == ast@ (the AST is lossy - comments and the
-- original whitespace are gone, so byte-faithful reproduction is impossible
-- and never implied). Layout is deliberately trivial: exactly one space
-- between emitted tokens, no indentation and no alignment. Parenthesization
-- is whatever the grammar's own structure produces; this generator adds none
-- of its own. The generated round-trip test (see @make test-pp@) is the
-- correctness oracle - under-parenthesization becomes a failing test, not a
-- silently wrong program.
--
-- The generated module depends only on @base@ (it imports its grammar's
-- @\<Name\>Parser@ AST types and "Data.List"), keeping the 8b
-- dependency-discipline rule: no rtk imports, no new packages.
--
-- The structure mirrors "GenAST": the same group-to-constructor view
-- ('normalRulesNamed'), the same rule-to-type map ('rulesMap') and the same
-- "this alternative produces a constructor" test ('needGenereateAlt'), so the
-- printer walks exactly the constructor set the AST declares. Terminal text
-- for fixed-literal tokens is reconstructed with the shared
-- 'literalTokenText' helper that "GenY" also uses.
module GenPP (genPP)
    where

import Text.PrettyPrint
import qualified Data.Map as Map

import Syntax
import Grammar (literalTokenText)
import GenAST (isAntiConstructor, normalRulesNamed, RulesMap, rulesMap,
               findRuleDataTypeName, needGenereateAlt)
import Diagnostics (Diagnostic(..))

-- | Token name -> its lexical rule, for reconstructing terminal text and for
-- telling a payload token (a leaf carrying a stored value) apart from a
-- nonterminal reference.
type LexMap = Map.Map String LexicalRule

genPP :: NormalGrammar -> Either Diagnostic String
genPP grammar = do
    bodies <- mapM (genPPRule rmap lmap) (normalRulesNamed (getSyntaxRuleGroups grammar))
    return $ render $ vcat (header : map (\d -> text "" $$ d) bodies)
  where
    name = getNGrammarName grammar
    rmap = rulesMap grammar
    lmap = Map.fromList [ (getLRuleName r, r) | r <- getLexicalRules grammar ]
    -- intercalate is the only base import the printer ever needs, and only
    -- list-typed rules use it; omit it otherwise so the module stays
    -- import-clean (no unused import even under -Werror).
    usesIntercalate = any isListClause (normalRulesNamed (getSyntaxRuleGroups grammar))
    isListClause (_, STMany{}) = True
    isListClause _             = False
    header = vcat $
        [ text (provenanceBanner name)
        , text "-- v1 pretty-printer (task 9): correctness-first, not pretty. Emits exactly"
        , text "-- one space between tokens, with no indentation or alignment. The only"
        , text "-- guarantee is the semantic round-trip parse (print ast) == ast; comments"
        , text "-- and the original whitespace are not recovered (the AST is lossy)."
        , text ("module " ++ name ++ "PP where")
        , text ("import " ++ name ++ "Parser")
        ] ++ [ text "import Data.List (intercalate)" | usesIntercalate ]

genPPRule :: RulesMap -> LexMap -> (ID, SyntaxTopClause) -> Either Diagnostic Doc
genPPRule rmap lmap (typeName, clause) = case clause of
    STMany _ cl msep   -> genPPList rmap lmap typeName cl msep
    STOpt cl           -> genPPOpt rmap lmap typeName cl
    STAltOfSeq seqs    -> genPPData rmap lmap typeName seqs

ppSig :: String -> Doc
ppSig typeName =
    text ("pp" ++ typeName) <+> text "::" <+> text typeName <+> text "->" <+> text "String"

-- A data type: one equation per constructor (exactly the alternatives that
-- produce one, by the same test GenAST uses). Anti_* constructors carry no
-- leading RtkPos field, so their pattern skips the wildcard.
genPPData :: RulesMap -> LexMap -> ID -> [STSeq] -> Either Diagnostic Doc
genPPData rmap lmap typeName seqs = do
    arms <- mapM genArm (filter needGenereateAlt seqs)
    return $ vcat (ppSig typeName : arms)
  where
    genArm (STSeq ctor clauses) = do
        exprs <- genExprs rmap lmap clauses
        let isAnti  = isAntiConstructor ctor
            nFields = length [ () | SSId{} <- clauses ]
            vars    = [ "x" ++ show i | i <- [1 .. nFields] ]
            posPat  = if isAnti then [] else ["_"]
            lhs     = text ("pp" ++ typeName)
                      <+> parens (hsep (text ctor : map text (posPat ++ vars)))
        return $ lhs <+> text "=" <+> unwordsExpr exprs

-- A list alias (type T = [Elem]): join the element printer with the
-- separator's reconstructed literal (space-padded so it stays its own token),
-- or a plain space when the list has no separator.
genPPList :: RulesMap -> LexMap -> ID -> SyntaxSimpleClause
          -> Maybe SyntaxSimpleClause -> Either Diagnostic Doc
genPPList rmap lmap typeName elemCl msep = do
    elemPP <- elemPrinter rmap lmap elemCl
    let sepText = case msep of
                    Just sc -> " " ++ ignoreLiteral lmap (clauseId sc) ++ " "
                    Nothing -> " "
        body = text ("pp" ++ typeName) <+> text "xs" <+> text "="
               <+> text "intercalate" <+> text (show sepText)
               <+> parens (text "map" <+> elemPP <+> text "xs")
    return $ vcat [ppSig typeName, body]

-- An optional alias (type T = Maybe Elem): print the element when present,
-- nothing when absent.
genPPOpt :: RulesMap -> LexMap -> ID -> SyntaxSimpleClause -> Either Diagnostic Doc
genPPOpt rmap lmap typeName elemCl = do
    elemPP <- elemPrinter rmap lmap elemCl
    let body = text ("pp" ++ typeName) <+> text "=" <+> text "maybe"
               <+> text (show "") <+> elemPP
    return $ vcat [ppSig typeName, body]

-- The expressions one alternative's clauses emit, left to right. Each is a
-- Haskell String expression; the arm 'unwords' them. SSId clauses consume a
-- field variable (x1, x2, ...) in order; SSIgnore clauses emit a fixed
-- literal and consume none.
genExprs :: RulesMap -> LexMap -> [SyntaxSimpleClause] -> Either Diagnostic [Doc]
genExprs rmap lmap = go 1
  where
    go :: Int -> [SyntaxSimpleClause] -> Either Diagnostic [Doc]
    go _ []       = Right []
    go i (c : cs) = case c of
        SSIgnore x -> (text (show (ignoreLiteral lmap x)) :) <$> go i cs
        SSId x     -> do e    <- idExpr rmap lmap x ("x" ++ show i)
                         rest <- go (i + 1) cs
                         return (e : rest)
        -- A lifted clause never reaches a constructor-producing alternative:
        -- an all-lifted sequence is filtered out by needGenereateAlt, and a
        -- sequence mixing a lifted clause with others is rejected upstream
        -- (Grammar.isClauseSeqLifted). So this is a pipeline bug, not a gap.
        SSLifted x -> Left (ppInternal ("lifted clause '" ++ x
                                        ++ "' survived into a printable constructor"))

-- The expression for one SSId field: a payload token contributes its stored
-- value; a nonterminal recurses through the printer of its rule's type.
idExpr :: RulesMap -> LexMap -> ID -> String -> Either Diagnostic Doc
idExpr rmap lmap x var = case Map.lookup x lmap of
    Just lr -> Right (leafExpr lr var)
    Nothing -> do
        ty <- findRuleDataTypeName rmap x x
        return $ parens (text ("pp" ++ ty) <+> text var)

-- The printer to map/maybe over a list or optional element: the element is a
-- nonterminal (a list/option over a value-less token is rejected upstream),
-- so this is just its type's printer.
elemPrinter :: RulesMap -> LexMap -> SyntaxSimpleClause -> Either Diagnostic Doc
elemPrinter rmap lmap cl = case cl of
    SSId x     -> resolve x
    SSLifted x -> resolve x
    SSIgnore x -> Left (ppInternal ("list/optional element is the ignored token '"
                                    ++ x ++ "'"))
  where
    resolve x = case Map.lookup x lmap of
        Just lr -> Right (leafFun lr)
        Nothing -> do ty <- findRuleDataTypeName rmap x x
                      return $ text ("pp" ++ ty)

-- A payload token leaf as an expression over its bound variable: a String
-- payload is emitted verbatim, anything else via 'show' (best effort; a
-- non-String token type round-trips only when 'show' reproduces its source).
leafExpr :: LexicalRule -> String -> Doc
leafExpr LexicalRule{ getLRuleDataType = "String" } var = text var
leafExpr LexicalRule{}                              var = parens (text "show" <+> text var)
leafExpr MacroRule{}                                var = text var

-- The same leaf as a function, for map/maybe over a token element.
leafFun :: LexicalRule -> Doc
leafFun LexicalRule{ getLRuleDataType = "String" } = text "id"
leafFun LexicalRule{}                              = text "show"
leafFun MacroRule{}                                = text "id"

-- The source text of a fixed-literal token (keyword/punctuation). A token
-- whose clause is not a bare literal - or a name that is no lexical rule at
-- all (an ignored !nonterminal) - has no recoverable spelling, so its name is
-- the fallback; such a gap surfaces as a round-trip failure, by design.
ignoreLiteral :: LexMap -> ID -> String
ignoreLiteral lmap x =
    maybe x id (Map.lookup x lmap >>= literalTokenText . getLClause)

unwordsExpr :: [Doc] -> Doc
unwordsExpr exprs = text "unwords" <+> brackets (hcat (punctuate (text ", ") exprs))

clauseId :: SyntaxSimpleClause -> ID
clauseId (SSId n)     = n
clauseId (SSLifted n) = n
clauseId (SSIgnore n) = n

ppInternal :: String -> Diagnostic
ppInternal msg = Diagnostic Nothing Nothing ("rtk internal error (GenPP): " ++ msg)
