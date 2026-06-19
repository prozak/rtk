-- | The pretty-printer generator: the "emit" third of "Rewrite ToolKit".
-- @--generate-pp@ runs this over a 'NormalGrammar' to produce @\<Name\>PP.hs@,
-- a module of @pp\<Type\>@ functions that turn the AST RTK generates for a
-- grammar back into source text.
--
-- Two layouts are baked at generation time ('PPLayout'):
--
--   * 'PPFlat' (task 9a, the default): correctness-first, NOT pretty. Exactly
--     one space between tokens, no indentation, no alignment, and no
--     parentheses of the generator's own.
--
--   * 'PPBlock' (task 9b): adds indentation and line breaks so output for
--     bracket-structured languages reads like hand-written source. Layout is
--     derived structurally - indentation comes purely from /block lists/
--     (statement\/declaration lists: an 'STMany' with no separator or a @;@
--     separator), which wrap their elements in an indented, line-broken
--     block. The flanking delimiters (@{@ @}@ for C\/Java, @begin@ @end@ for
--     PL\/0) need no special handling: they render inline around the list and
--     the list supplies the indent, so there is no bracket charset baked into
--     rtk and a grammar with no block lists simply degrades to flat output.
--
-- BOTH layouts keep the same guarantee and the same dependency discipline:
-- the only promise is the semantic round-trip @parse (print ast) == ast@ (the
-- AST is lossy - comments and the original whitespace are gone, so
-- byte-faithful reproduction is impossible and never implied), and the
-- generated module depends only on @base@ (it imports its grammar's
-- @\<Name\>Parser@ AST types, "Data.List", and an in-module ~20-line layout
-- engine for block mode). Layout is whitespace, and every grammar in the
-- corpus ignores whitespace, so block layout cannot change the parse - the
-- 9a round-trip test proves it stays green. The 'layoutSensitive' guard makes
-- that assumption explicit and falls back to flat should a layout-sensitive
-- grammar ever exist (TODO(#95)/task 12).
--
-- The structure mirrors "GenAST": the same group-to-constructor view
-- ('normalRulesNamed'), the same rule-to-type map ('rulesMap') and the same
-- "this alternative produces a constructor" test ('needGenereateAlt'), so the
-- printer walks exactly the constructor set the AST declares. Terminal text
-- for fixed-literal tokens is reconstructed with the shared
-- 'literalTokenText' helper that "GenY" also uses.
module GenPP (genPP, PPLayout(..))
    where

import Text.PrettyPrint
import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax
import Grammar (literalTokenText)
import GenAST (isAntiConstructor, normalRulesNamed, RulesMap, rulesMap,
               findRuleDataTypeName, needGenereateAlt)
import Diagnostics (Diagnostic(..))

-- | Layout selected at generation time (@--pp-layout=flat|block@).
data PPLayout = PPFlat | PPBlock
    deriving (Eq, Show)

-- | Token name -> its lexical rule, for reconstructing terminal text and for
-- telling a payload token (a leaf carrying a stored value) apart from a
-- nonterminal reference.
type LexMap = Map.Map String LexicalRule

genPP :: PPLayout -> NormalGrammar -> Either Diagnostic String
genPP PPFlat  grammar = genPPFlat grammar
genPP PPBlock grammar
    -- Forward guard (TODO(#95)/task 12): block layout assumes the lexer
    -- ignores whitespace. That holds for every grammar today; if a
    -- layout-sensitive (offside/indentation) lexer ever lands, block layout
    -- could change the token stream and thus the parse, so fall back to flat
    -- with a one-line note instead of silently emitting a wrong program.
    | layoutSensitive grammar = withNote <$> genPPFlat grammar
    | otherwise               = genPPBlock grammar
  where
    withNote = insertNote
        ("-- NOTE: block layout was requested but the grammar's lexer is "
         ++ "layout-sensitive; emitting flat layout (whitespace would change "
         ++ "the parse).")

-- | Whether the grammar's lexer is sensitive to whitespace/indentation. There
-- is no way to express offside or indentation lexing in the grammar language
-- yet (issue #95 / task 12), so this is always 'False' for now; the guard
-- above is the place to teach block layout to bow out once that changes.
layoutSensitive :: NormalGrammar -> Bool
layoutSensitive _ = False -- TODO(#95): detect offside/indentation-sensitive lexers

-- | Splice an extra comment line in right after the provenance banner (the
-- first line) of a generated module. Only used by the layout-sensitive
-- fallback, which is unreachable today.
insertNote :: String -> String -> String
insertNote note src = case lines src of
    (banner : rest) -> unlines (banner : note : rest)
    []              -> src

--------------------------------------------------------------------------------
-- Flat layout (task 9a): byte-for-byte the original generator.
--------------------------------------------------------------------------------

genPPFlat :: NormalGrammar -> Either Diagnostic String
genPPFlat grammar = do
    bodies <- mapM (genPPRule rmap lmap) (normalRulesNamed (getSyntaxRuleGroups grammar))
    return $ render $ vcat (header : map (\d -> text "" $$ d) bodies)
  where
    name = getNGrammarName grammar
    rmap = rulesMap grammar
    lmap = mkLexMap grammar
    -- intercalate is the only base import the printer ever needs, and only
    -- list-typed rules use it; omit it otherwise so the module stays
    -- import-clean (no unused import even under -Werror).
    usesIntercalate = grammarHasList grammar
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
        let lhs = text ("pp" ++ typeName) <+> parens (armPattern ctor clauses)
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

unwordsExpr :: [Doc] -> Doc
unwordsExpr exprs = text "unwords" <+> brackets (hcat (punctuate (text ", ") exprs))

--------------------------------------------------------------------------------
-- Block layout (task 9b): indentation driven structurally by block lists.
--------------------------------------------------------------------------------

-- The set of types whose top clause is a block list (an STMany with no
-- separator or a ';' separator). A field of such a type renders as an
-- indented, line-broken block; this set is what lets the per-clause walk stay
-- delimiter-agnostic - it splices lay<Type> and the list type supplies the
-- indentation.
genPPBlock :: NormalGrammar -> Either Diagnostic String
genPPBlock grammar = do
    bodies <- mapM (genBlockRule rmap lmap blocky) named
    return $ render $ vcat (header : engineDoc : map (\d -> text "" $$ d) bodies)
  where
    name = getNGrammarName grammar
    rmap = rulesMap grammar
    lmap = mkLexMap grammar
    named = normalRulesNamed (getSyntaxRuleGroups grammar)
    blocky = blockyTypes lmap named
    usesIntercalate = grammarHasList grammar
    header = vcat $
        [ text (provenanceBanner name)
        , text "-- Block-layout pretty-printer (task 9b): indentation and line breaks for"
        , text "-- bracket-structured languages. Layout comes structurally from block lists"
        , text "-- (statement/declaration lists), so it adds no parentheses and no charset is"
        , text "-- baked in. The only guarantee is the semantic round-trip parse (print ast)"
        , text "-- == ast; comments and the original whitespace are not recovered (lossy)."
        , text ("module " ++ name ++ "PP where")
        , text ("import " ++ name ++ "Parser")
        ] ++ [ text "import Data.List (intercalate)" | usesIntercalate ]
    engineDoc = text "" $$ vcat (map text ppEngine)

genBlockRule :: RulesMap -> LexMap -> Set.Set ID -> (ID, SyntaxTopClause) -> Either Diagnostic Doc
genBlockRule rmap lmap blocky (typeName, clause) = case clause of
    STMany _ cl msep -> genBlockList rmap lmap blocky typeName cl msep
    STOpt cl         -> genBlockOpt rmap lmap typeName cl
    STAltOfSeq seqs  -> genBlockData rmap lmap typeName seqs

-- The types whose values are statement/declaration-shaped: a type some of
-- whose constructors contain a statement-terminator literal (';'). A
-- no-separator list of such a type is a block list (each element wants its
-- own line); a no-separator list of anything else stays inline. This keeps
-- statement and field-declaration lists breaking while leaving inline
-- repetitions - modifier lists ('public' 'static' ...) and dotted-name
-- continuations ('.' id)* - on one line. (';' is the statement terminator
-- the layout heuristic already recognizes; ';'-separated lists are blocks
-- regardless, see isBlockSep.)
blockyTypes :: LexMap -> [(ID, SyntaxTopClause)] -> Set.Set ID
blockyTypes lmap named = Set.fromList
    [ ty | (ty, STAltOfSeq seqs) <- named
         , any hasTerminator (filter needGenereateAlt seqs) ]
  where
    hasTerminator (STSeq _ clauses) = any isTerminator clauses
    isTerminator (SSIgnore x) = ignoreLiteral lmap x `elem` terminatorLiterals
    isTerminator _            = False

-- Statement-terminator literals recognized by the block-layout heuristic.
terminatorLiterals :: [String]
terminatorLiterals = [";"]

-- The public String entry (ppRender . lay) plus the type's signature, shared
-- by every block-mode rule. lay<Type> :: <Type> -> [PpItem] does the work;
-- pp<Type> :: <Type> -> String renders it, so callers (and the round-trip
-- test) use the same pp<Type> name in either layout.
blockEntry :: String -> [Doc]
blockEntry typeName =
    [ ppSig typeName
    , text ("pp" ++ typeName) <+> text "=" <+> text "ppRender ." <+> text ("lay" ++ typeName)
    , laySig typeName
    ]

laySig :: String -> Doc
laySig typeName =
    text ("lay" ++ typeName) <+> text "::" <+> text typeName <+> text "-> [PpItem]"

genBlockData :: RulesMap -> LexMap -> ID -> [STSeq] -> Either Diagnostic Doc
genBlockData rmap lmap typeName seqs = do
    arms <- mapM genArm (filter needGenereateAlt seqs)
    return $ vcat (blockEntry typeName ++ arms)
  where
    genArm (STSeq ctor clauses) = do
        pieces <- mapM (blockPiece rmap lmap) (numberFields clauses)
        let lhs = text ("lay" ++ typeName) <+> parens (armPattern ctor clauses)
        return $ lhs <+> text "=" <+> concatExpr pieces

-- A block list indents and line-breaks its elements; an inline list (any
-- other separator, e.g. ',') stays on one line like flat mode. An empty list
-- emits nothing in either case, so an empty '{ }'/'begin end' stays inline.
genBlockList :: RulesMap -> LexMap -> Set.Set ID -> ID -> SyntaxSimpleClause
             -> Maybe SyntaxSimpleClause -> Either Diagnostic Doc
genBlockList rmap lmap blocky typeName elemCl msep = do
    elemFun <- blockElemFun rmap lmap elemCl
    let sepText  = fmap (ignoreLiteral lmap . clauseId) msep
        elemTy   = Map.lookup (clauseId elemCl) rmap
        elemBlocky = maybe False (`Set.member` blocky) elemTy
        elems    = "(map " ++ elemFun ++ " xs)"
        body
          | isBlockSep elemBlocky sepText =
              let between = case sepText of
                              Just s | s /= "" -> "[PpTok " ++ show s ++ ", PpBreak]"
                              _                -> "[PpBreak]"
              in "lay" ++ typeName ++ " xs = if null xs then [] else [PpOpen] ++ intercalate "
                 ++ between ++ " " ++ elems ++ " ++ [PpClose]"
          | otherwise =
              "lay" ++ typeName ++ " xs = intercalate [PpTok "
              ++ show (maybe "" id sepText) ++ "] " ++ elems
    return $ vcat (blockEntry typeName ++ [text body])

genBlockOpt :: RulesMap -> LexMap -> ID -> SyntaxSimpleClause -> Either Diagnostic Doc
genBlockOpt rmap lmap typeName elemCl = do
    elemFun <- blockElemFun rmap lmap elemCl
    return $ vcat (blockEntry typeName ++ [text ("lay" ++ typeName ++ " = maybe [] " ++ elemFun)])

-- The function to map/maybe over a list or optional element in block mode: a
-- nonterminal's lay<Type>, or a [PpItem]-producing lambda for a token leaf (a
-- list/option over a value-less token is rejected upstream).
blockElemFun :: RulesMap -> LexMap -> SyntaxSimpleClause -> Either Diagnostic String
blockElemFun rmap lmap cl = case cl of
    SSId x     -> resolve x
    SSLifted x -> resolve x
    SSIgnore x -> Left (ppInternal ("list/optional element is the ignored token '"
                                    ++ x ++ "'"))
  where
    resolve x = case Map.lookup x lmap of
        Just LexicalRule{ getLRuleDataType = "String" } -> Right "(\\v -> [PpTok v])"
        Just LexicalRule{}                              -> Right "(\\v -> [PpTok (show v)])"
        Just MacroRule{}                                -> Right "(\\v -> [PpTok v])"
        Nothing -> do ty <- findRuleDataTypeName rmap x x
                      return ("lay" ++ ty)

-- Whether a list lays out as a block (indented, one element per line). A ';'
-- separator always means a statement list. A separatorless list is a block
-- only when its elements are themselves multi-token ('blockyTypes'); a
-- separatorless list of single-token keywords (modifiers) stays inline. Any
-- other separator (notably ',') stays inline.
isBlockSep :: Bool -> Maybe String -> Bool
isBlockSep _          (Just ";") = True
isBlockSep elemBlocky Nothing    = elemBlocky
isBlockSep _          _          = False

-- One clause as a [PpItem] expression for block mode (mirrors genExprs but
-- builds layout items): a fixed literal is a PpTok, a payload token its
-- stored value, a nonterminal a spliced lay<Type>.
blockPiece :: RulesMap -> LexMap -> (Int, SyntaxSimpleClause) -> Either Diagnostic Doc
blockPiece _    lmap (_, SSIgnore x) = Right $ text ("[PpTok " ++ show (ignoreLiteral lmap x) ++ "]")
blockPiece rmap lmap (i, SSId x) = case Map.lookup x lmap of
    Just lr -> Right (leafItems lr ("x" ++ show i))
    Nothing -> do ty <- findRuleDataTypeName rmap x x
                  return $ text ("lay" ++ ty) <+> text ("x" ++ show i)
blockPiece _ _ (_, SSLifted x) =
    Left (ppInternal ("lifted clause '" ++ x ++ "' survived into a printable constructor"))

-- A payload token leaf as a [PpItem]: String verbatim, otherwise via show.
leafItems :: LexicalRule -> String -> Doc
leafItems LexicalRule{ getLRuleDataType = "String" } var = text ("[PpTok " ++ var ++ "]")
leafItems LexicalRule{}                              var = text ("[PpTok (show " ++ var ++ ")]")
leafItems MacroRule{}                                var = text ("[PpTok " ++ var ++ "]")

concatExpr :: [Doc] -> Doc
concatExpr []     = text "[]"
concatExpr pieces = text "concat" <+> brackets (hcat (punctuate (text ", ") pieces))

-- The block-layout engine, emitted verbatim into the generated module. It
-- depends only on base. render walks the item stream once tracking indent
-- depth: PpOpen indents and arms a new line, PpClose dedents and arms a new
-- line, PpBreak arms a new line at the current indent. Directives only arm a
-- pending line (the indentation is written when the next token appears), so
-- consecutive directives collapse to one line break and a trailing directive
-- emits nothing - no blank lines, no trailing whitespace.
ppEngine :: [String]
ppEngine =
    [ "data PpItem = PpTok String | PpOpen | PpClose | PpBreak"
    , "ppRender :: [PpItem] -> String"
    , "ppRender = start"
    , "  where"
    , "    start []            = \"\""
    , "    start (PpTok s : r) = s ++ inLine 0 r"
    , "    start (PpOpen  : r) = fresh 1 r"
    , "    start (PpClose : r) = fresh 0 r"
    , "    start (PpBreak : r) = fresh 0 r"
    , "    inLine _ []            = \"\""
    , "    inLine n (PpTok s : r) = ' ' : s ++ inLine n r"
    , "    inLine n (PpOpen  : r) = fresh (n + 1) r"
    , "    inLine n (PpClose : r) = fresh (max 0 (n - 1)) r"
    , "    inLine n (PpBreak : r) = fresh n r"
    , "    fresh _ []            = \"\""
    , "    fresh n (PpTok s : r) = '\\n' : (replicate (n * 2) ' ' ++ s) ++ inLine n r"
    , "    fresh n (PpOpen  : r) = fresh (n + 1) r"
    , "    fresh n (PpClose : r) = fresh (max 0 (n - 1)) r"
    , "    fresh n (PpBreak : r) = fresh n r"
    ]

--------------------------------------------------------------------------------
-- Shared helpers
--------------------------------------------------------------------------------

mkLexMap :: NormalGrammar -> LexMap
mkLexMap grammar = Map.fromList [ (getLRuleName r, r) | r <- getLexicalRules grammar ]

grammarHasList :: NormalGrammar -> Bool
grammarHasList grammar = any isListClause (normalRulesNamed (getSyntaxRuleGroups grammar))
  where isListClause (_, STMany{}) = True
        isListClause _             = False

-- The constructor pattern for an alternative: the constructor, the leading
-- RtkPos wildcard (every non-Anti_ constructor has one) and a field variable
-- per SSId clause, in order.
armPattern :: ConstructorName -> [SyntaxSimpleClause] -> Doc
armPattern ctor clauses = hsep (text ctor : map text (posPat ++ vars))
  where nFields = length [ () | SSId{} <- clauses ]
        vars    = [ "x" ++ show i | i <- [1 .. nFields] ]
        posPat  = if isAntiConstructor ctor then [] else ["_"]

-- The source text of a fixed-literal token (keyword/punctuation). A token
-- whose clause is not a bare literal - or a name that is no lexical rule at
-- all (an ignored !nonterminal) - has no recoverable spelling, so its name is
-- the fallback; such a gap surfaces as a round-trip failure, by design.
ignoreLiteral :: LexMap -> ID -> String
ignoreLiteral lmap x =
    maybe x id (Map.lookup x lmap >>= literalTokenText . getLClause)

clauseId :: SyntaxSimpleClause -> ID
clauseId (SSId n)     = n
clauseId (SSLifted n) = n
clauseId (SSIgnore n) = n

numberFields :: [SyntaxSimpleClause] -> [(Int, SyntaxSimpleClause)]
numberFields = go 1
  where go _ []                 = []
        go i (c@SSId{} : cs)     = (i, c) : go (i + 1) cs
        go i (c        : cs)     = (i, c) : go i cs

ppInternal :: String -> Diagnostic
ppInternal msg = Diagnostic Nothing Nothing ("rtk internal error (GenPP): " ++ msg)
