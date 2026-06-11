{-# LANGUAGE QuasiQuotes #-}
module GenY (genY)
    where

import Parser
import Diagnostics (Diagnostic)
import Text.PrettyPrint hiding ((<>))
import qualified Data.List as List
import qualified Data.Set as Set
import GenAST
import Grammar
import StrQuote

genY :: NormalGrammar -> Either Diagnostic String
genY g@(NormalGrammar name srules lex_rules _ _ _ _) = do
    ast <- genAST g
    return $ render $ vcat [
                   text header,
                   nl,
                   text "%name parse" <> text name,
                   text "%tokentype { L.PosToken }",
                   text "%monad { Either String }",
                   text "%error { parseError }",
                   nl,
                   text "%token",
                   nl,
                   lexDoc,
                   nl,
                   text "%%",
                   nl,
                   rulesDoc,
                   nl,
                   text (footer ast)
                  ]
    where normal_rules = normalRules srules
          listRuleSet = makeListRuleSet normal_rules
          payloadTokens = makePayloadTokenSet lex_rules
          -- The start rule is wrapped so that the parser consumes the
          -- EndOfFile token emitted by the lexer; this way errors at end of
          -- input are reported by parseError with a real source position
          startWrapper = case normal_rules of
                           (r : _) -> [(text (name ++ "__top") <+> text ":" <+> text (getSRuleName r)
                                        <+> text "rtk__eof" <+> text "{ $1 }") <> text "\n"]
                           []      -> []
          rulesDoc = vcat (startWrapper ++ map (genRule listRuleSet payloadTokens) normal_rules)
          lexDoc = vcat (combineAlt (text "rtk__eof") (text "L.PosToken _ L.EndOfFile")
                         : map genToken (removeSymmacros lex_rules))
          nl = text ""
          header = "{\n\
                   \{-# LANGUAGE DeriveDataTypeable #-}\n\
                   \module " ++ name ++ "Parser where\n\
                   \import qualified Data.Generics as Gen\n\
                   \import qualified " ++ name ++  "Lexer as L (Token(..), PosToken(..), AlexPosn(..), alexScanTokens)\n\
                   \}"
          parseErrorDefs = "parseError :: [L.PosToken] -> Either String a\n\
                           \parseError [] = Left \"Parse error: unexpected end of input\"\n\
                           \parseError (L.PosToken (L.AlexPn _ line col) tok : _) =\n\
                           \    Left $ \"Parse error at line \" ++ show line ++ \", column \" ++ show col ++ \": unexpected \" ++ showRtkToken tok\n"
          showTokenDefs = render $ vcat (text "-- Render a token the way it appears in the source, for error messages"
                                         : text "showRtkToken :: L.Token -> String"
                                         : text "showRtkToken L.EndOfFile = \"end of input\""
                                         : map genShowToken (removeSymmacros lex_rules))
          posDefs = genPosDefs lex_rules
          tkValDefs = render $ vcat (text "-- Recover a token's payload from the whole positioned token: %token"
                                     : text "-- bindings keep the L.PosToken so semantic actions can read its position"
                                     : map genTkVal (removeSymmacros lex_rules))
          footer ast = "{\n" ++ parseErrorDefs ++ "\n" ++ showTokenDefs ++ "\n\n" ++ posDefs
                       ++ "\n" ++ tkValDefs ++ "\n\n" ++ ast ++ "\n}"

type ListRuleSet = Set.Set ID

makeListRuleSet :: [SyntaxRule] -> ListRuleSet
makeListRuleSet lst = Set.fromList $ map getSRuleName $ filter isMany lst
    where isMany (SyntaxRule { getSClause = STMany{} }) = True
          isMany _ = False

-- The tokens that carry a payload (everything except keywords, which are
-- their own value, and ignored tokens, which never reach the parser).
-- References to them in semantic actions go through a tkVal_* extractor.
type PayloadTokenSet = Set.Set ID

makePayloadTokenSet :: [LexicalRule] -> PayloadTokenSet
makePayloadTokenSet lst = Set.fromList [ name | LexicalRule{ getLRuleName = name, getLRuleDataType = dt } <- lst
                                        , not (dt == "Keyword" || dt == "Ignore") ]

-- The RtkPos machinery shared by all generated parsers, plus one dummy
-- RtkPosOf instance per non-String token payload type (String is covered by
-- the Char instance through [a]); a list or optional rule over such a token
-- is a type synonym, and projecting its position goes through the payload.
genPosDefs :: [LexicalRule] -> String
genPosDefs lex_rules =
    [str|-- Source position of a node: every constructor except the Anti_* splice
-- artifacts stores the position of its alternative's first symbol in its
-- first field. Positions are transparent for equality and ordering, so two
-- ASTs that differ only in source positions (e.g. a quasi-quote parsed at
-- compile time vs the same construct parsed at run time) compare equal.
newtype RtkPos = RtkPos L.AlexPosn deriving (Show, Gen.Data, Gen.Typeable)
instance Eq RtkPos where _ == _ = True
instance Ord RtkPos where compare _ _ = EQ

-- The position used where no source token exists: empty productions, empty
-- lists, absent optionals and Anti_* quasi-quote splices
rtkNoPos :: RtkPos
rtkNoPos = RtkPos (L.AlexPn 0 0 0)

class RtkPosOf a where
    rtkPosOf :: a -> RtkPos
instance RtkPosOf L.PosToken where
    rtkPosOf (L.PosToken p _) = RtkPos p
instance RtkPosOf a => RtkPosOf [a] where
    rtkPosOf (x : _) = rtkPosOf x
    rtkPosOf []      = rtkNoPos
instance RtkPosOf a => RtkPosOf (Maybe a) where
    rtkPosOf (Just x) = rtkPosOf x
    rtkPosOf Nothing  = rtkNoPos
-- A Char carries no position; this also covers String token payloads
instance RtkPosOf Char where
    rtkPosOf _ = rtkNoPos
|] ++ payloadInstances
    where payloadInstances =
              concat [ "instance RtkPosOf " ++ dt ++ " where rtkPosOf _ = rtkNoPos\n"
                     | dt <- List.nub [ dt | LexicalRule{ getLRuleDataType = dt } <- lex_rules ]
                     , dt `notElem` ["Keyword", "Ignore", "String", "Char"] ]

genToken :: LexicalRule -> Doc
genToken LexicalRule{ getLRuleName = name, getLRuleDataType = dtn } =
    case dtn of
        "Keyword" -> combineAlt (text name) (text "L.PosToken _ L." <> text (tokenName name))
        "Ignore"  -> empty
        -- payload tokens bind the whole positioned token (not a $$
        -- projection), so semantic actions can take their position; the
        -- payload is recovered with the tkVal_* extractor at the use site
        _         -> combineAlt (text name) (text "L.PosToken _ (L." <> text (tokenName name) <> text " _)")
genToken (MacroRule _ _) = empty

genTkVal :: LexicalRule -> Doc
genTkVal LexicalRule{ getLRuleName = name, getLRuleDataType = dtn } =
    case dtn of
        "Keyword" -> empty
        "Ignore"  -> empty
        _         -> vcat [ text ("tkVal_" ++ name ++ " :: L.PosToken -> " ++ dtn)
                          , text ("tkVal_" ++ name ++ " (L.PosToken _ (L." ++ tokenName name ++ " v)) = v")
                          , text ("tkVal_" ++ name ++ " t = error (\"rtk internal error: token "
                                  ++ name ++ " expected, got \" ++ showRtkToken (L.ptToken t))")
                          ]
genTkVal (MacroRule _ _) = empty

genShowToken :: LexicalRule -> Doc
genShowToken LexicalRule{ getLRuleName = name, getLRuleDataType = dtn, getLClause = cl } =
    case dtn of
        "Ignore"  -> empty
        "Keyword" -> (text "showRtkToken L." <> text (tokenName name)) <+> text "=" <+> text (show (keywordText cl))
        _         -> (text "showRtkToken (L." <> text (tokenName name)) <+> text "v) =" <+> text (show (name ++ " ")) <+> text "++ show v"
    where keywordText (IStrLit s) = "'" ++ s ++ "'"
          keywordText _           = name
genShowToken (MacroRule _ _) = empty

genRule :: ListRuleSet -> PayloadTokenSet -> SyntaxRule -> Doc
-- <Rule>* with separator can only be expressed using two rules in LR grammar
-- Will do it here
genRule lrs toks SyntaxRule{ getSClause = (STMany STStar cl (Just cl1)), getSRuleName = name } =
    let lstName = name ++ "__plus_list_"
    in
      vcat [
            ((text lstName) <+> (text ":") <+> (genTopClause lrs toks lstName
                                               (STMany STPlus cl (Just cl1)))) <> text "\n",
            ((text name) <+> (text ":") <+> (genOptPlusClause lstName)) <> text "\n"
           ]


genRule lrs toks SyntaxRule{ getSClause = cl, getSRuleName = name } =
    ((text name) <+> (text ":") <+> (genTopClause lrs toks name cl)) <> text "\n"

genTopClause :: ListRuleSet -> PayloadTokenSet -> String -> SyntaxTopClause -> Doc

-- Ignore is not expected in the cl
genTopClause _ toks rn (STMany op cl Nothing) = joinAlts [base, step]
    where (baseAlt,alt) = case op of
                            STStar -> (text "[]", emptyAlt)
                            STPlus -> (brackets (clRef 1), clDoc)
          base = combineAlt alt baseAlt
          step = combineAlt (text rn <+> clDoc) (clRef 2 <+> text ": $1")
          clDoc = genSimpleClause cl
          clRef = tokenAwareRef toks cl

genTopClause _ toks rn (STMany STPlus cl (Just cl1)) = joinAlts [base, step]
    where base = combineAlt clDoc (brackets (clRef 1))
          step = combineAlt (text rn <+> genSimpleClause cl1 <+> clDoc) (clRef 3 <+> text ": $1")
          clDoc = genSimpleClause cl
          clRef = tokenAwareRef toks cl

genTopClause _ _ _ (STMany STStar _ (Just _)) = error "STMany with STStar and separator not supported in this position"

genTopClause lrs toks _ (STOpt cl) = joinAlts [present, not_present]
    where present = combineAlt (genSimpleClause cl)
                               (text "Just" <+> constructor_call)
          constructor_call = hsep $ enumClauses lrs toks [cl]
          not_present = combineAlt emptyAlt (text "Nothing")

genTopClause lrs toks _ (STAltOfSeq clauses) = joinAlts $ map (genClauseSeq lrs toks) clauses

genOptPlusClause :: String -> Doc
genOptPlusClause lstName = joinAlts [present, not_present]
    where present = combineAlt (text lstName)
                               (text "$1")
          not_present = combineAlt emptyAlt (text "[]")

genClauseSeq :: ListRuleSet -> PayloadTokenSet -> STSeq -> Doc
genClauseSeq lrs toks (STSeq _ clauses) | isClauseSeqLifted clauses = combineAlt rule production
    where rule = hsep (map genSimpleClause clauses)
          production = hsep $ (enumClauses lrs toks clauses)
genClauseSeq lrs toks (STSeq constructor clauses)  = combineAlt rule production
    where rule = hsep (map genSimpleClause clauses)
          production = hsep $ (text constructor) : posArgs ++ (enumClauses lrs toks clauses)
          -- Anti_* constructors are quasi-quote splice artifacts and carry
          -- no position field (see GenAST.isAntiConstructor)
          posArgs | isAntiConstructor constructor = []
                  | otherwise                     = [posOfFirst lrs clauses]

-- The position of an alternative's first symbol - token or nonterminal,
-- ignored or not (every symbol stays $n-addressable in happy actions). List
-- rules build their lists reversed, so the source-order head is at the end.
-- An empty alternative has no symbols and gets the dummy position.
posOfFirst :: ListRuleSet -> [SyntaxSimpleClause] -> Doc
posOfFirst _ [] = text "rtkNoPos"
posOfFirst lrs (cl : _)
    | Set.member (getSID cl) lrs = text "(rtkPosOf (reverse $1))"
    | otherwise                  = text "(rtkPosOf $1)"

genSimpleClause :: SyntaxSimpleClause -> Doc
-- TODO: check whether reverse is needed (monad again) (switch to left recursion)
genSimpleClause (SSId idName) = text idName
genSimpleClause (SSIgnore idName) = text idName
 -- TODO: no lifted yet, need monad with rules map here
genSimpleClause (SSLifted idName) = text idName

-- A reference to symbol $n of a production: payload tokens go through their
-- tkVal_* extractor, everything else is the plain happy attribute.
tokenAwareRef :: PayloadTokenSet -> SyntaxSimpleClause -> Int -> Doc
tokenAwareRef toks cl n
    | Set.member (getSID cl) toks = parens (text ("tkVal_" ++ getSID cl) <+> (text "$" <> int n))
    | otherwise                   = text "$" <> int n

enumClauses :: ListRuleSet -> PayloadTokenSet -> [SyntaxSimpleClause] -> [Doc]
enumClauses lrs toks cls = f cls 1 []
    where f (ssc:rest) count acc | isNotIgnored ssc = f rest (count + 1) (ref ssc count : acc)
          f (_:       rest) count acc               = f rest (count + 1) acc
          f []              _     acc               = reverse acc
          ref ssc count | Set.member (getSID ssc) lrs = text "(reverse $" <> int count <> text ")"
                        | otherwise                   = tokenAwareRef toks ssc count

getSID :: SyntaxSimpleClause -> ID
getSID (SSId n) = n
getSID (SSLifted n) = n
getSID (SSIgnore n) = n

emptyAlt :: Doc
emptyAlt = text "{- empty -}"

joinAlts :: [Doc] -> Doc
joinAlts alts = vcat $ punctuate (text " |") alts

combineAlt :: Doc -> Doc -> Doc
combineAlt rule production = rule <+> text "{" <+> production <+> text "}"

