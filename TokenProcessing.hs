module TokenProcessing
    ( processTokens
    , unBackQuote
    , catBigstrs
    ) where

import Lexer (Token(..), PosToken(..))

-- | Process tokens after lexical analysis: concatenate adjacent multi-line
-- string blocks. Escape handling no longer happens here: both lexers keep
-- the raw token text, and the single 'unBackQuote' pass runs on the parsed
-- AST (see @Frontend.cleanGrammarTokens@) so the cleanup logic is shared by
-- both front ends.
processTokens :: [PosToken] -> [PosToken]
processTokens = catBigstrs

-- | Handle backslash escape sequences
-- Preserves \\n, \\t, \\r, \\f, \\v as-is (for grammar rules); this must stay
-- exactly the set that GenX.isAlexEscape emits bare into the generated lexer
-- Removes backslash from other escaped characters.
-- Applied to string and regex literal CONTENT by Frontend.cleanGrammarTokens,
-- after parsing, for both front ends.
unBackQuote :: String -> String
unBackQuote ('\\':'n':xs) = '\\':'n' : unBackQuote xs
unBackQuote ('\\':'t':xs) = '\\':'t' : unBackQuote xs
unBackQuote ('\\':'r':xs) = '\\':'r' : unBackQuote xs
unBackQuote ('\\':'f':xs) = '\\':'f' : unBackQuote xs
unBackQuote ('\\':'v':xs) = '\\':'v' : unBackQuote xs
unBackQuote ('\\':c:xs) = c : unBackQuote xs
unBackQuote (c:xs) = c : unBackQuote xs
unBackQuote [] = []

-- | Concatenate adjacent BigStr tokens with newlines.
-- This handles multi-line triple-quoted strings. Tokens carry the raw text
-- (including the @"""@ delimiters), so the merge drops the closing delimiter
-- of the first block and the opening delimiter of the second; the merged
-- token is again a well-delimited block. It keeps the position of the first
-- part.
catBigstrs :: [PosToken] -> [PosToken]
catBigstrs (PosToken pos (BigStr s1) : toks) = case catBigstrs toks of
                (PosToken _ (BigStr s2) : toks') -> (PosToken pos (BigStr (mergeBigstrs s1 s2)) : toks')
                _ -> PosToken pos (BigStr s1) : toks
catBigstrs (tok : toks) = tok : catBigstrs toks
catBigstrs [] = []

mergeBigstrs :: String -> String -> String
mergeBigstrs s1 s2 = take (length s1 - 3) s1 ++ ('\n' : drop 3 s2)
