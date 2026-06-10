module TokenProcessing
    ( processTokens
    , unBackQuote
    , catBigstrs
    ) where

import Lexer (Token(..), PosToken(..))

-- | Process tokens after lexical analysis
-- This applies escape sequence handling and concatenates multi-line strings
processTokens :: [PosToken] -> [PosToken]
processTokens = catBigstrs . map processEscapes

-- | Process escape sequences in string and regex tokens
processEscapes :: PosToken -> PosToken
processEscapes (PosToken pos tok) = PosToken pos (processEscapesTok tok)

processEscapesTok :: Token -> Token
processEscapesTok (StrLit s) = StrLit (unBackQuote s)
processEscapesTok (RegExpLit s) = RegExpLit (unBackQuote s)
processEscapesTok tok = tok

-- | Handle backslash escape sequences
-- Preserves \\n, \\t, \\r, \\f, \\v as-is (for grammar rules); this must stay
-- exactly the set that GenX.isAlexEscape emits bare into the generated lexer
-- Removes backslash from other escaped characters
unBackQuote :: String -> String
unBackQuote ('\\':'n':xs) = '\\':'n' : unBackQuote xs
unBackQuote ('\\':'t':xs) = '\\':'t' : unBackQuote xs
unBackQuote ('\\':'r':xs) = '\\':'r' : unBackQuote xs
unBackQuote ('\\':'f':xs) = '\\':'f' : unBackQuote xs
unBackQuote ('\\':'v':xs) = '\\':'v' : unBackQuote xs
unBackQuote ('\\':c:xs) = c : unBackQuote xs
unBackQuote (c:xs) = c : unBackQuote xs
unBackQuote [] = []

-- | Concatenate adjacent BigStr tokens with newlines
-- This handles multi-line triple-quoted strings
-- The merged token keeps the position of the first part
catBigstrs :: [PosToken] -> [PosToken]
catBigstrs (PosToken pos (BigStr s1) : toks) = case catBigstrs toks of
                (PosToken _ (BigStr s2) : toks') -> (PosToken pos (BigStr (s1 ++ ('\n' : s2))) : toks')
                _ -> PosToken pos (BigStr s1) : toks
catBigstrs (tok : toks) = tok : catBigstrs toks
catBigstrs [] = []
