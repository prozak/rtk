module TokenProcessing
    ( processTokens
    , unBackQuote
    , catBigstrs
    ) where

import Lexer (Token(..))

-- | Process tokens after lexical analysis
-- This applies escape sequence handling and concatenates multi-line strings
processTokens :: [Token] -> [Token]
processTokens = catBigstrs . map processEscapes

-- | Process escape sequences in string and regex tokens
processEscapes :: Token -> Token
processEscapes (StrLit s) = StrLit (unBackQuote s)
processEscapes (RegExpLit s) = RegExpLit (unBackQuote s)
processEscapes tok = tok

-- | Handle backslash escape sequences
-- Preserves \\n, \\t, \\r as-is (for grammar rules)
-- Removes backslash from other escaped characters
unBackQuote :: String -> String
unBackQuote ('\\':'n':xs) = '\\':'n' : unBackQuote xs
unBackQuote ('\\':'t':xs) = '\\':'t' : unBackQuote xs
unBackQuote ('\\':'r':xs) = '\\':'r' : unBackQuote xs
unBackQuote ('\\':c:xs) = c : unBackQuote xs
unBackQuote (c:xs) = c : unBackQuote xs
unBackQuote [] = []

-- | Concatenate adjacent BigStr tokens with newlines
-- This handles multi-line triple-quoted strings
catBigstrs :: [Token] -> [Token]
catBigstrs (BigStr s1 : toks) = case catBigstrs toks of
                (BigStr s2 : toks') -> (BigStr (s1 ++ ('\n' : s2)) : toks')
                _ -> BigStr s1 : toks
catBigstrs (tok : toks) = tok : catBigstrs toks
catBigstrs [] = []
