-- STUB MODULE - Replaced by generated version when running 'make generate-bootstrap'
module GrammarLexer (alexScanTokens, Token(..)) where
data Token = EndOfFile deriving (Show, Eq)
alexScanTokens :: String -> [Token]
alexScanTokens _ = error "GrammarLexer stub: Run 'make generate-bootstrap' to generate the actual parser"
