import System.Environment (getArgs)
import Pl0Lexer
import Pl0Parser
import Text.Show.Pretty (ppShow)
import System.Exit (exitFailure, exitSuccess)

data ParseMode = LexOnly | FullParse
    deriving (Eq, Show)

parseArgs :: [String] -> Either String (ParseMode, String)
parseArgs args = case args of
    ["--lex-only", file] -> Right (LexOnly, file)
    [file] -> Right (FullParse, file)
    _ -> Left "Usage: pl0 [--lex-only] <pl0-file>"

-- The generated lexer and parser encode error positions as "LINE:COL:message"
-- (machine-splittable); render them back human-readably for the console
renderError :: String -> String
renderError err =
    case span (/= ':') err of
        (l, ':' : rest1) | [(line, "")] <- (reads l :: [(Int, String)]) ->
            case span (/= ':') rest1 of
                (c, ':' : msg) | [(col, "")] <- (reads c :: [(Int, String)]) ->
                    "line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg
                _ -> err
        _ -> err

-- PL/0 parser driver: the "validator" stage of the tutorial that
-- pl0.pg follows (parts 1-3 of "Let's write a compiler").
-- For quasi-quotation tests, see TestQQ.hs
main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left errMsg -> do
            putStrLn errMsg
            exitFailure
        Right (mode, file) -> do
            content <- readFile file

            case mode of
                LexOnly -> do
                    -- Only perform lexical analysis
                    let tokens = either (errorWithoutStackTrace . renderError) id $
                                   scanTokens content
                    putStrLn "=== Lexical analysis successful! ==="
                    putStrLn $ "Token count: " ++ show (length tokens)
                    exitSuccess

                FullParse -> do
                    -- Perform full parse; lexer and parser report errors as Left
                    let ast = either (errorWithoutStackTrace . renderError) id $
                                scanTokens content >>= parsePl0
                    putStrLn "=== Parsed PL/0 AST ==="
                    putStrLn $ ppShow ast
                    putStrLn "\n=== Parse successful! ==="
                    exitSuccess
