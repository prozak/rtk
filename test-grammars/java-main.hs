import System.IO(readFile, writeFile)
import System.Environment(getArgs)
import JavaLexer
import JavaParser
import Text.Show.Pretty
import System.Exit(exitFailure, exitSuccess)

data ParseMode = LexOnly | DumpTokens | FullParse
    deriving (Eq, Show)

parseArgs :: [String] -> Either String (ParseMode, String)
parseArgs args = case args of
    ["--lex-only", file] -> Right (LexOnly, file)
    ["--dump-tokens", file] -> Right (DumpTokens, file)
    [file] -> Right (FullParse, file)
    _ -> Left "Usage: java-main [--lex-only|--dump-tokens] <java-file>"

-- Simple Java file parser driver
-- For quasi-quotation tests, see java-qq-test.hs
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
                    let tokens = alexScanTokens content
                    putStrLn "=== Lexical analysis successful! ==="
                    putStrLn $ "Token count: " ++ show (length tokens)
                    exitSuccess

                DumpTokens -> do
                    -- Print one token per line so the exact token stream can
                    -- be compared against a golden file (catches
                    -- mis-tokenization that --lex-only cannot)
                    let tokens = alexScanTokens content
                    mapM_ (putStrLn . show . ptToken) tokens
                    exitSuccess

                FullParse -> do
                    -- Perform full parse; lexer and parser report errors as Left
                    let ast = either errorWithoutStackTrace id $
                                scanTokens content >>= parseJava
                    putStrLn "=== Parsed Java AST ==="
                    putStrLn $ ppShow ast
                    putStrLn "\n=== Parse successful! ==="
                    exitSuccess
