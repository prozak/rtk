-- | Structured, user-facing diagnostics for the grammar-processing pipeline.
--
-- Errors caused by a bad grammar are represented as 'Diagnostic' values that
-- are threaded through 'Either' instead of being thrown with 'error'. The
-- pipeline stages (lexing, parsing, normalization, code generation) return
-- @Either Diagnostic a@; the executable renders the diagnostic in GNU one-line
-- style and exits, so RTK never shows a Haskell exception for a user mistake.
--
-- This module sits below the rest of the project (it imports nothing from it),
-- so 'SourcePos' lives here and both the lexer and the parser can refer to it.
module Diagnostics
    ( SourcePos(..)
    , showSourcePos
    , Diagnostic(..)
    , renderDiagnostic
    , diagnosticFromPositioned
    ) where

import Data.Data (Data, Typeable)

-- | Position in the grammar source file (line and column, both 1-based).
data SourcePos = SourcePos { srcLine :: Int, srcColumn :: Int }
                 deriving (Eq, Ord, Show, Typeable, Data)

showSourcePos :: SourcePos -> String
showSourcePos (SourcePos line col) = "line " ++ show line ++ ", column " ++ show col

-- | A user-facing diagnostic: an optional source position, optional context
-- (such as the rule being processed) and the message itself.
--
-- The 'Data' instance lets @--profile-stages@ deep-force an @Either Diagnostic
-- a@ result like any other pipeline value.
data Diagnostic = Diagnostic
  { diagPos     :: Maybe SourcePos
  , diagContext :: Maybe String    -- ^ e.g. @"in rule 'Foo'"@
  , diagMessage :: String
  } deriving (Eq, Show, Data, Typeable)

-- | Render a diagnostic in GNU one-line style:
--
-- >>> renderDiagnostic "g.pg" (Diagnostic (Just (SourcePos 2 1)) (Just "in rule 'Foo'") "message")
-- "g.pg:2:1: error: in rule 'Foo': message"
--
-- The position is omitted when unknown and the context when absent:
--
-- >>> renderDiagnostic "g.pg" (Diagnostic Nothing Nothing "message")
-- "g.pg: error: message"
renderDiagnostic :: FilePath -> Diagnostic -> String
renderDiagnostic file (Diagnostic mpos mctx msg) =
    file ++ posPart ++ ": error: " ++ ctxPart ++ msg
  where
    posPart = case mpos of
                Just (SourcePos line col) -> ":" ++ show line ++ ":" ++ show col
                Nothing                   -> ""
    ctxPart = case mctx of
                Just ctx -> ctx ++ ": "
                Nothing  -> ""

-- | Recover a structured diagnostic from an error message that encodes its
-- position as @\"LINE:COL:message\"@ — the machine-splittable encoding shared
-- by the hand-written lexer and by every generated lexer and parser. Falls
-- back to a position-less diagnostic when the encoding is absent (e.g. an
-- internal alex error).
diagnosticFromPositioned :: String -> Diagnostic
diagnosticFromPositioned err =
    case span (/= ':') err of
        (l, ':' : rest1) | [(line, "")] <- reads l ->
            case span (/= ':') rest1 of
                (c, ':' : msg) | [(col, "")] <- reads c ->
                    Diagnostic (Just (SourcePos line col)) Nothing msg
                _ -> noPos
        _ -> noPos
  where noPos = Diagnostic Nothing Nothing err
