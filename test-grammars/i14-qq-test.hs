{-# LANGUAGE QuasiQuotes #-}

-- Quasi-quotation against covered pure types (issue #14): in i14.pg the
-- types 'Shape' and 'Label' exist only through rule annotations, and rtk
-- synthesizes their cover rules (Shape = ,Circle | ,Square). The covered
-- group is an ordinary data group, so it gets a top-level quoter and
-- $Type:var splices exactly like a hand-written type-named rule.

import qualified I14QQ as I
import Text.Show.Pretty (ppShow)

main :: IO ()
main = do
    putStrLn "Issue #14 quasi-quotation tests (covered pure types)"
    putStrLn ""

    -- Top-level quoters for the covered types
    let c = [I.shape| circle 42 |]
        s = [I.shape| square 7 |]
    putStrLn "✅ [shape| ... |] quoter works for the covered type"

    let _ = [I.label| note 1 |]
    putStrLn "✅ [label| ... |] quoter works for a type only the start wrapper demands"

    -- The original issue #14 shape: a list over the bare type name
    let xs = [I.items| circle 1, square 2, circle 3 |]
    case xs of
        [_, q, _] | q == [I.shape| square 2 |] ->
            putStrLn "✅ [items| ... |] list over the bare type parses to plain Shape nodes"
        other -> error $ "FAILED: unexpected list AST:\n" ++ ppShow other

    -- Expression splices: the splice token reduces at the attach point
    -- (Circle) and climbs to Shape through the synthesized cover
    let spliced = [I.start| $Shape:c square 1, square 2 |]
    putStrLn "✅ $Shape:var splices in a Shape position"

    let inList = [I.items| $Shape:c, $Shape:s |]
    if inList == [c, s]
        then putStrLn "✅ $Shape:var splices as list elements"
        else error $ "FAILED: list element splices did not round-trip:\n" ++ ppShow inList

    -- Pattern splice: extract back exactly what was spliced in
    case spliced of
        [I.start| $Shape:got square 1, square 2 |]
            | got == c -> putStrLn "✅ pattern splice extracts the covered-type value"
        other -> error $ "FAILED: pattern splice did not round-trip:\n" ++ ppShow other

    putStrLn ""
    putStrLn "All issue #14 QQ tests passed."
