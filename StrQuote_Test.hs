{-# LANGUAGE QuasiQuotes #-}
module StrQuote_Test
where

import StrQuote
import Test.HUnit

testSimple = TestCase $ assertEqual "This expr should return simple string" [str|simple string|] "simple string"
testNewLine = TestCase $ assertEqual "This expr should return simple string with new line" [str|simple string
with new line|] "simple string\r\nwith new line" --TODO: make it platform independent
testEmpty = TestCase $ assertEqual "This expr should return empty string" [str||] ""

testEmptyVar = TestCase $ assertEqual "This expr should return error" [str|?|] "<empty var name>"
testEmptyVarExpr = TestCase $ assertEqual "This expr should return error" [str|?()|] "<empty expr>"

main = runTestTT $ TestList [ testSimple, testNewLine, testEmpty, testEmptyVar, testEmptyVarExpr ]
