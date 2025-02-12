module Main where

import Text.Regex.PCRE
import Test.HUnit
import qualified Data.ByteString.UTF8 as BSU
import qualified System.Exit as Exit

test_tester :: Test
test_tester = TestCase $ assertBool "always True" $ (1 :: Int) == 1

test_version :: Test
test_version = TestCase $ assertBool "should be a non-empty string"
  $ maybe False ((>4) . length) getVersion

test_string :: Test
test_string = TestCase $ assertBool "should match successfully"
  $ "abcdef" =~ "[abc]+[def]{3}"

test_bytestring :: Test
test_bytestring = TestCase $ assertBool "should match successfully"
  $ (BSU.fromString "abbabbbbabbabc") =~ (BSU.fromString "^(abba|bbbb|bc)*$")

emailRegex :: String
emailRegex = "<([^>@]*)@([a-zA-Z.-]*)>"

test_capture :: Test
test_capture = TestCase $ assertEqual "should capture correctly"
  ("abc","<test@example.com>","xyz",["test","example.com"])
  $ "abc<test@example.com>xyz" =~ emailRegex

tests :: Test
tests = TestList [ TestLabel "test_tester"           test_tester
                 , TestLabel "test_version"          test_version
                 , TestLabel "test_string"           test_string
                 , TestLabel "test_bytestring"       test_bytestring
                 , TestLabel "test_capture"          test_capture ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
