module TestMain where

import Test.HUnit
import TestFormProp
import TestFormNorm

main :: IO ()
main = runTestTTAndExit $ TestList $ testsFormProp ++ testsFormNorm
