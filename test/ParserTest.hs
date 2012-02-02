module ParserTest where

import Test.QuickCheck
import Test.HUnit

import NoLit.Parser


--------------------------------------------------    
emptyCodeBlock = "----\n----"
case_parseEmptyCodeBlock = assertEqual "Incorrectly parsed empty block" expected obtained
    where
        expected = parse emptyCodeBlock
        obtained = False

--------------------------------------------------    
prop_theProperty = property True

