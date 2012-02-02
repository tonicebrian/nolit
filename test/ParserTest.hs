module ParserTest where

import Test.QuickCheck
import Test.HUnit

--------------------------------------------------    
case_theTest = assertEqual "message" expected obtained
    where
        expected = True
        obtained = False

--------------------------------------------------    
prop_theProperty = property True

