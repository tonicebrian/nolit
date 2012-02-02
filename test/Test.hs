import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import ParserTest

tests = [
    testGroup "cases" $ zipWith (testCase . show) [1::Int ..] $
    [ case_theTest
    ]
    , testGroup "properties" $ zipWith (testProperty . show) [1::Int ..] $
        [
            prop_theProperty
        ]
    ]


-- Main program
main = defaultMain tests 



