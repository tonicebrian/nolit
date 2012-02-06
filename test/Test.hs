import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit
import Test.QuickCheck

case_Test = assertEqual "" True False 
prop_Test = property True

tests = [
    testGroup "cases" $ zipWith (testCase . show) [1::Int ..] $
    [ 
        case_Test
    ]
    , testGroup "properties" $ zipWith (testProperty . show) [1::Int ..] $
        [
        prop_Test
        ]
    ]


-- Main program
main = defaultMain tests 



