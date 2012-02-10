import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit
import Test.QuickCheck
import Control.Monad

import NoLit

prop_Test = property True

tests = [
    testGroup "cases" $ zipWith (testCase . show) [1::Int ..] $
    [ 
        case_simpleTangle
    ,   case_matchFileName
    ]
    , testGroup "properties" $ zipWith (testProperty . show) [1::Int ..] $
        [
        prop_Test
        ]
    ]

-- Unit tests
case_simpleTangle :: Assertion 
case_simpleTangle = do
    content <- readFile "test/resources/test.acd"
    let obtained = tangle content
        expected = [TangledFile "test.cpp" "int main(){\n}\n"]
    unless (obtained == expected) (assertFailure "Tangling not working")

case_matchFileName = assertEqual "File name not extracted" 
                        (Just "test.cpp") (getHeader $ matchHeader "<test.cpp>=")



-- Main program
main = defaultMain tests 



