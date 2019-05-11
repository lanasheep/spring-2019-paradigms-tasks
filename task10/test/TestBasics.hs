import Test.Tasty
import Test.Tasty.HUnit

import Basics

main :: IO ()
main = defaultMain testsBasics

testsBasics :: TestTree
testsBasics = testGroup "Unit tests for Basics tasks"
    [testCase "head' works on non-empty list" $
        head' [1,2,3] @?= 1

    , testCase "head' works on infinite list too" $
        head' [1..] @?= 1


    , testCase "tail' works on empty list" $
        tail' ([] :: [Integer]) @?= []

    , testCase "tail' works on non-empty list too" $
        tail' [1,2,3] @?= [2,3]

    , testCase "tail' works on infinite list too" $
        take 30 (tail' [1..]) @?= take 30 [2..]


    , testCase "take' takes 1 element from 3-element list" $
        take' 1 [1,2,3] @?= [1]

    , testCase "take' takes 0 element from 3-element list" $
        take' 0 [1,2,3] @?= []

    , testCase "take' takes 3 element from infinite list" $
        take' 3 [1..] @?= [1,2,3]


    , testCase "drop' drops 1 element from 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]

    , testCase "drop' drops 5 element from 3-element list" $
        drop' 5 [1,2,3] @?= []

    , testCase "drop' drops 5 element from infinite list" $
        take 30 (drop' 5 [1..]) @?= take 30 [6..]


    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [0,2..10]

    , testCase "filter' selects only odd numbers from 0 to 10" $
        filter' odd [0..10] @?= [1,3..9]

    , testCase "filter' selects only odd numbers from 0 to infinity" $
        take 30 (filter' odd [0..]) @?= take 30 [1,3..]


    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1,2,3] @?= 6

    , testCase "foldl'' can be used for finding multiplication of elements" $
        foldl'' (*) 1 [1,2,3] @?= 6

    , testCase "foldl'' can be used for finding exponentiation of element" $
        foldl'' (**) 2 [3,2,1] @?= 64


    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]

    , testCase "concat' works on empty and non-empty finite lists" $
        concat' [] [2,2,2] @?= [2,2,2]

    , testCase "concat' works on finite and infinite lists" $
        take 30 (concat' [1,2,3] [4..]) @?= take 30 [1..]


    , testCase "quickSort actualy sorts the sorted descending list" $
        quickSort' [5,2,3,4,1] @?= [1..5]

    , testCase "quickSort actualy sorts the list with duplicates" $
        quickSort' [2,7,1,9,4,2,7] @?= [1,2,2,4,7,7,9]

    , testCase "quickSort actualy sorts the sorted descending list with duplicates" $
        quickSort' [3,3,2,2,1] @?= [1,2,2,3,3]
    ]
