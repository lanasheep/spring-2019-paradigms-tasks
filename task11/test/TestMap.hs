{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.

  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testCase "toAscList . fromList sorts list" $
            let map = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
            toAscList map @?= [(1, "x"), (2, "a"), (3, "c")]
        ,

        testGroup "insert tests" [
            testCase "empty list" $
                let map = empty :: m Int String in
                toAscList (insert 3 "a" map) @?= [(3, "a")]
            ,
            testCase "existing key" $
                let map = singleton 3 "a" :: m Int String in
                toAscList (insert 3 "a" map) @?= [(3, "a")]
            ,
            testCase "non-existent key" $
                let map = fromList [(1, "c"), (9, "z")] :: m Int String in
                toAscList (insert 3 "a" map) @?= [(1, "c"), (3, "a"), (9, "z")]
        ],

        testGroup "insertWith tests" [
            testCase "non-existent key" $
                let map = fromList [(2, "b"), (3, "c")] :: m Int String in
                toAscList (insertWith (++) 1 "a" map) @?= [(1, "a"), (2, "b"), (3, "c")]
            ,
            testCase "existing key with ++" $
                let map = fromList [(1, "old"), (3, "z")] :: m Int String in
                toAscList (insertWith (++) 1 "new" map) @?= [(1, "newold"), (3, "z")]
            ,
            testCase "existing key with max" $
                let map = fromList [(1, 100), (3, 300)] :: m Int Int in
                toAscList (insertWith (max) 1 0 map) @?= [(1, 100), (3, 300)]
        ],

        let f key new old = (show key) ++ ":" ++ new ++ "|" ++ old in
        testGroup "insertWithKey tests" [
            testCase "non-existent key" $
                let map = fromList [(2, "b"), (3, "c")] :: m Int String in
                toAscList (insertWithKey f 1 "a" map) @?= [(1, "a"), (2, "b"), (3, "c")]
            ,
            testCase "existing key" $
                let map = fromList [(1, "b"), (3, "c")] :: m Int String in
                toAscList (insertWithKey f 1 "a" map) @?= [(1, "1:a|b"), (3, "c")]
        ],

        let map = fromList [(1, 10), (2, 20)] :: m Int Int in
        testGroup "delete tests" [
            testCase "existing key" $
                toAscList (delete 1 map) @?= [(2, 20)]
            ,
            testCase "non-existent key" $
                toAscList (delete 3 map) @?= [(1, 10), (2, 20)]
        ],

        let map = fromList [(1, 10), (2, 20)] :: m Int Int in
        testGroup "adjust tests" [
            testCase "existing key" $
                toAscList (adjust (*7) 1 map) @?= [(1, 70), (2, 20)]
            ,
            testCase "non-existent key" $
                toAscList (adjust (*7) 3 map) @?= [(1, 10), (2, 20)]
        ],

        let f key val = (show key) ++ " new " ++ val in
        let map = fromList [(1, "a"), (2, "b")] :: m Int String in
        testGroup "adjustWithKey tests" [
            testCase "existing key" $
                toAscList (adjustWithKey f 1 map) @?= [(1, "1 new a"), (2, "b")]
            ,
            testCase "non-existent key" $
                toAscList (adjustWithKey f 3 map) @?= [(1, "a"), (2, "b")]
        ],

        let f val = if val == "a" then Just "new a" else Nothing in
        testGroup "update tests" [
            testCase "change element" $
                let map = fromList [(1, "a"), (3, "c")] :: m Int String in
                toAscList (update f 1 map) @?= [(1, "new a"), (3, "c")]
            ,
            testCase "delete element" $
                let map = fromList [(1, "b"), (3, "c")] :: m Int String in
                toAscList (update f 1 map) @?= [(3, "c")]
            ,
            testCase "non-existent key" $
                let map = fromList [(2, "c"), (9, "z")] :: m Int String in
                toAscList (update f 1 map) @?= [(2, "c"), (9, "z")]
        ],

        let f key val = if val == "a" then Just ((show key) ++ " new a") else Nothing in
        testGroup "updateWithKey tests" [
            testCase "change element" $
                let map = fromList [(1, "a"), (3, "c")] :: m Int String in
                toAscList (updateWithKey f 1 map) @?= [(1, "1 new a"), (3, "c")]
            ,
            testCase "delete element" $
                let map = fromList [(1, "b"), (3, "c")] :: m Int String in
                toAscList (updateWithKey f 1 map) @?= [(3, "c")]
            ,
            testCase "non-existent key" $
                let map = fromList [(2, "c"), (9, "z")] :: m Int String in
                toAscList (updateWithKey f 1 map) @?= [(2, "c"), (9, "z")]
        ],

        let map = fromList [(1, 10), (2, 20)] :: m Int Int in
        testGroup "member tests" [
            testCase "existing key" $
                member 1 map @?= True
            ,
            testCase "non-existent key" $
                member 3 map @?= False
        ],

        let map = fromList [(1, 10), (2, 20)] :: m Int Int in
        testGroup "notMember tests" [
            testCase "non-existent key" $
                notMember 3 map @?= True
            ,
            testCase "existing key" $
                notMember 1 map @?= False
        ],

        testGroup "null tests" [
            testCase "empty list" $
                let map = empty :: m Int Int in
                Map.null map @?= True
            ,
            testCase "non-empty list" $
                let map = singleton 3 "a" :: m Int String in
                Map.null map @?= False
        ]
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 2 "b" (Node 1 "a" Nil Nil) Nil
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList),
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree
    ]
