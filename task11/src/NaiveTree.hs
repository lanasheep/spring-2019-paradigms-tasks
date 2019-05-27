{-|
  Реализация класса типов 'Map' в виде дерева поиска,
  необязательно сбалансированного, работает за линейное
  время в худшем случае.
-}
module NaiveTree where
import Map

{-|
  Двоичное дерево поиска, необязательно сбалансированное.

  Инвариант: для любой вершины @v@:

  1. Все ключи в левом поддереве строго меньше ключа @v@.
  2. Все ключи в правом поддереве строго больше ключа @v@.
-}
data NaiveTree k a =
    -- |Пустое дерево
    Nil
    -- |@Node k a l r@ – дерево с корнем в вершине с ключом @k@,
    -- значением @a@, левым ребёнком @l@ и правым ребёнком @r@.
  | Node k a (NaiveTree k a) (NaiveTree k a)
  deriving (Show, Eq)

{-|
  @merge l r@ объединяет два дерева в одно при условии,
  что все ключи из @l@ строго меньше ключей из @r@.
-}
merge :: NaiveTree k a -> NaiveTree k a -> NaiveTree k a
merge left Nil                     = left
merge left (Node k a left' right') = Node k a (merge left left') right'

{-|
  Реализация функций 'Map' для 'NaiveTree'.

  'empty', 'singleton' и 'Map.null' работают за /O(1)/.
  Если /n/ – количество вершин дерева, а /h/ – высота дерева,
  то 'fromList' работает за /O(nh)/, 'toAscList' работает за /O(n^2)/,
  а 'size' работает за /O(n)/.
  Остальные функции работают за /O(h)/,
  причём каждая функция должна спускаться вниз по дереву и
  подниматься обратно не больше одного раза.

  Скорее всего, при реализации вам потребуется функция 'merge'.
-}
instance Map NaiveTree where
    empty = Nil

    singleton k a = Node k a Nil Nil

    toAscList Nil                   = []
    toAscList (Node k a left right) = (toAscList left) ++ [(k, a)] ++ (toAscList right)

    alter f k Nil = maybe empty (singleton k) (f Nothing)
    alter f k (Node k' a left right) 
      | k < k'    = Node k' a (alter f k left) right
      | k > k'    = Node k' a left (alter f k right)
      | otherwise = case f (Just a) of
        Nothing   -> merge left right
        Just a'   -> merge (merge left (Node k a' Nil Nil)) right

    lookup _ Nil  = Nothing
    lookup k (Node k' a left right) 
      | k < k'    = Map.lookup k left
      | k > k'    = Map.lookup k right
      | otherwise = Just a

    null Nil = True
    null _   = False

    size Nil                   = 0
    size (Node _ _ left right) = size left + 1 + size right
