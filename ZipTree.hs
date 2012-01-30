import Prelude

data Tree a = Node a (Tree a) (Tree a) | Leaf

data Direct = L | R

data ZipTree a = ZipTree ([Direct], [a], [Tree a], Tree a)

toLeft :: ZipTree a -> ZipTree a
toLeft (ZipTree (d, lv, lt, Node a b c)) = ZipTree (L:d, a:lv,  c:lt, b)
toLeft x = x

toRight :: ZipTree a -> ZipTree a
toRight (ZipTree (d, lv, lt, Node a b c)) = ZipTree (R:d, a:lv,  b:lt, c)
toRight x = x

toTop :: ZipTree a -> ZipTree a
toTop (ZipTree (d:[], v:[],  t:[], x)) = ZipTree (d:[], v:[],  t:[], x)
toTop (ZipTree (d:ld, v:lv,  t:lt, x)) = case d of
                        L -> ZipTree (ld, lv, lt, Node v x t)
                        R -> ZipTree (ld, lv, lt, Node v t x)    
