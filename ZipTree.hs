import Prelude

data Tree a = Node a (Tree a) (Tree a) | Leaf

data Direct = L | R

data ZipTree a = ZipTree { dir :: [Direct]
                         , value :: [a]
			 , subTree :: [Tree a]
                         , finalTree :: Tree a
                         }

toLeft :: ZipTree a -> ZipTree a
toLeft z = case (finalTree z) of
     Node a l r -> ZipTree { dir = L : (dir z)
			   , value = a : (value z)
			   , subTree = r : (subTree z)
			   , finalTree = l
			   }
     Leaf -> z

toRight :: ZipTree a -> ZipTree a
toRight z = case (finalTree z) of
     Node a l r -> ZipTree { dir = R : (dir z)
			   , value = a : (value z)
			   , subTree = l : (subTree z)
			   , finalTree = r
			   }
     Leaf -> z

toParent :: ZipTree a -> ZipTree a
toParent z = case (dir z) of
     [] -> z
     d:ld -> ZipTree { dir = ld
		     , value = tail $ value z
		     , subTree = tail $ subTree z
		     , finalTree = case d of
				L -> Node (head $ value z) (finalTree z) (head $ subTree z)
				R -> Node (head $ value z) (head $ subTree z) (finalTree z)
		     }

initZip :: Tree a -> ZipTree a
initZip tree = ZipTree{ dir = []
		      , value = []
		      , subTree = []
		      , finalTree = tree
		      }
