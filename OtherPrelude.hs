module OtherPrelude where
import Prelude

-- Склеить два списка за O(length a)
(++) :: [a] -> [a] -> [a]
[] ++ b = b
(x:lx) ++ b = x : (lx ++ b)

-- Список без первого элемента
tail :: [a] -> [a]
tail [] = []
tail (x:lx) = lx

-- Список без последнего элемента
init :: [a] -> [a]
init ([]:[]) = []
init (x:[]) = x
init (x:lx) = init lx

-- Первый элемент
head :: [a] -> a
head [] = error "!!: empty list"
head (x:xs) = x 

-- Последний элемент
last :: [a] -> a
last [] = error "!!: empty list"
last (x:[]) = x
last (x:lx) = last lx

-- n первых элементов списка
take :: Integer -> [a] -> [a]
take n []= []
take n (x:lx)
    | n<=0 = []
    | n>0 = x:(lx (n-1) lx)

-- Список без n первых элементов
drop :: Integer -> [a] -> [a]
drop n [] = []
drop n (x:lx)
    | n>0 = drop (n-1) lx
    | n<=0 = lx

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile pred [] = []
takewhile pred (x:lx)
	| pred x = x: takeWhile pred lx 
	| otherwise = []

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile pred [] = []
dropWhile pred (x:lx)
	| pred x = drop pred lx
	| otherwise = (x:lx)

-- Разбить список в пару (наибольший префикс удовлетворяющий p, всё остальное)
span :: (a -> Bool) -> [a] -> ([a], [a])
span pred l = (takeWhile pred l, dropWhile pred l)

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее

break':: (a->Bool)->[a]->([a], [a])->([a], [a])
break' p [] (la, lb) = (la, lb)
break' p (x:lx) (la, lb)  
	| pred p = (la ++ (x:[]), lb)
	| otherwise = (la, (x:lx) ++ lb)



break :: (a -> Bool) -> [a] -> ([a], [a])
break p l  = break' p l ([], [])

-- n-ый элемент списка (считая с нуля)
(!!) :: [a] -> Integer -> a
[] !! n = error "!!: empty list"
(x:lx)  !! n | n==0 = x
	     | n>0 = lx !! (n-1)

-- Список задом на перёд
reverse :: [a] -> [a]
reverse [] = []
reverse (x:lx) = lx ++ (x:[])

--добавить элемент в начало каждого списка
inHead :: a -> [[a]] -> [[a]]
inHead x [] = []
inHead x (l:ll) = (x:l) : (inHead x ll)

--добавить элемент в конец каждого списка
inTail :: a -> [[a]] -> [[a]] 
inTail x [] = []
inTail x (l:ll) = (l++(x:[])) : (inTail x ll)  


-- (*) Все подсписки данного списка
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:lx) = (subsequences lx) ++ [x:ps| ps<-(subsequences lx)]

-- (*) Все перестановки элементов данного списка
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations l = [x:ps| (lp,x,rp)<-split l, ps<-permutations (lp ++ rp)]
   where split = [(lp,x,rp)|lp<-take (n-1) l, rp<-drop n l, x<-l!!n, n<-[0 .. (length l)-1 ]] 

-- Повторяет элемент бесконечное число раз
repeat :: a -> [a]
repeat a = a:(repeat a)


-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x:lx) = foldl (f z x) lx

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f z [] = [z]
scanl f z (x:lx) = (f z x): (scanl (f z x) lx)

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f z [] = [z]
scanr f z (x:xs) = f x (foldr f z xs):(scanr f z xs)

finiteTimeTest = take 10 $ foldr (:) [] $ repeat 1

map f [] = []
map f (x:xs) = (f x):(map f xs)

-- Склеивает список списков в список
concat :: [[a]] -> [a]
concat (x:xs)= x ++ (concat xs) 

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f []= []
concatMap f (x:xs) = (f x) ++ (f xs) 

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: [a] -> [b] -> [(a, b)]
zip a [] = []
zip [] b = []
zip (a:la) (b:lb) = (a,b):(zip la lb)

-- Аналогично, но плющить при помощи функции, а не конструктором (,)
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] _ = []
zipWith f _ [] = []
zipWith f (a:la) (b:lb) = (f a b):(zipWith f la lb)

-- Интересные классы типов
class Monoid a where
    mzero :: a
    mappend :: a -> a -> a

instance Monoid [a] where
    mzero = []
    mappend = (++)

instance Monoid Integer where
    mzero = 0
    mappend = (+)

data MulInteger = Mult Integer

data MulRational = RMult Rational

instance Monoid MulInteger where
    mzero = 1
    (Mult a) `mappend` (Mult b) = Mult $ a * b

-- Реализуйте инстансы Monoid для Rational и MulRational
instance Monoid Rational where
    mzero = 0.0
    mappend = (+)

instance Monoid MulRational where
    mzero = 1.0
    mappend (RMult a) (RMult b) = RMult (a*b)

-- Фолдабл
class MFoldable t where
    mfold :: Monoid a => t a -> a

-- Смотрите какой чит. Можно построить дерево только из элементов моноида.
data MTree a = Monoid a => MLeaf | MNode a (MTree a) (MTree a)

-- Альтернативный фолдабл
class Monoid a => AMFoldable t a where
    amfold :: t a -> a
-- Изучите раздницу между этими двумя определениями.

-- Выпишите тип этого выражения. Фигурирует ли в нём Monoid? Почему?
mtfold MLeaf = mzero -- А то, что a - моноид нам будет даровано самой природой
mtfold (MNode a l r) = a `mappend` (mtfold l) `mappend` (mtfold r)

-- Напишите терм с типом
-- (...) => MTree a -> x
-- где x -- тип на ваш вкус, (...) - какие-то констреинты (возможно пустые),
-- при этом этот терм внутри должен использовать то, что a -- моноид, но в
-- констреинтах Monoid a быть не должно.
-- Для широты фантазии в терме можно использовать классы типов, определённые в любом
-- месте этого файла.
mterm::Field a => MTree a -> a
mterm MLeaf = mzero
mterm (MNode a l r) = (rinv a) `mappend` (mterm l) `mappend` (mterm r)


-- (**) Разберитесь чем отличаются эти определения.
-- "Скомпилируйте" их в наш гипотетический язык программирования с
-- типом Dict.
instance MFoldable MTree where
    mfold = mtfold

instance Monoid a => AMFoldable MTree a where
    amfold = mtfold

-- Группа
class Monoid a => Group a where
     ginv :: a -> a

instance Group Integer where
    ginv a = -a

instance Group Rational where
    ginv a = -a

instance Group MulRational where
    ginv (RMult a) = RMult (1/a)

class Group a => Ring a where
    rmul :: a -> a -> a -- а это умножение

instance Ring Integer where
    rmul = (*)

instance Ring Rational where
    rmul = (*)

class Ring a => Field a where
    rinv :: a -> a
 
instance Field  Rational where
    rinv a = 1/a   

-- Реализуйте тип для матриц (через списки) и операции над ними
--data Matrix a = 
-- Чем должно быть a? Моноидом? Группой? Ещё чем-нибудь?

--matsum = ?

--matscalarmul = ?

--matmul = ?

-- (**) Реализуйте классы типов для векторных и скалярных полей.
-- Перепишите в этих терминах что-нибудь из написанного выше.
-- Реализуйте оператор дифференцирования, взятия градиента.
--class ? ScalarField ? where
--    ?

--class ? VectorField ? where
--    ?
--