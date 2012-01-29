module Lambda where

import Prelude hiding (iterate, (++), elem)

type Varible = String

-- Лямбда-терм
data Term = Var Varible
          | Abs Varible Term
          | App Term Term
          | EmptyTerm
          deriving (Show)

-- Тип [ a ] == Типу List a
-- значение [] == значению Nil
-- значение [ a ] == значению Cons a Nil
-- конструктор (:) == конструктору Cons

-- Конкатенация двух списков
(++) :: [a] -> [a] -> [a]
[] ++ b = b
(a:as) ++ b = a:(as ++ b)

-- Свободные переменные терма
free (Var v) = [v]
free (Abs v t) = filter (/= v) . free $ t -- /= это <<не равно>>
free (App t t') = (free t) ++ (free t')
free (EmptyTerm) = []

-- Заменить все вхождения переменной var на what в терме term
subst var what term = case term of
    Var v    -> if v == var then what else term
    Abs v t  -> if v == var then term else Abs v (subst var what t)
    App t t' -> App (subst var what t) (subst var what t')
    EmptyTerm -> EmptyTerm

-- Содержит ли список элемент?
elem a [] = False
elem a (l:ls) = if a == l then True else elem a ls

-- Любопытная функция
iterate f x = (:) x $ iterate f (f x)

-- Генерирует список имён, производных от v, не входящих в fv
newname fv v = head . filter (\x -> not . elem x $ fv) . iterate ('_':) $ v

-- Обычная бета-редукция, хендлящая переименования переменных
betaReduct :: Varible -> Term -> Term -> Term
betaReduct var what (Abs v t) =
    if (v == var)
      then (Abs v t)
      else Abs name (betaReduct var what (subst v (Var name) t))
          where name = newname ((free t) ++ (free what)) v 
betaReduct var what (Var v) = subst var what (Var v)
betaReduct var what (App t t') = App (betaReduct var what t) (betaReduct var what t')   
betaReduct var what EmptyTerm = EmptyTerm

-- Нормализация нормальным порядком терма term
normal' :: Term -> Term
normal' EmptyTerm = EmptyTerm
normal' t = if (snd (oneStepN t))
	then t
        else normal' (fst (oneStepN t))

oneStepN :: Term -> (Term, Bool)
oneStepN t =  case t of
	Var v -> (t,True)
	Abs v t -> (Abs v (fst (oneStepN t)),snd (oneStepN t))
	App (Abs v t) t' -> (betaReduct v t t', False)
	App t t' -> if snd (oneStepN t)
              then if snd (oneStepN t')
		   then (t, True)
		   else (App t (fst (oneStepN t')), False) 
	      else (App (fst (oneStepN t)) t',False) 
        EmptyTerm -> (EmptyTerm, True)  

-- Нормализация аппликативным порядком терма term
applicative' :: Term -> Term
applicative' t = if (snd (oneStepA t))
		then t
		else applicative' (fst (oneStepA t))

oneStepA :: Term -> (Term, Bool)
oneStepA t =  case t of
	Var v -> (t,True)
	Abs v t -> (Abs v (fst (oneStepA t)),snd (oneStepA t))
	App (Abs v t) t' -> (betaReduct v t t', False)
	App t t' -> if snd (oneStepA t)
              then if snd (oneStepA t')
		   then (t, True)
		   else (App (fst (oneStepA t)) t', False) 
	      else (App  t (fst (oneStepA t')),False)

-- Маркер конца ресурсов
data TooLoong = TooLoong deriving Show

-- (*) Нормализация нормальным порядком терма term за неболее чем n шагов.
-- Результат: Или числа итераций недостаточно, чтобы достичь нормальной
-- формы. Или (число нерастраченных итераций, терм в нормальной форме).
-- 
normal :: Int -> Term -> Either TooLoong (Int, Term)
normal n t = if (n<0) 
		then Left TooLoong
		else if (snd (oneStepN t))
			then Right (n, t)
			else normal (n-1) (fst (oneStepN t))  

-- (*) Аналогичная нормализация аппликативным порядком.
applicative :: Int -> Term -> Either TooLoong (Int, Term)
applicative n t = if (n<0) 
		then Left TooLoong
		else if (snd (oneStepA t))
			then Right (n, t)
			else applicative (n-1) (fst (oneStepA t))



-- (***) Придумайте и реализуйте обобщённую функцию, выражающую некоторое
-- семейство стратегий редуцирования. В том смысле, что номальная, нормальная
-- головная, нормальная слабо-головная и аппликативная стратегии
-- при помощи этой функции будут выражаться некоторым элементарным образом.
-- Аргумент n можно отбросить, а можно оставить.
--
-- strategy = ?
--
-- normal = strategy ?
-- hnf = strategy ?
-- whnf = strategy ?
-- applicative = strategy ?
--
-- Какие ещё стратегии редуцирования вы знаете? Можно ли их выразить
-- при помощи этой стратегии? Если да, то как?
-- Если нет, то можно ли реализовать аналогичную функцию для _всех_
-- возможных стратегий редуцирования, а не только для такого семейства?
-- Если да, то как? Если нет, то почему?

--------------------------------------------------------

-- Область тестирования

loop' = Abs "x" $ App (Var "x") (Var "x")
loop = App loop' loop'

u = Abs "a" $ Abs "b" $ App (Var "a") $ App (Var "b") (Var "_b")
v = Abs "a" $ Abs "b" $ App (App (Var "a") (Var "b")) (Var "_b")
w = Abs "a" $ Abs "b" $ Abs "c" $ Abs "d" $ App (App (Var "a") (Var "b")) (App (Var "c") (Var "d"))

main = test 100
    [ ("no", normal)
    , ("ap", applicative) ]
    [ Var "a"
    , u
    , v
    , loop'
    , u `App` Var "a"
    , v `App` Var "a"
    , u `App` Var "b"
    , v `App` Var "b"
    , u `App` Var "_b"
    , v `App` Var "_b"
    , (u `App` Var "_b") `App` Var "_b"
    , (v `App` Var "_b") `App` Var "_b"
    , w
    , w `App` (Abs "a" (Var "a") `App` (Abs "b" $ Var "b"))
    , (w `App` Abs "a" (Var "b")) `App` loop
    , loop
    ]

-- Если вы не понимаете как это работает, то пока и не надо
pall n term  = mapM_ (\(desc, reduce) -> putStr (desc ++ ": ") >> print (reduce n term))

test :: Show a => Int -> [(String, Int -> Term -> a)] -> [Term] -> IO ()
test n funcs = mapM_ (\term -> print term >> pall n term funcs)
