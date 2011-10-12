import Prelude ( Show(..)
               , Bool(..), Int(..), Double(..), (.)
               , (+), (-), (*), (/), mod, (++)
               , (<), (==), (>), (<=), (>=))

data Nat = Zero | 
           Succ Nat           

instance Show Nat where
    show x = (show . toInt) x

toInt:: Nat -> Int
toInt Zero = 0
toInt (Succ(a)) = 1 + (toInt a)

fromInt 0 = Zero
fromInt x = Succ (fromInt (x-1))

plus :: Nat->(Nat->Nat)
plus a Zero = a
plus a (Succ b) = plus (Succ a) b

mul :: Nat->Nat->Nat
mul a Zero = Zero
mul a (Succ b) = plus a (mul a b)

sub :: Nat->Nat->Nat
sub a Zero = a
sub Zero a = Zero
sub (Succ a) (Succ b) = sub a b


div :: Nat->Nat->Nat
div'' :: Nat->Nat->Nat
--div'' a Zero = ?
div'' Zero a = Zero
div'' a b =plus (div'' (sub a b) b) (Succ Zero)
div a b = sub (div'' (Succ a) b) (Succ Zero)

data Integer = Pos Nat|
               Neg Nat

instance Show Integer where
  show a = show (zToInt a)

zToInt :: Integer -> Int
zToInt (Pos a) = toInt a
zToInt (Neg a) =  -(toInt a)-1

zFromInt :: Int -> Integer
zFromInt 0 = Pos Zero
zFromInt a = 
      if a<0
        then Neg (fromInt (-a-1))
        else Pos (fromInt a)

isNull :: Nat->Bool
isNull Zero = True
isNull (Succ a) = False
          	

zplus :: Integer -> Integer -> Integer
zplus (Pos a) (Pos b) = Pos (plus a b)
zplus (Neg a) (Neg b) = Neg (plus (Succ a) (Succ b))
zplus (Pos a) (Neg b) = 
     if (isNull (sub a (Succ b)))
                  then Neg (sub (Succ b) a)
		  else Pos (sub a (Succ b))
zplus (Neg a) (Pos b) = 
     if (isNull (sub b (Succ a)))
                  then Neg (sub b (Succ a))
		  else Neg (sub (Succ a) b)           

zmul :: Integer -> Integer -> Integer
zmul (Pos a) (Pos b) = Pos (mul a b)
zmul (Neg a) (Neg b) = Pos (mul (Succ a) (Succ b))
zmul (Neg a) (Pos b) = Neg (mul (Succ a) b)
zmul (Pos a) (Neg b) = Neg (mul a (Succ b))

zsub :: Integer -> Integer -> Integer
zsub a (Neg b) = zplus a (Pos (Succ b))
zsub a (Pos Zero) = a
zsub a (Pos (Succ x)) = zplus a (Neg x)

zdiv :: Integer -> Integer -> Integer
zdiv (Pos a) (Pos b) = Pos (div a b)
zdiv (Neg a) (Neg b) = Pos (div (Succ a) (Succ b))
zdiv (Neg a) (Pos b) = Neg (div (Succ a) b)
zdiv (Pos a) (Neg b) = Neg (div a (Succ b))

data Rational = Rational Integer Nat

instance Show Rational where
  show (Rational x y)= (show x)++(show (" / "))++(show y) 

rplus :: Rational -> Rational -> Rational
rplus (Rational x y) (Rational u v) = 
      Rational (zplus (zmul (Pos v) x) (zmul (Pos y) u)) (mul y v) 

rmul :: Rational -> Rational -> Rational
rmul (Rational x y) (Rational u v) = Rational (zmul x u) (mul y v)

rsub :: Rational -> Rational -> Rational
rsub (Rational x y) (Rational u v) = 
     Rational (zsub (zmul (Pos v) x) (zmul (Pos y) u)) (mul y v)

rdiv :: Rational -> Rational -> Rational
rdiv (Rational x y) (Rational (Pos u) v) =
     Rational (zmul (Pos v) x) (mul u y)
rdiv (Rational x y) (Rational (Neg u) v) =
     Rational (zmul (Neg v) x) (mul u y)

data Pair a b = Pair a b
    deriving Show

curry :: (Pair a b -> c) -> a -> b -> c
curry f a b = f (Pair a b)

uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f (Pair a b) = f a b

pmap :: (a -> a') -> (b -> b') -> Pair a b -> Pair a' b'
pmap f g (Pair a b) = Pair (f a) (g b)

data List a = Cons a (List a) -- Элемент и хвост
            | Nil             -- Конец списка
    deriving Show

length Nil = 0
length (Cons _ b) = 1 + length b

-- Реализуйте функцию map с типом
map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons a la) = Cons (f a) (map f la)

-- делающую что-то разумное и такую, что length l == length (map f l)

data Tree a = Node a (Tree a) (Tree a) -- Узел
            | Leaf                     -- Лист
    deriving Show

max a b = if a >= b then a else b

height Leaf = 0
height (Node _ a b) = 1 + max (height a) (height b)

tmap :: (a -> b) -> Tree a -> Tree b
tmap f Leaf = Leaf
tmap f (Node a ta_l ta_r) = Node (f a) (tmap f ta_l) (tmap f ta_r)

list2tree :: List a -> Tree a
list2tree Nil= Leaf
list2tree (Cons a la)= Node a Leaf (list2tree la)

-- Конструируем типы.
-- Логическое или

data Maybe a = Just a
             | Nothing
    deriving Show

find :: (a -> Bool) -> List a -> Maybe a
find p Nil = Nothing
find p (Cons a la) = 
     if (p a)
       then Just a
       else find p la

filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons a la) = 
       if (p a)
         then Cons a (filter p la)
	 else filter p la

isJust Nothing  = False
isJust (Just _) = True

maybefilter :: List (Maybe a) -> List a
maybefilter l= map ( \(Just x)->x ) (filter (isJust) l)

gfilter :: (a -> Maybe b) -> List a -> List b
gfilter f Nil = Nil
gfilter f (Cons a l) = 
      if (isJust (f a))
        then  let unpack (Just x) = x
              in Cons (unpack (f a)) (gfilter f l)     
        else gfilter f l

maybefilter' :: List (Maybe a) -> List a
maybefilter' l = gfilter (\a->a) l

data Either a b = Left a
                | Right b
    deriving Show

data Empty = Empty--Пустое множество

maybe2either :: Maybe a -> Either Empty a
maybe2either Nothing = Left Empty
maybe2either (Just a) = Right a

emap :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
emap f g (Left x) = Left (f x)
emap f g (Right y) = Right (g y)