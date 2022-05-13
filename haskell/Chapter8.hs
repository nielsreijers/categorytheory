-- Challenge 1
import Data.Bifunctor
import Data.Functor.Identity
import Data.Functor.Const
import System.Win32 (COORD(x))

data Pair a b = Pair a b

instance Bifunctor Pair where
    bimap g h (Pair x y) = Pair (g x) (h y)
    first g (Pair x y) = Pair (g x) y
    second h (Pair x y) = Pair x (h y)

-- Challenge 2
type Maybe' a = Either (Const () a) (Identity a)

f :: Maybe a -> Maybe' a
f Nothing = Left (Const ())
f (Just x) = Right (Identity x)

g :: Maybe' a -> Maybe a
g (Left (Const ())) = Nothing
g (Right (Identity x)) = Just x

-- Challenge 3
data PreList a b = Nil | Cons a b
instance Bifunctor PreList where
    bimap g h Nil = Nil
    bimap g h (Cons x y) = Cons (g x) (h y)

-- Challenge 4
data K2 c a b = K2 c
data Fst a b = Fst a
data Snd a b = Snd b

instance Bifunctor (K2 c) where
    bimap _ _ (K2 x) = K2 x

instance Bifunctor Fst where
    bimap f _ (Fst x) = Fst (f x)

instance Bifunctor Snd where
    bimap _ g (Snd x) = Snd (g x)
