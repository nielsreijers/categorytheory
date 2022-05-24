{-# LANGUAGE TypeFamilies #-}

class Representable f where
    type Rep f :: *
    tabulate :: (Rep f -> x) -> f x
    index    :: f x -> Rep f -> x

data Stream x = Cons x (Stream x)
instance Representable Stream where
    type Rep Stream = Integer
    tabulate f = Cons (f 0) (tabulate (f . (+1)))
    index (Cons b bs) = \n -> if n == 0 then b else index bs (n - 1)

memoized :: Stream Integer
memoized = tabulate (\x -> x * x)

data Pair a = Pair a a deriving Show
instance Representable Pair where
    type Rep Pair = Bool
    tabulate f = Pair (f True) (f False)
    index (Pair x y) True  = x
    index (Pair x y) False = y

p :: Pair Int
p = Pair 42 45

main :: IO ()
main = do
    print (index memoized 1)
    print (index memoized 5)
    print (index memoized 10)
    print (index memoized 32)
    print p
    print ((tabulate . index) p :: Pair Int)
    print (index p True)
    print (index p False)
