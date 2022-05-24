# Chapter 14 - Representable Functors

## 1. Show that the hom-functors map identity morphisms in C to corresponding identity morphisms in Set.

As shown in the text, for the C(a,-) functor, lifting a function f is defined as precomposition:

C(a,f) h = f . h

So for lifting identity: C(a, id) h = id . h = h, and C(a, id) = id.

Similarly for C(-,a), lifting is postcomposition:

C(f,a) h = h . f

So C(id, a) h = h . id = h, and C(id, a) = id.

Since the profunctor C(-,=) is define in terms of the two previous functors, this must also map identity to identity.

## 2. Show that ```Maybe``` is not representable.

A functor is representable if it is isomorphic to the hom-functor C(a,-), for some a.

This means there must be an invertible natural transformation between ```Maybe``` and C(a,-). In other words we need to find these two functions, and their composition must be identity:
```
tabulate :: (a -> x) -> Maybe x
index :: Maybe x -> (a -> x)
```

The problem is with ```index```. For the ```Just x``` case it will be easy to create a function, but for ```Nothing``` we can never construct a function ```a -> x```, regardless of what ```a``` we chose.

This is the same problem as shown for the list functor in the text, which is not surprising since ```Maybe``` can be seen as a special kind of list with a maximum length of 1.

Still this argument is not completely satisfying. We have shown we cannot write a parametrically polymorph ```index```, but does that mean we cannot find an ```index``` and ```tabulate``` for any ```Maybe```?

For ```Maybe Integer```, the following implementation would work, with ```a::()```:
```
    tabulate f 
        | f () == 0 = Nothing
        | f () < 0  = Just (f ())
        | otherwise = Just (f () - 1)
    index Nothing = \_ -> 0
    index (Just x)
        | x < 0     = \_ -> x + 1
        | otherwise = \_ -> x
```

We simply use a function returning 0 to encode Nothing, and shift all the non-negative numbers up by 1.

But this only works for infinite types where we can reserve a value for ```Nothing``` and shift all others without creating an overflow. For ```Maybe Int``` (fixed precision) we would get an overflow encoding ```MAX_INT```.

For types ```x``` with a fixed precision and X possible values, it's generally not possible to find a function type ```a -> x``` with exactly X+1 possible functions. Therefore, there can never be an isomorphism, since some values always get collapsed in one direction or the other, which means either ```tabulate . index /= id``` or ```index . tabulate /= id```.

## 3. Is the ```Reader``` functor representable?

Again, a functor is representable if it is isomorphic to the hom-functor C(a,-), for some a.

Since the ```Reader``` functor _is_ the hom-functor, it is isomorphic to itself (using identity natural transformations), and thus representable.

## 4. Using ```Stream``` representation, memoize a function that squares its argument.

Using the definitions from the text:
```
class Representable f where
    type Rep f :: *
    tabulate :: (Rep f -> x) -> f x
    index    :: f x -> Rep f -> x

data Stream x = Cons x (Stream x)
instance Representable Stream where
    type Rep Stream = Integer
    tabulate f = Cons (f 0) (tabulate (f . (+1)))
    index (Cons b bs) = \n -> if n == 0 then b else index bs (n - 1)
```

We can memoize the function as follows:
```
memoized :: Stream Integer
memoized = tabulate (\x -> x * x)
```

To test:
```
main :: IO ()
main = do
    print (index memoized 1)
    print (index memoized 5)
    print (index memoized 10)
    print (index memoized 32)
```
will print
```
1
25
100
1024
```
as output.

## 5. Show that ```tabulate``` and ```index``` for ```Stream``` are indeed the inverse of each other. (Hint: use induction.)

First we show ```index . tabulate = id```.

For n = 0:
```
index (tabulate f) 0
= // definition of tabulate
index (Cons (f 0) (tabulate (f . (+1)))) 0
= // definition of index
f 0
```

Then for n > 0:
```
index (tabulate f) n
= // definition of tabulate
index (Cons (f 0) (tabulate (f . (+1)))) n
= // definition of index
index (tabulate (f . (+1))) (n - 1)
= // induction: assume (index . tabulate) is identity for n - 1
(f . (+1)) (n - 1)
= // cancel +1 against -1
f n
```

Then we need to prove the reverse is true and ```tabulate . index = id``` as well.

First the head of the stream:
```
tabulate (index (Cons b bs))
// definition of tabulate
Cons (index (Cons b bs) 0) (tabulate ((index (Cons b bs)) . (+1)))
// definition of index
Cons ((\n -> if n == 0 then b else index bs (n - 1)) 0) (tabulate ((index (Cons b bs)) . (+1)))
// function application
Cons b (tabulate ((index (Cons b bs)) . (+1)))
```

So for the head ```b```, ```tabulate . index``` is indeed identity. Continuing the equational reasoning for the tail:

```
Cons b (tabulate ((index (Cons b bs)) . (+1)))
// definition of index
Cons b (tabulate ((\n -> if n == 0 then b else index bs (n - 1)) . (+1)))
// remove the if, since we know n > 0
Cons b (tabulate ((\n -> index bs (n - 1)) . (+1)))
// cancel -1 and +1
Cons b (tabulate (\n -> index bs n))
// remove lambda
Cons b (tabulate (index bs))
// induction: assume (tabulate . index) is identity for the tail
Cons b bs
```

## 6. The functor ```Pair a = Pair a a``` is representable. Can you guess the type that represents it? Implement ```tabulate``` and ```index```.

The function to represent the pair needs to be able to store two ```a```.

The ```Pair``` functor is a product of ```a``` and ```a```, or ```a^2```. The function to represent this must have an equal number of possible values so it can be a bijection with the functor. The function type is an exponent: ```return_value ^ argument```. The return value is ```a```, and from the product in the ```Pair``` functor it is clear the argument type must have a cardinality of 2.

The canonical type for this is ```Bool```. The implementation of ```Representable``` is then quite straightforward:

```
data Pair a = Pair a a
instance Representable Pair where
    type Rep Pair = Bool
    tabulate f = Pair (f True) (f False)
    index (Pair x y) True  = x
    index (Pair x y) False = y
```