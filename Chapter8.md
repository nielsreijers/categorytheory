# Chapter 8 - Functoriality

## 1. Show that the data type: ```data Pair a b = Pair a b``` is a bifunctor. For additional credit implement all three methods of ```Bifunctor``` and use equational reasoning to show that these definitions are compatible with the default implementations whenever they can be applied.

The implementation of ```Bifunctor``` is fairly straighforward:
```
instance Bifunctor Pair where
    bimap g h (Pair x y) = Pair (g x) (h y)
    first g (Pair x y) = Pair (g x) y
    second h (Pair x y) = Pair x (h y)
```

To show it is bifunctorial, it is enough to show it is functorial in both arguments separately. To do this we must show ```first``` and ```second``` preserve identity and composition.

Identity:
```
first id (Pair x y)
= { definition of first }
Pair (id x) y
= { definition of identity }
Pair x y

second id (Pair x y)
= { definition of second }
Pair x (id y)
= { definition of identity }
Pair x y
```

Composition:
```
first (g . f) (Pair x y)
= { definition of first }
Pair ((g . f) x) y
= { definition of composition }
Pair (g (f x)) y
= { definition of first }
first g (Pair (f x) y)
= { definition of first }
first g (first f (Pair x y))
= { definition of composition }
(first g . first f) (Pair x y)

second (g . f) (Pair x y)
= { definition of second }
Pair x ((g . f) y)
= { definition of composition }
Pair x (g (f y))
= { definition of second }
second g (Pair x (f y))
= { definition of second }
second g (second f (Pair x y))
= { definition of composition }
(second g . second f) (Pair x y)
```

Using equational reasoning to show this implementation of ```bimap``` is equivalent to the default ```bimap g h = first g . second h``` with the given implementations of ```first``` and ```second```:

```
bimap g h (Pair x y)
= { definition of bimap }
Pair (g x) (h y)
= { definition of first }
first g (Pair x (h y))
= { definition of second }
first g (second h (Pair x y))
= { definition of composition
first g . second h (Pair x y)
```

Using equational reasoning to show this implementation of ```first``` is equivalent to the default ```first g = bimap g id``` with the given implementation of ```bimap```:

```
first g (Pair x y)
= { definition of first }
Pair (g x) y
= { definition of identity }
Pair (g x) (id y)
= { definition of bimap }
bimap g id (Pair x y)
```

The proof for ```second``` follows the same pattern.

## 2. Show the isomorphism between the standard definition of ```Maybe``` and this desugaring. Hint: Define two mappings between the two implementations. For additional credit, show that they are the inverse of each other using equational reasoning.

```type Maybe' a = Either (Const () a) (Identity a)```
 
 The following two functions map ```Maybe``` to ```Maybe'``` and back:
 
 ```
 f :: Maybe a -> Maybe' a
f Nothing = Left (Const ())
f (Just x) = Right (Identity x)

g :: Maybe' a -> Maybe a
g (Left (Const ())) = Nothing
g (Right (Identity x)) = Just x
```
 
For ```Maybe``` and ```Maybe'``` to be isomorphic, the composition of these two must be equal to identity: ```f . g = g . f = id```. This is trivially shown using equational reasoning:

```
g (f Nothing)
= { definition of f }
g (Left (Const ()))
= { definition of g }
Nothing
```

```
f (g (Left (Const ()))
= { definition of g }
f Nothing
= { definition of f }
Left (Const ())
```

```
g (f (Just x))
= { definition of f }
g (Right (Identity x))
= { definition of g }
Just x
```

```
f (g (Right (Identity x)))
= { definition of g }
f (Just x)
= { definition of f }
Right (Identity x)
```

## 3. Let's try another data structure. I call it a ```PreList``` because it's a precursor to a ```List```. It replaces recursion with a type parameter ```b```. You  could recover our earlier definition of a ```List``` by recursively applying ```PreList``` to itself (we'll see how it's done when we talk about fixed points). Show that ```PreList``` is an instance of ```Bifunctor```.

```data PreList a b = Nil | Cons a b```

```
instance Bifunctor PreList where
bimap _ _ Nil = Nil
bimap g h (Cons x y) = Cons (g x) (h y)
first _ Nil = Nil
first g (Cons x y) = Cons (g x) y
second _ Nil = Nil
second h (Cons x y) = Cons x (h y)
```

To show ```PreList``` is bifunctorial, we can skip the ```Cons``` part, since it is equivalent to ```Pair``` binfunctor.
Showing ```first``` preserves identity and composition for ```Nil```:

```
first id Nil
= { definition of first }
Nil

first (g . f) Nil
= { definition of first }
Nil
= { definition of first }
first g Nil
= { definition of first }
first g (first f Nil)
= { definition of composition }
(first g . first f) Nil
```

The proof for ```second``` is identical.

## 4. Show that the following data types define bifunctors in ```a``` and ```b```. For additional credit, check your solutions against Conor McBride's paper [Clowns to the Left of me, Jokers to the Right](http://strictlypositive.org/CJ.pdf)

```
data K2 c a b = K2 c
data Fst a b = Fst a
data Snd a b = Snd b
```

```
instance Bifunctor (K2 c) where
    bimap _ _ (K2 x) = K2 x

instance Bifunctor Fst where
    bimap f _ (Fst x) = Fst (f x)

instance Bifunctor Snd where
    bimap _ g (Snd x) = Snd (g x)
```


## 5. Define a bifunctor in a language other than Haskell. Implement ```bimap``` for a generic pair in that language.

In C# we would like to implement this as an interface that defines ```Bimap```, and a class implementing it:

```
public interface IBifunctor<A, B> {
    IBifunctor<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g);
}

public class Pair<A, B> : IBifunctor<A, B> {
    public A item1;
    public B item2;

    public Pair<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g)
        => new Pair<C, D> {
            item1 = f(item1),
            item2 = g(item2)
        };
}
```

However, in the current version (10) of C# this does not work since the implementation of ```Bimap``` must match the signature of the interface exactly and return an ```IBifunctor<C, D>``` instead of a ```Pair<C, D>```.

Since C# version 9, [covariant returns](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-9.0/covariant-returns) for overridden methods from a superclass are supported, and a draft specification exists that would extend this to interface implementations which would allow the implementation above.

Currently, the best we can do using an interface is something like this, implementing the interface method to return an ```IBifunctor<C, D>```, and adding a separate method to return a ```Pair<C, D>```:
```
public class Pair<A, B> : IBifunctor<A, B> {
    public A item1;
    public B item2;

    IBifunctor<C, D> IBifunctor<A, B>.Bimap<C, D>(Func<A, C> f, Func<B, D> g)
        => this.Bimap(f, g);

    public Pair<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g)
        => new Pair<C, D> {
            item1 = f(item1),
            item2 = g(item2)
        };
}
```

The alternative is to make ```Bifunctor``` an abstract base class, and use the C# 9 feature to allow covariant returns and return a ```Pair<C, D>``` instead of a ```Bifunctor<C, D>```:

```
public abstract class Bifunctor<A, B> {
    public abstract Bifunctor<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g);
}

public class Pair<A, B>: Bifunctor<A, B> {
    public A item1;
    public B item2 ;

    public override Pair<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g) =>
        new Pair<C, D> {
            item1 = f(item1),
            item2 = g(item2)
        };
}
```

However, even in this case the abstract class cannot _enforce_ the subclass to return it's own type. There's no way in C# to express the equivalent of the Haskell class definition ```class Bifunctor f where bimap :: (a -> c) -> (b -> d) -> f a b -> f c d```, because there is no C# equivalent of the type variable ```f```.

## 6. Should ```std::map``` be considered a bifunctor or a profunctor in the two template arguments ```Key``` and ```T```? How would you redesign this data type to make it so?

This question is trickier than it seems. I'm not very familiar with C++, but maps are similar across most languages. They may differ in how many values can be stored for a given key, or how lookups for missing keys are handled, but they are all similar to a function ```Key -> T```, ```Key -> [T]```, ```Key -> Maybe T```, ```Key -> Pair Bool T```, or something similar. In fact the Haskell ```Data.Map``` has an infix operator ```(!) :: Ord k => Map k a -> k -> a``` that turns the map into a function.

We know a function ```a -> b``` is a profunctor, contra variant in ```a```, and covariant in ```b```, so we would expect a map to be a profunctor as well. Given a function ```f :: Key2 -> Key```, we can turn lookups using ```Key``` into lookups using ```Key2``` by first transforming the ```Key2``` into a ```Key``` and then doing the original lookup.

Another way to look at it, is to consider a map as a list of ```Key, T``` pairs. We know that a pair is a bifunctor, and that the list is also functorial, so from this perspective we would expect a map to be a bifunctor. Given a function ```g :: Key -> Key2``` we can create a new map of ```Key2, T``` pairs by applying ```g``` to all the keys in the original list, and choosing some policy to handle collisions (since multiple ```Key```s may map to the same ```Key2```).

So it seems a map can be considered a profunctor if there is a way to insert ```f``` to turn the ```Key2``` into a ```Key``` before the original lookup, and that it can be considered a bifunctor if there is a way to iterate over the key values, so we can create a new map, using ```g``` to transform all the ```Key```s into ```Key2```s.

Since ```std::map``` does not appear to have a way to insert ```f``` before the lookup, but does allow us to iterate over the keys, this makes it a bifunctor, but not a profunctor.
