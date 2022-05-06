# Chapter 7 - Functors

## 1. Can we turn the Maybe type constructor into a functor by defining: ```fmap _ _ = Nothing``` which ignores both of its arguments? (Hint: Check the functor laws.)

The functor laws state that ```fmap``` must preserve identity and composition:
- Identity: ```F id_a = id_Fa``` 
- Composition: if ```h = g . f``` then ```F h = F g . F f```

Using equational reasoning it is easy to prove the proposed ```fmap``` does not preserve identity for any ```Just x```:

```
fmap id (Just x)
= { definition of fmap }
Nothing
```

## 2. Prove functor laws for the reader functor. Hint: it's really simple.

The implementation of ```fmap``` for the reader functor is simply function composition:
```
fmap = (.)
```

Using equational reasoning we can prove preservation of identity and composition:

```
fmap id f = id f

fmap id f
= { definition of fmap }
id . f
= { composition with identity }
f
= { definition of identity }
id f
```

```
fmap (g . f) x = (fmap g . fmap f) x

fmap (g . f) x
= { definition of fmap }
g . f . x
= { definition of fmap }
(g . (fmap f x)
= { definition of fmap }
fmap g (fmap f x)
= { definition of composition }
(fmap g . fmap f) x
```

## 3. Implement the reader functor in your second favorite language (the first being Haskell, of course).

In C#:
```
public static class Reader {
    public static Func<Func<R, A>, Func<R, B>> fmap<R, A, B>(Func<A, B> f) =>
        reader => (x => f(reader(x)));

    public static Func<R, B>fmap<R, A, B>(Func<A, B> f, Func<R, A> reader) =>
        x => f(reader(x));
}
```


## 4. Prove the functor laws for the list functor. Assume that the laws are true for the tail part of the list you're applying it to (in other words, use _induction_).

The functor laws: ```fmap id = id``` and ```fmap (g . f) = (fmap g . fmap f)```

For lists we have two options to consider: 
```
data List a = Nil | Cons a (List a)

instance Functor List where
fmap _ Nil = Nil
fmap f (Cons x t) = Cons (f x) (fmap f t)
```

Identity:
```
fmap id Nil
= { definition of fmap }
Nil
= { definition of id }
id Nil

fmap id (Cons x t)
= { definition of fmap }
Cons (id x) (fmap id t)
= { definition of id }
Cons x (fmap id t)
= { definition of id }
id (Cons x (fmap id t))
= { induction }
id (Cons x (id t))
= { definition of id }
id (Cons x t)
```

Composition:
```
fmap (g . f) Nil 
= { definition of fmap }
Nil
= { definition of fmap }
fmap f Nil
= { definition of fmap }
fmap g (fmap f Nil)
= { definition of composition }
(fmap g . fmap f) Nil

fmap (g . f) (Cons x t)
= { definition of fmap }
Cons ((g . f) x) (fmap (g . f) t)
= { induction }
Cons ((g . f) x) (fmap g . fmap f) t)
= { defintion of composition }
Cons (g (f x)) (fmap g (fmap f t))
= { definition of fmap }
fmap g (Cons (f x) (fmap f t)
= { definition of fmap }
fmap g (fmap f (Cons x t))
= { defintion of composition }
(fmap g . fmap f) (Cons x t)
```
