# Chapter 15 - The Yoneda Lemma

## 1. Show that the two functions ```phi``` and ```psi``` that form the Yoneda isomorphism in Haskell are the inverses of each other.

```
phi :: (forall x . (a -> x) -> F x) -> F a
phi alpha = alpha id

psi :: F a -> (forall x . (a -> x) -> F x)
psi fa h = fmap h fa
```

To show the isomorphism, we need to show these two compose to identity in both directions.

First show ```phi . psi = id```: 
```
phi (psi fa)
= // definition of psi
phi (\h => fmap h fa)
= // definition of phi
(\h => fmap h fa) id
= // function application
fmap id fa
= // preservation of identity 
fa
```

Then ```psi . phi = id```: 
```
psi (phi alpha)
= // definition of phi
psi (alpha id)
= // definition of psi
\h -> fmap h (alpha id)
= // by naturality: fmap h . alpha = alpha . fmap h
\h -> alpha (fmap h id)
= // fmap for hom-functor is precomposition
\h -> alpha (h . id)
= // eliminate composition with identity
\h -> alpha h
=
alpha
```

## 2. A discrete category is one that has objects but no morphisms other than identity morphisms. How does the Yoneda lemma work for functors from such a category?

With no morphisms, the image of C under the hom functor C(a,-) is reduced to two sets: the empty set and the singleton set. Only ```a``` is mapped to the singleton set, all other objects are collapsed into the empty set.

The natural transformation from C(a,-) to a set-valued functor ```F``` is a collection of functions ```alpha :: C(a, x) -> F x```.

The component at points other than ```a```, map ```C(a,b) -> F b```, and since ```C(a,b)``` is the empty set, there is only one choice: the ```absurd``` function.

The component at ```a``` maps the singleton set ```C(a,a)``` to select a single object in ```F a```. Consistent with the Yoneda lemma, there are as many options to choose as there are elements in ```F a```.

## 3. A list of units ```[()]``` contains no other information but its length. So, as a data type, it can be considered an encoding of integers. An empty list encodes zero, a singleton ```[()]``` (a value, not a type) encodes one, and so on. Construct another representation of this data type using the Yoneda lemma for the list functor.

The Yoneda lemma states:
```
forall r . (a -> r) -> F r ≅ F a
```

```F  a``` in this case is ```[()]```, so we get:

```
forall r . (() -> r) -> [r] ≅ [()]
```

There is a one-on-one correspondence between natural transformations from the Hom functor ```() -> r``` to lists of ```r```s, and lists of ```()```. These natural transformations have the signature ```(() -> r) -> [r]```.

We can simply use the ```phi``` and ```psi``` from the first question to obtain the alternative representation as a ```() -> r``` function:

```
phi alpha = alpha id
psi fa h = fmap h fa
```

We can test this with:
```
zero = psi ([] :: [()])
one  = psi [()]
four = psi [(), (), (), ()]

main = do
    print (phi zero)
    print (phi one)
    print (phi four)
```

This will print:
```
[]
[()]
[(), (), (), ()]
```
The signatures of ```zero```, ```one```, and ```four``` are all ```(() -> a) -> [a]```, with the length of the resulting list encoded in the function.
