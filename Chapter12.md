# Chapter 12 - Limits and Colimits

## 1. How would you describe a pushout in the category of C++ classes?

Assuming we still use the subclass relationship as morphisms, the pushout is the reverse of the pullback diagram. For the pullback, we had two classes ```B``` and ```C```, both inheriting from ```A```. The pullback was a class ```D``` that is both a subclass of ```B``` and ```C```, such that any class inheriting from ```B``` and ```C``` also inherits from ```D```. This cannot be expressed in C++, but without duck typing, this ```D``` would have to be the smallest union of ```B``` and ```C```.

If we reverse this diagram, for the pushout the diagram would be ```A``` inheriting from both ```B``` and ```C```. The pushout ```D``` would be the class with the maximum functionality that still allows it to be a shared base class for of ```B``` and ```C```.

In essence where the pullback represents the union of the functionality in ```B``` and ```C```, the pushout represents the intersection.

## 2. Show that the limit of the identity functor Id :: C -> C is the initial object.

The definition of the initial object is: "The initial object is the object that has one _and only one_ morphism going to any object in the category."

Instead of having a simple source category with a few objects with a functor F selecting two or three objects in C, now the source category is the whole of C, and F is the identity functor selecting all objects in C.

The apex of a cone for this functor has to be an object I with at least 1 morphism going to every object X in C. For this I to be the initial object, we must show that this is the only morphism between I and X.

The cone is a natural transformation from ΔI to IdC. Let's call the component of this transformation at I PI, and the component at X PX. For any morphism f from I to X, the naturality condition states that:

```
IdC(f) . PI = PX . ΔI(f)
```

I.e. first going through the natural transformation at I from ΔI(I) to IdC(I) and then through f lifted in IdC, should be equal to going through f lifted in ΔI, and then through the natural transformation at X.

Several of these operations are identity: f lifted in ΔI just becomes identity on I, so the right hand side is simple PX.

PI must also be identity, because for each ```f :: X -> Y``` we know ```f . PX = PY```. We can take ```f = PY``` (with signature ```f :: I -> Y```) to get ```PY . PI = PY```. Therefore PI must be identity, and the left hand side of the equation becomes ```IdC(f)```, or simply ```f```.

The whole equation is reduced to ```f = PX```, which tells us that for any X, any morphism from I to X must be equal to PX. If the limit I exists, there is a morphism from it to any object, and there can be only one morphism from it to any object. Therefore, by definition, it is the initial object.

## 3. Subsets of a given set form a category. A morphism in that category is defined to be an arrow connecting two sets if the first is a subset of the second. What is a pullback of two sets in such a category? What's a pushout? What are the initial and terminal objects?

The pullback ```d``` is a subset of ```a``` and ```b```, so that for any ```d'``` that is a subset of both sets, ```d'``` is also a subset of ```d```. This means ```d``` is the largest set that is a subset of ```a``` and ```b```, which is the intersection of both sets.

Reversing the arrows means the pushout ```d``` is now a superset of ```a``` and ```b```, so that any ```d'``` that is a superset of both sets, is also a superset of ```d```. This means it is the smallest superset, which is the union of ```a``` and ```b```.

The initial object is a subset of every set, so it must be the empty set, and the terminal object is a superset of every set, so it is the union of all sets, containing all elements contained in any set.

## 4. Can you guess what a coequalizer is?
For two morphisms ```f, g :: A -> B```, the equalizer is an object ```C``` and morphism ```p :: C -> A```, such that ```f . p = g . p```. This can be seen as ```p``` selecting the inputs in ```A``` for which ```f``` and ```g``` are equal.

When the arrows are reversed for the coequalizer, we get a similar commuting triangle, with the compositions reversed: ```p . f = p . g```. This time ```p``` is a morphism ```p :: A -> C``` that happens after ```f``` and ```g```, and must make their result equal.

A trivial option would be ```C = ()``` and ```p _ = ()```, which is indeed a valid co-cone, but it is not universal since any other valid cone could be trivially transformed into this one, so any other cone would be a better option.

The universal co-cone in this case is the smallest equivalence relation: ```p``` must partition ```A``` into the largest possible number of partitions, such that for any ```x``` in ```B```, ```(p . f) x = (p . q) x``` still holds.

In other words, wherever ```f``` and ```g``` produce different results for ```x```, ```p``` maps those results to the same value or equivalence class.

For example using natural numbers, if ```f x = x``` and ```g x = x + 1```, we would get an endless chain: 1 should be equivalent to 2, 2 should be equivalent to 3, etc. So the coequalizer is a single equivalent class and is isomorphic to the trivial ```p _ = ()``` shown before.

If we chose ```f x = x``` and ```g x = x - (x % 2)```, we map each even number to itself and odd numbers to the even number before it. A valid coequalizer for this would be ```p x = x - (x % 2)``` (since this function is idempotent).

For ```f x = x``` and ```g x = x % 2``` we would end up with two equivalence classes for odd and even numbers and ```p = even``` would be a valid coequalizer.

## 5. Show that, in a category with a terminal object, a pullback towards the terminal object is a product.

First, let's review the graph for the pullback:

![Alt text](https://g.gravizo.com/source/challenge5amark?https%3A%2F%2Fraw.githubusercontent.com%2Fnielsreijers%2Fcategorytheory%2Fmain%2FChapter12.md)
<details>
<summary></summary>
challenge5amark
digraph G {
    d -> a [label="p"]
    d -> b [label="r"]
    d -> c [label="q"]
    a -> b [label="f"]
    c -> b [label="g"]
}
challenge5amark
</details>

and for the product:

![Alt text](https://g.gravizo.com/source/challenge5bmark?https%3A%2F%2Fraw.githubusercontent.com%2Fnielsreijers%2Fcategorytheory%2Fmain%2FChapter12.md)
<details>
<summary></summary>
challenge5bmark
digraph G {
    d -> a [label="p"]
    d -> b [label="q"]
}
challenge5bmark
</details>

If we can add the terminal object ```b``` to the graph of the product and show the result is a valid pullback, this would show the pullback to the terminal object and the product are the same thing.

For this two things must be true: there must be only one way to add ```b``` and its morphisms, and all the required naturality conditions must still hold.

The first is easy to see. Since ```b``` is the terminal object, by definition there is one, and only one morphism to it from any object, and so the morphisms ```f```, ```r```, and ```g``` are the only morphisms from the three other objects to ```b```.

The naturality conditions hold by the a similar argument. ```f . p = r``` must be true, because we are in a category, so there must be a morphism from ```d``` to ```b``` that is equal the composition of ```p``` and ```f```, and by definition of the terminal object this has to be ```r``` because it is the only choice. The same goes for any commuting triagle towards the terminal object.

Therefore, when we add the terminal object ```b``` to the graph of the product, all conditions are automatically satisfied which shows that ```d``` with morphisms ```p``` and ```q``` is also a valid pullback towards the terminal object.

## 6. Similarly, show that a pushout from an initial object (if one exists) is the coproduct.

Since the initial object is the "co-terminal" object and the pushout the "co-pullback", all the arrows are reversed and the same argument  holds.
 