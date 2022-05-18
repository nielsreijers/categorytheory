# Chapter 3 - Categories Great and Small

## 1. Generate a free category from:
### a. A graph with one node and no edges
To make a free category, each node should have an identity morphism:

![Alt text](https://g.gravizo.com/source/challenge1amark?https%3A%2F%2Fraw.githubusercontent.com%2Fnielsreijers%2Fcategorytheory%2Fmain%2FChapter3.md)
<details>
<summary></summary>
challenge1amark
digraph G {
    node1 -> node1 [label="id"]
}
challenge1amark
</details>

### b. A graph with one node and one directed edge
Again we need to add the identity morphism. Additionally, the existing morphism ```f``` may compose with itself to form ```f . f```, which may again compose with ```f``` to produce ```f . f . f```, etc., leading to infinitely many morphisms from the node to itself.

![Alt text](https://g.gravizo.com/source/challenge1bmark?https%3A%2F%2Fraw.githubusercontent.com%2Fnielsreijers%2Fcategorytheory%2Fmain%2FChapter3.md)
<details>
<summary></summary>
challenge1bmark
digraph G {
    node1 -> node1 [label="id"]
    node1:n -> node1:n [label="f"]
    node1:w -> node1:w [label="f . f"]
    node1:s -> node1:s [label="f . f . f ... etc."]
}
challenge1bmark
</details>

### c. A graph with two nodes and a single arrow between them
Again, we add identity to all objects. In this case the existing morphism ```f``` can only compose with one of the identities, which by definition yields ```f``` again, so no additions new morphisms can be added.

![Alt text](https://g.gravizo.com/source/challenge1cmark?https%3A%2F%2Fraw.githubusercontent.com%2Fnielsreijers%2Fcategorytheory%2Fmain%2FChapter3.md)
<details>
<summary></summary>
challenge1cmark
digraph G {
    node1 -> node1 [label="id"]
    node1 -> node2 [label="f"]
    node2 -> node2 [label="id"]
}
challenge1cmark
</details>

### d. A graph with a single node and 26 arrows marked with the letters of the alphabet: a, b, c, ... z.
Besides adding identity, all the 26 morphisms compose, and resulting new morphisms compose again, producing an infinite number of morphisms. Only the first few for ```a```, ```b``` and ```c``` are shown below:


![Alt text](https://g.gravizo.com/source/challenge1dmark?https%3A%2F%2Fraw.githubusercontent.com%2Fnielsreijers%2Fcategorytheory%2Fmain%2FChapter3.md)
<details>
<summary></summary>
challenge1dmark
digraph G {
    node1 -> node1 [label="id"]
    node1:n -> node1:n [label="a"]
    node1:n -> node1:n [label="b"]
    node1:n -> node1:n [label="c"]
    node1:w -> node1:w [label="a . a"]
    node1:w -> node1:w [label="a . b"]
    node1:w -> node1:w [label="a . c"]
    node1:w -> node1:w [label="b . a"]
    node1:w -> node1:w [label="b . b"]
    node1:w -> node1:w [label="b . c"]
    node1:w -> node1:w [label="c . a"]
    node1:w -> node1:w [label="c . b"]
    node1:w -> node1:w [label="c . c"]
    node1:s -> node1:s [label="a . a . a"]
    node1:s -> node1:s [label="a . a . b"]
    node1:s -> node1:s [label="a . a . c"]
    node1:s -> node1:s [label="a . b . a"]
    node1:s -> node1:s [label="a . b . b"]
    node1:s -> node1:s [label="a . b . c"]
    node1:s -> node1:s [label="a . c . a"]
    node1:s -> node1:s [label="a . c . b"]
    node1:s -> node1:s [label="a . c . c ... etc"]
}
challenge1dmark
</details>

## 2. What kind of order is this?
### a. A set of sets with the inclusion relation: A is included in B if every element of A is also an element of B.

It is a pre-order since if A ⊆ B, and B ⊆ C, then A ⊆ C.

It is also a partial order since if A ⊆ B and B ⊆ A, then A = B.

It is not a total order since for disjoint sets, neither A ⊆ B or B ⊆ A is true.

### b. C++ types with the following subtyping relation: ```T1``` is a subtype of ```T2``` if a pointer to ```T1``` can be passed to a function that expects a pointer to ```T2``` without triggering a compilation error.


It is a pre-order since if ```T1``` is a subtype of ```T2``` and ```T2``` a subtype of ```T3```, then ```T1``` is also a subtype of ```T3```.

When only considering classes, it is also a partial order since if ```T1``` is a subtype of ```T2``` and ```T2``` a subtype of ```T1```, we would have a circular inheritance which is not possible in C++, unless ```T1``` and ```T2``` are the same class.
For simple types passing a pointer to an different type always seems to result in a compilation error, with the exception that a different pointer may be passed to a function expecting void*. The reverse is not true however: a void* cannot be passed to a function expecting a different pointer type without a cast.

It is not a total order since not all types are related.

## 3. Considering that ```Bool``` is a set of two values ```True``` and ```False```, show that it forms two (set-theoretical) monoids with respect to, respectively operator ```&&``` (AND) and ```||``` (OR).

To form a monoid the operator must be associative, and there must be an element that behaves like a unit with respect to the operator.

Both operators are associative:
```
(a && b) && c = a && (b && c)
(a || b) || c = a || (b || c)
```

The unit element is ```True``` and ```False``` for ```&&``` and ```||``` respectively:

```
True && a = a
False || a = a
```

## 4. Represent the ```Bool``` monoid with the AND operator as a category: List the morphisms and their rules of composition.

The identity morphism represents True. In addition we need a second morphism to represent False.
By definition, composition with identity doesn't change anything. Composition of false with itself results in false.
```
id . id = id
id . false = false . id = false
false . false = false
```


## 5. Represent addition modulo 3 as a monoid category.

Since the addition is modulo 3, we only need 3 morphisms to add 0, 1 or 2.
Identity represents adding 0, so two more morphisms ```+1``` and ```+2``` are needed.

The rules of composition are:
```
id . id = id
id . +1 = +1 . id = +1
id . +2 = +2 . id = +2
+1 . +1 = +2
+2 . +2 = +1
+1 . +2 = +2 . +1 = id
```
