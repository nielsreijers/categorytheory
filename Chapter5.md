# Chapter 5 - Products and Coproducts

## 1. Show that the terminal object is unique up to unique isomorphism.

The definition of the terminal object is: the object with one and only one morphism coming to it from any object in the category.

If two objects, A and B, both have a single morphism coming to them from any object, they are both terminal objects, and the following morphisms must exist: ```f :: A -> B``` and ```g :: B -> A```.

The composition of these two leads back to the original object: ```f . g :: A -> A``` and ```g . f :: B -> B```. By definition of the terminal object, there is only a single morphism coming from ```A``` to ```A``` and from ```B``` to ```B```, which must be the identity morphism, so ```f . g = id```  and ```g . f = id```, which shows ```A``` and ```B``` are isomorphic. The isomorphism is unique since, again by definition of the terminal object, ```f``` and ```g``` are the only morphisms between ```A``` and ```B```.

## 2. What is a product of two objects in a poset? Hint: Use the universal construction.

In a poset if ```A -> B``` and ```B -> A```, then ```B = A```, so it is a category with no loops and with 0 or 1 morphism between any two objects since the relation either holds or not.

The definition of the product of two objects is:

> A product of two objects _a_ and _b_ is the object _c_ equipped with two projections such that for any other object _c'_ equipped with two projections there is a unique morphism _m_ from _c'_ to _c_ that factorizes those projections.
![Alt text](https://g.gravizo.com/source/challenge2mark?https%3A%2F%2Fraw.githubusercontent.com%2Fnielsreijers%2Fcategorytheory%2Fmain%2FChapter5.md)
<details>
<summary></summary>
challenge2mark
digraph G {
    "c" -> "a" [label="p"]
    "c" -> "b" [label="q"]
    "c'" -> "a" [label="p'"]
    "c'" -> "b" [label="q'"]
    "c'" -> "c" [label="m"]
}
challenge2mark
</details>

If ```A -> B``` or ```B -> A```, the product is ```A``` and ```B``` respectively, which is the case in a total order. For example for natural numbers with 'less than or equal to', the product is simply the smallest of the two.

If ```A``` and ```B``` are not related, as may be the case in a poset, the product is the 'closest' object with morphisms to both ```A``` and ```B```.

For example, for sets with ⊆, it is the intersection of ```A``` and ```B``` since this is the largest set that is a subset of both. There may be other sets that are a subset of both ```A``` and ```B```, but these are also subsets of the intersection, which gives us the morphism _m_ from this smaller set to the intersection.

For natural numbers with divisibility, the product is the least common multiple. Again, there are (infinitely many) other numbers divisible by ```A``` and ```B```, but these will also be divisible by the least common multiple, giving us _m_.

## 3. What is a coproduct of two objects in a poset?

The definition of the coproduct of two objects is:

> the object _c_ equipped with two injections such that for any other object _c'_ equipped with two injections there is a unique morphism _m_ from _c_ to _c'_ that factorizes those injections.
![Alt text](https://g.gravizo.com/source/challenge3mark?https%3A%2F%2Fraw.githubusercontent.com%2Fnielsreijers%2Fcategorytheory%2Fmain%2FChapter5.md)
<details>
<summary></summary>
challenge3mark
digraph G {
    "a" -> "c" [label="i"]
    "b" -> "c" [label="j"]
    "a" -> "c'" [label="i'"]
    "b" -> "c'" [label="j'"]
    "c" -> "c'" [label="m"]
}
challenge3mark
</details>


## 4. Implement the equivalent of Haskell ```Either``` as a generic type in your favorite language (other than Haskell).

## 5. Show that ```Either``` is  "better" coproduct than ```int``` equipped with two injections: (Hint: Define a function ```m(Either const & e);``` that factorizes ```i``` and ```j```)

```
int i(int n) { return n; }
int j(bool b) { return b ? 0 : 1; }
```

## 6. Continuing the previous problem: How would you argue that ```int``` with two injections ```i``` and ```j``` cannot be "better" than ```Either```?

## 7. Still continuing: What about these injections?
```
int i(int n) {
	if (n < 0) return n;
	return n + 2;
}

int j(bool b) { return b ? 0 : 1; }
```

## 8. Come up with an inferior candidate for a coproduct of ```int``` and ```bool``` that cannot be better than ```Either``` because it allows multiple acceptable morphisms from it to ```Either```.