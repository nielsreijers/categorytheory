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

In the opposite category, everything is reversed, so if ```A -> B``` or ```B -> A```, the coproduct is ```B``` and ```A``` respectively, which is the case in a total order.

Fornatural numbers with 'less than or equal to', the coproduct is the largest of the two.

If ```A``` and ```B``` are not related, the coproduct is again the 'closest' object with morphisms _from_ both ```A``` and ```B```.

For sets with ⊆, it is now the union of both sets. Again, there may be other, larger, sets for which both ```A``` and ```B``` are a subset, but the union of ```A``` and ```B``` will also be a subset of those sets, giving us _m_.

For natural numbers with divisibility, the product is the greatest common divisor. Again, there maybe other common divisors, but these will also be divisors of the gcd, giving us _m_.

## 4. Implement the equivalent of Haskell ```Either``` as a generic type in your favorite language (other than Haskell).

In C#:
```
public class Either<A, B> {
    private A left;
    private B right;
    private bool isLeft;
    public Either(A Left) {
        this.left = Left;
        this.isLeft = true;
    }
    public Either(B Right) {
        this.right = Right;
        this.isLeft = false;
    }
    public C Factorizer<C>(Func<A, C> f, Func<B, C> g)
        => this.isLeft
            ? f(this.left)
            : g(this.right);
}
```

In Scheme:

Since Scheme is not statically typed we don't need separate left and right fields in the datatype, but only need to remember whether the value is a left or right value so we can apply the correct function in ```factorizer```.
```
(struct either (value is-left))
(define (left x) (either x #t))
(define (right x) (either x #f))
(define (factorizer f g x)
  (if (either-is-left x)
      (f (either-value x))
      (g (either-value x))))
```


## 5. Show that ```Either``` is  "better" coproduct than ```int``` equipped with these two injections:

```
int i'(int n) { return n; }
int j'(bool b) { return b ? 0 : 1; }
```

Looking back at the graph in challenge 3, ```a``` is ```int```, ```b``` is ```bool```, ```c``` is ```Either int bool``` and ```c'``` is ```int```.

We must present a function ```m```, such that ```i' = m . i``` and ```j' = m . j```:

```
int m(Either const & e) {
	if (e.isLeft)
		return e.left;
	else
		return e.right ? 0 : 1;
}
```

This shows that ```Either int bool``` can be easily transformed into an ```int``` with the two injections from ```int``` and ```bool``` that were given, which makes ```Either``` a better coproduct.

## 6. Continuing the previous problem: How would you argue that ```int``` with two injections ```i'``` and ```j'``` cannot be "better" than ```Either```?

For this ```int``` to be better than ```Either int bool``` there would have to be an ```m``` from ```int``` to ```Either int bool``` that factorizes ```i``` and ```j```.

However, if the input for ```m``` is 0 or 1, we do not know if this ```int``` is the result from an ```int``` passed through ```i'``` or a ```bool``` passed through ```j'```.

The factorizing function ```m``` would have to choose whether to map 0 and 1 to a left or right ```Either int bool```, so only one of the two conditions ```i' = m . i``` and ```j' = m . j``` can be satisfied.

## 7. Still continuing: What about these injections?
```
int i'(int n) {
	if (n < 0) return n;
	return n + 2;
}

int j'(bool b) { return b ? 0 : 1; }
```

These injections avoid the previous problem by reserving integers 0 and 1 for booleans. A factorizing ```m``` would map 0 to ```Right false```, 1 to ```Right true```, and any other integer ```x``` to ```Left x```.

This works, except that ```INT_MAX``` and ```INT_MAX - 1``` would cause ```i``` to overflow. Therefore ```j' = m . j``` but ```i' /= m . i```, so ```int``` with these injections is not a better coproduct than ```Either```.

In a language with unlimited precision integers it would work, and ```int``` with these two injections would be uniquely isomorphic with ```Either```.

## 8. Come up with an inferior candidate for a coproduct of ```int``` and ```bool``` that cannot be better than ```Either``` because it allows multiple acceptable morphisms from it to ```Either```.

An inferior candidate would be ```long``` with the following injections:

```
long i(int n) {
	return n;
}

long j(bool b) { return b ? MAX_INT+1 : MAX_INT+2; }
```

Since ```long``` is larger than ```int```, this avoids the overflow problem from challenge 7. We can now simple map ```int```s to the same value in ```long```, and use two values larger than ```MAX_INT``` to encode ```true``` and ```false```.

For this to be a better candidate than ```Either int bool``` with injections```i'``` and ```j'```, there would have to be a _unique_ ```m``` such that ```i' = m . i``` and ```j' = m . j```.

All ```m```s that map values ```x``` in the range ```[MIN_INT, MAX_INT]``` to ```Left x```, and map ```MAX_INT+1``` and ```MAX_INT+2``` to ```Right true``` and ```Right false``` respectively, satisfies the two conditions.

Therefore, the factorizing morphism exists, but it is not _unique_ since the morphism is free to map values outside of the range ```[MIN_INT, MAX_INT+2]``` to any value. 





