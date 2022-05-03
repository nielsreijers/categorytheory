# Chapter 1

## 1. Implement, as best as you can, the identity function in your favourite language (or second favourite if your favourite language happens to be Haskell).

In C#:
```
public static T Identity<T>(T x) => x;
```

In Scheme:
```
(define (identity x) x)
```

## 2. Implement the composition function in your favourite language. It takes two functions as arguments and returns a function that is their composition.

In C#:
```
public static Func<A, C> Compose<A, B, C>(Func<A, B> f, Func<B, C> g) => x => g(f(x));
```

In Scheme:
```
(define (compose f g) (lambda (x) (g (f x))))
```

## 3. Write a program that tries to test that your composition function respects identity.

If the composition function respects identity, then the following should hold:
```
f . id = id . f = f
```

Since compose produces a new function in both C# and Scheme, we cannot test whether this holds for all functions. The best we can do is write a unit test and some test cases.

In C#:
```
public void TestCompositionRespectsIdentity<A, B>(Func<A, B> f, A x) {
    var fid = Chapter1.Compose(f, Chapter1.Identity);
    var idf = Chapter1.Compose<A, A, B>(Chapter1.Identity, f);
    var fx = f(x);
    Assert.AreEqual(fx, fid(x));
    Assert.AreEqual(fx, idf(x));
}

[TestMethod]
public void Test1_3() {
    TestCompositionRespectsIdentity((string x) => x.Length, "hello world");
    TestCompositionRespectsIdentity((int x) => x / 2, 41);
    TestCompositionRespectsIdentity((double x) => x / 2, 2.0/3.0);
}
```

In Scheme:
```
(define (test-compose f x)
  (let ((fid (compose f identity))
        (idf (compose identity f))
        (fx (f x)))
    (and (equal? fx (fid x))
         (equal? fx (idf x)))))
(test-compose (lambda (x) (string-length x)) "hello world")
(test-compose (lambda (x) (/ x 2)) 1)
(test-compose (lambda (x) (/ x 2)) (/ 2 3))
```

## 4. Is the world-wide web a category in any sense? Are links morphisms?

For it to be a category two things must hold:
- Every object must have an identity morphism
- Morphisms must compose

Both conditions are not met for the world-wide web with links as morphisms. Most pages do not contain a link to themselves, and if page A links to B, and page B links to C, this does not mean there is a link from A to C.

Therefore the web with links as morphisms is not a category.

## 5. Is Facebook a category, with people as objects and friendships as morphisms?

Again, both conditions do not hold. There is no identity friendship to oneself, and people are not necessarily friends with their friends' friends so friendships do not compose.

Therefore Facebook with friendships as morphisms is not a category.

## 6. When is a directed graph a category?

For it to be a category two conditions must hold:
- Identity: Every vertex must have an edge to itself.
- Composition: If there is an edge from A to B, and from B to C, there must be an edge from A to C.
