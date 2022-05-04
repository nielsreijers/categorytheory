# Chapter 2 - Types and Functions

## 1. Define a higher-order function (or a function object) ```memoize``` in your favorite language. This function takes a pure function ```f``` as an argument and returns a function that behaves almost exactly life ```f```, except that it only calls the original function once for every argument, stores the result internally, and subsequently returns this stored value every time it's called with the same argument.

In C#:
```
public static Func<A, B> Memoize<A, B>(Func<A, B> f) {
    Dictionary<A, B> results = new Dictionary<A, B>();
    return x => {
        if (!results.ContainsKey(x)) {
            results[x] = f(x);
        }
        return results[x];
    };
}
```

In Scheme:
```
(define (memoize f)
  (let ((d (make-hash)))
     (lambda (x)
       (cond [(not (dict-has-key? d x)) (dict-set! d x (f x))])
       (dict-ref d x))))
```

## 2. Try to memoize a function from your standard library that you normally use to produce random numbers. Does it work?

In C#:
```
Func<int, int> f = max => new Random().Next(0, max);
var memoized = Chapter2.Memoize<int, int>(f);
```

In Scheme:
```
(define (f max) (random max))
(define memoized (memoize f))
```

This doesn't work since it will always return the same value after the first call for some maximum value.

## 3. Most random number generators can be initialized with a seed. Implement a function that takes a seed, calls the random number generator with that seed, and returns the result. Memoize that function. Does it work?

In C#:
```
Func<int, int> f = seed => new Random(seed).Next(0, 1000);
var memoized = Chapter2.Memoize<int, int>(f);
```

In Scheme:
```
(define (f seed)
  (random-seed seed)
  (random 1000))
(define memoized (memoize f))
```

This works because for a given seed, the function should always return the same pseudo-random number, so it can be safely memoized.

## 4. Which of these C++ functions are pure?
a. The factorial function is pure, because its return value depends only on the parameter passed and it has no side effects.

b. ```std::getchar()``` is not pure because its return value depends on the user's input.

c.
```
bool f() {
	std::cout << "Hello!" << std::endl;
	return true;
}
```
is not pure because it has a side effect. The original function would print 'Hello!' on each call, while the memoized version would print only once.

d.
```
int f(int x) {
	static int y = 0;
	y += x;
	return y;
}
```
is not pure because it's maintains state in the static variable ```y```, so it may return a different value for the same value passed as a parameter.

## 5. How many different functions are there from Bool to Bool? Can you implement them all?

There are 2 possible values for the parameter, and 2 possible values for the return value. This means there are 2^2 = 4 possible functions.

In C#:
```
Func<bool, bool> booleanTrue = _ => true;
Func<bool, bool> booleanFalse = _ => false;
Func<bool, bool> booleanIdentity = x => x;
Func<bool, bool> booleanNot = x => !x;
```

In Scheme:
```
(define (boolean-true _) #t)
(define (boolean-false _) #f)
(define (boolean-identity x) x)
(define (boolean-not x) (not x))
```

## 6. Draw a picture of a category whose only objects are the types ```Void```, ```()``` (unit), and ```Bool```; with arrows corresponding to all possible functions between these types. Label the arrows with the names of the functions.
![Alt text](https://g.gravizo.com/source/challenge6mark?https%3A%2F%2Fraw.githubusercontent.com%2Fnielsreijers%2Fcategorytheory%2Fmain%2FChapter2.md)
<details>
<summary></summary>
challenge6mark
digraph G {
    Bool -> Bool:nw [label="true"]
    Bool -> Bool:nw [label="false"]
    Bool -> Bool [label="not"]
    Bool -> Bool [label="id"]
    Void -> Void [label="id"]
    "()" -> "()" [label="id"]
    Void -> "()" [label="absurd"]
    Void -> Bool [label="absurd"]
    Bool -> "()" [label="unit"]
    "()" -> Bool [label="true"]
    "()" -> Bool [label="false"]
}
challenge6mark
</details>
