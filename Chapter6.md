# Chapter 6 - Simple Algebraic Data Types

## 1. Show the isomorphism between ```Maybe a``` and ```Either () a```.

Given the following two mappings between ```Maybe a``` and ```Either ()```, it should be easy to see that ```f . g = id``` and ```g . f = id``` :
```
f :: Maybe a -> Either () a
f Nothing = Left ()
f Just x = Right x

g  :: Either () a -> Maybe a
g Left _ = Nothing
g Right x = Just x
```

## 2. Here's a sum type defined in Haskell ```data Shape = Circle Float | Rect Float Float```. When we want to define a function like ```area``` that acts on a ```Shape``` we do it by pattern matching on the two constructors. Implement ```Shape``` in C++ or Java as an interface and create two classes ```Circle``` and ```Rect```. Implement ```area``` as a virtual function. 
```
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect d h) = d * h
```

In C#:
```
public interface Shape {
    double area();
}

public class Circle : Shape {
    public double r;
    public double area() => Math.PI * this.r * this.r;
}
public class Rect : Shape {
    public double d, h;
    public double area() => this.d * this.h;
}
```


## 3. Continuing with the previous example: We can easily add a new function ```circ``` that calculates the circumference of a ```Shape```. We can do it without touching the definition of ```Shape```. Add a ```circ``` to your C++ or Java implementation. What parts of the original code did you have to touch?
```
circ :: Shape -> Float
circ (Circle r) = 2.0 * pi * r
circ (Rect d h) = 2 * (d + h)
```

In C#:
```
public interface Shape {
    double area();
    double circ();
}

public class Circle : Shape {
    public double r;
    public double area() => Math.PI * this.r * this.r;
    public double circ() => 2 * Math.PI * this.r;
}
public class Rect : Shape {
    public double d, h;
    public double area() => this.d * this.h;
    public double circ() => 2 * (this.d + this.h);
}
```

We need to update the interface, as well as both implementing classes. 

## 4. Continuing further: Add a new shape, ```Square```, to ```Shape``` and make all the necessary updates. What code did you have to touch in Haskell vs. C++ or Java? (Even if you're not a Haskell programmer, the modifications should be pretty obvious.)

In Haskell we need to modify the datatype ```Shape```, as well as both functions ```area``` and ```circ```:
```
data Shape = Circle Float | Rect Float Float | Square Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect d h) = d * h
area (Square e) = e * e

circ :: Shape -> Float
circ (Circle r) = 2.0 * pi * r
circ (Rect d h) = 2 * (d + h)
circ (Square e) = 4 * e
```

In C# we only need to add the new class with implementations for ```area``` and ```circ```:
```
public class Square : Shape {
    public double e;
    public double area() => this.e * this.e;
    public double circ() => 4 * this.e;
}
```

## 5. Show that a + a = 2 x a holds for types (up to isomorphism). Remember that 2 corresponds to Bool, according to our translation table.

The since the coproduct is a tagged union, it can store exactly twice as many values as ```a``` can: each value ```x``` as a ```Left x``` or ```Right x```.

The following two functions show the isomorphism between a + a (implemented as ```Either a a```) and 2 x a (implemented as ```(Bool, x)```) 

```
f :: Either a a -> (Bool, a)
f (Left x) = (True, x)
f (Right x) = (False, x)

g :: (Bool, a) -> Either a a
g (True, x) = (Left x)
g (False, x) = (Right x)
```
