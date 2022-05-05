# Chapter 4 - Kleisli Categories

# 1. Construct the Kleisli category for partial functions (define composition and identity).

Since I'm not familiar with C++, I will implement ```Optional``` in Haskell.

The Haskell standard library already contains the ```Maybe``` type for this purpose, which should work in the same way. 

```
data Optional a = InValid | Valid a
    deriving (Show)

return :: a -> Optional a
return = Valid

(>=>) :: (a -> Optional b) -> (b -> Optional c) -> (a -> Optional c)
(>=>) f g x = case f x of
                Valid y -> g y
                InValid -> InValid
```

# 2. Implement the embellished function ```safe_reciprocal``` that returns a valid reciprocal of its argument, if it's different from zero.

```
safeRoot :: Double -> Optional Double
safeRoot x
    | x >= 0    = Valid (sqrt x)
    | otherwise = InValid

safeReciprocal :: Double -> Optional Double
safeReciprocal x
    | x /= 0    = Valid (1 / x)
    | otherwise = InValid
```

# 3. Compose ```safe_root``` and ```safe_reciprocal``` to implement ```safe_root_reciprocal``` that calculates ```sqrt(1/x)``` whenever possible.

```
safeRootReciprocal :: Double -> Optional Double
safeRootReciprocal = safeReciprocal >=> safeRoot

main :: IO ()
main = do
    print (safeRootReciprocal 0.0)
    print (safeRootReciprocal (-1.0))
    print (safeRootReciprocal 100.0)
```

This will print:
```
InValid
InValid
Valid 0.1
```