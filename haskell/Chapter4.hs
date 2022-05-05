data Optional a = InValid | Valid a
    deriving (Show)

return :: a -> Optional a
return = Valid

(>=>) :: (a -> Optional b) -> (b -> Optional c) -> (a -> Optional c)
(>=>) f g x = case f x of
                Valid y -> g y
                InValid -> InValid

safeRoot :: Double -> Optional Double
safeRoot x
    | x >= 0    = Valid (sqrt x)
    | otherwise = InValid

safeReciprocal :: Double -> Optional Double
safeReciprocal x
    | x /= 0    = Valid (1 / x)
    | otherwise = InValid

safeRootReciprocal :: Double -> Optional Double
safeRootReciprocal = safeReciprocal >=> safeRoot

main :: IO ()
main = do
    print (safeRootReciprocal 0.0)
    print (safeRootReciprocal (-1.0))
    print (safeRootReciprocal 100.0)
    