f :: [()] -> Int
f [] = 0
f (_ : tail) = 1 + f tail

f' :: Int -> [()]
f' 0 = []
f' x = () : f' (x - 1)



main :: IO ()
main = do
    print ((f' . f) [])
    print ((f' . f) [()])
    print ((f' . f) [(), (), (), ()])
    print ((f . f') 0)
    print ((f . f') 1)
    print ((f . f') 4)
