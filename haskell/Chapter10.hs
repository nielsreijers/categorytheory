import Data.Functor.Contravariant (Contravariant (contramap))
import System.Win32 (COORD(x))

alpha1 :: Maybe a -> [a]
alpha1 Nothing = []
alpha1 (Just x) = [x]

newtype Reader e a = Reader (e -> a)
instance Functor (Reader e) where
    fmap f (Reader g) = Reader (f . g)

alpha2_1 :: Reader () a -> [a]
alpha2_1 _ = []

alpha2_2 :: Reader () a -> [a]
alpha2_2 (Reader x) = [x ()]

alpha2_3 :: Reader () a -> [a]
alpha2_3 (Reader x) = [x (), x ()]


alpha3 :: Reader Bool a -> Maybe a
alpha3 _ = Nothing

alpha3_T :: Reader Bool a -> Maybe a
alpha3_T (Reader x) = Just (x True)

alpha3_F :: Reader Bool a -> Maybe a
alpha3_F (Reader x) = Just (x True)

newtype Op e a = Op (a -> e)
instance Contravariant (Op e) where
  contramap f (Op g) = Op (g . f)


op :: Op Bool Int
op = Op (0 <)
f :: String -> Int
f x = read x
alpha6 :: (Op Bool a) -> (Op String a)
alpha6 (Op x) = Op (show . x)

op' :: Op Int String
op' = Op length
f' :: Int -> String
f' = show
alpha6' :: (Op Int a) -> (Op Double a)
alpha6' (Op x) = Op (sqrt . fromIntegral . x)


main :: IO ()
main = do
    -- alphaMap :: String -> String
    let (Op alphaMap) = (alpha6 . contramap f) op
    let (Op mapAlpha) = (contramap f . alpha6) op
    print (alphaMap "1")
    print (mapAlpha "1")
    print (alphaMap "-42")
    print (mapAlpha "-42")
    -- alphaMap :: Int -> Double
    let (Op alphaMap') = (alpha6' . contramap f') op'
    let (Op mapAlpha') = (contramap f' . alpha6') op'
    print (alphaMap' 1)
    print (mapAlpha' 1)
    print (alphaMap' 42)
    print (mapAlpha' 42)
