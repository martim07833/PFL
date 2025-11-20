import Distribution.Simple.Utils (xargs)
classify :: Int -> String
classify grade
  | grade <= 9 = "failed"
  | grade <= 12 = "passed"
  | grade <= 15 = "good"
  | grade <= 18 = "very good"
  | otherwise = "excellent"

classifyBMI :: Float -> Float -> String 
classifyBMI weight height
  | bmi <= 18.5 = "underweight"
  | bmi <= 25 = "normal weight"
  | bmi <= 30 = "overweight"
  | otherwise = "obese"
  where
    bmi = weight / (height^2)

myMax :: Ord a => a -> a -> a
myMax x y = if x >= y then x 
    else y

myMin :: Ord a => a -> a -> a
myMin x y = if x <= y then x
    else y

max3 :: Ord a => a -> a -> a -> a
{-max3 x y z 
    | (x >= y) && (x >= z) = x
    | (y >= z) && (y >= x) = y
    | (z >= x) && (z >= y) = z-}
max3 x y z 
    | (max x y == x) && (max x z == x) = x
    | (max x y == y) && (max y z == y) = y
    | (max y z == z) && (max x z == z) = z


min3 :: Ord a => a -> a -> a -> a
min3 x y z 
    | (min x y == x) && (min x z == x) = x
    | (min x y == y) && (min y z == y) = y
    | (min x z == z) && (min y z == z) = z

{-min3 x y z 
    | (x <= y) && (x <= z) = x
    | (y <= z) && (y <= x) = y
    | (z <= x) && (z <= y) = z
    -}

myXOR :: Bool -> Bool -> Bool

myXOR True False = True
myXOR False True = True
myXOR False False = False
myXOR True True = True

safetail :: [a] -> [a]

safetail a = if null a then []
             else tail a

short :: [a] -> Bool
{-short a = if size >= 3 then False
        else True
        where size = length a-}

short [] = True
short [_] = True
short [_,_] = True
short _ = False

median :: (Ord a, Num a) => a -> a -> a -> a

{-median x y z | (x >= y && x <= z) || (x >= z && x <= y) = x
             | (y >= x && y <= z) || (y <= x && y >= z) = y
             | otherwise = z-}

median x y z = x + y + z - (max3 x y z) - (min3 x y z)

propDivs :: Integer -> [Integer]

propDivs n = [d | d <- [1 .. n-1], n`mod`d == 0]

sumAllDivs :: [Integer] -> Integer
sumAllDivs [] = 0
sumAllDivs (x : xs) = x + sumAllDivs xs

perfects :: Integer -> [Integer]

perfects p = [n | n <- [1 .. p - 1], n == sumAllDivs(propDivs n)]

checkTriple :: Integer -> Integer -> Integer -> Bool
checkTriple x y z = x^2 + y^2 == z^2


pyths :: Integer -> [(Integer, Integer, Integer)]
pyths a = [(x,y,z) | x <- [1 .. a], y <- [1 .. a], z <- [1 .. a], checkTriple x y z]


isPrime :: Integer -> Bool
isPrime a = if sumAllDivs (propDivs a) == 1 then True
            else False


myconcat :: [[a]] -> [a]
myconcat xss = [x | xs <- xss, x <- xs]

myreplicate :: Int -> a -> [a]
myreplicate n a = [a | _ <- [1..n]]

myindex :: [a] -> Int -> a
myindex xs n = head [x | (i, x) <- zip [0..] xs , i == n]

factorial :: (Eq a, Num a, Enum a) => a -> a
factorial n = product [1..n]

binom :: Integer -> Integer -> Integer
binom n k = factorial n `div` (factorial k * factorial (n - k))

pascal :: Integer -> [[Integer]]

pascal n = [ [ binom r k | k <- [0..r] ] | r <- [0..n] ]
