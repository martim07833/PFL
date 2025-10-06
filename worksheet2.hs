-- 2.1

classify :: Int -> String

classify x =
    if x <= 9 then "failed"
        else if x <= 12 then "passed"
            else if x <= 15 then "good"
                else if x <= 18 then "very good"
                    else "excellent" 

classifyBMI :: Float -> Float -> String

-- 2.2

classifyBMI weight height =
    let bmi = (weight / (height^2)) 
    in 
        if bmi < 18.5 then "underweight"
        else if bmi < 25 then "normal weight"
        else if bmi < 30 then "overweight"
        else "obese"

-- 2.3 a)

max3 :: Ord a => a -> a -> a -> a
max3 x y z | (x >= y && x >= z) = x
           | (y >= x && y >= z) = y
           | otherwise = z

min3 :: Ord a => a -> a -> a -> a
min3 x y z | (x <= y && x <= z) = x
           | (y <= x && y <= z) = y
           | otherwise = z
        
-- 2.3 b)

max3b :: Ord a => a -> a -> a -> a
max3b x y z = max x (max y z)


min3b :: Ord a => a -> a -> a -> a
min3b x y z = min x (min y z)

-- 2.4
xor :: Bool -> Bool -> Bool

xor True True = False
xor False True = True
xor True False = True
xor False False = False

-- 2.5

safetail :: [a] -> [a]

safetail a = if null a then []
             else tail a

--safetail a | null a = []
        -- | otherwise = tail a

--safetail [] = []
--safetail a = tail a

-- 2.6

short :: [a] -> Bool

{-short s =
        let size = length s
        in 
            if size >= 3 then False
            else True-}

short [] = True
short [_] = True
short [_,_] = True
short _ = False
--short (_:_:_:_) = False

-- 2.7 a) TPC

-- 2.7 b)

median :: (Ord a, Num a) => a -> a -> a -> a


median x y z = x + y + z - (max3 x y z) - (min3 x y z)

-- 2.8

propDivs :: Integer -> [Integer]

propDivs n = [d | d <- [1 ..n-1], n`mod`d == 0]

-- 2.9

perfects :: Integer -> [Integer]

perfects p = [n | n <- [1 ..p-1], sum(propDivs n) == n] 

-- 2.10

pyths :: Integer -> [(Int ..., Int ..., Int ...)]

