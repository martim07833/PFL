-- TP3

-- 1. a)

anda :: [Bool] -> Bool
anda [] = True
anda (False:_) = False
anda (_:xs) = anda xs

-- b)

orb :: [Bool] -> Bool
orb [] = False
orb (True :_) = True
orb (_:xs) = orb xs

-- c)

myconcat :: [[a]] -> [a]

myconcat [] = []
myconcat (xs:xss) = xs ++ myconcat xss

-- d)

myreplicate :: Int -> a -> [a]
myreplicate 0 xs = []
myreplicate n xs = [xs] ++ myreplicate (n-1) xs



-- e)

myindex :: [a] -> Int -> a
myindex (x:_)  0 = x
myindex (_:xs) n = myindex xs (n - 1)


-- f)

myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem k (x:xs) | k == x = True
                | otherwise = myelem k xs

-- 3.2 a)

leastDiv :: Integer -> Integer
leastDiv n = findDiv 2
  where
    findDiv d | d * d > n = n     
      | n `mod` d == 0 = d  
      | otherwise       = findDiv (d + 1)