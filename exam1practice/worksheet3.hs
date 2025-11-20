import Distribution.Simple.Utils (xargs)
import Data.Time.Format.ISO8601 (yearFormat)
myand :: [Bool] -> Bool
myand [] = True
myand (False:xs) = False
myand (_:xs) = myand xs

myor :: [Bool] -> Bool
myor [] = False
myor (True:xs) = True
myor (_:xs) = myor xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (xs:xss) = xs ++ myconcat xss

myreplicate :: Int -> a -> [a]
myreplicate 0 x = []
myreplicate n x = [x] ++ (myreplicate (n-1) x)

myindex :: [a] -> Int -> a
myindex (x:_) 0 = x
myindex (_:xs) n = myindex xs n

myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem k (x:xs) | k == x = True
                | otherwise = myelem k xs

leastDiv :: Integer -> Integer
leastDiv n = leastDivFrom 2
  where
    leastDivFrom d
      | d * d > n      = n       
      | n `mod` d == 0 = d       
      | otherwise      = leastDivFrom (d + 1)

isPrimeFast :: Integer -> Bool
isPrimeFast n = leastDiv n == n

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (removeAll x xs)
    where
        removeAll _ [] = []
        removeAll y (z:zs)
            | y == z = removeAll y zs
            | otherwise = z : removeAll y zs
    
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse n (x:xs) = x : n : intersperse n xs

insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) | n <= x = n : x : xs
                | otherwise = x : insert n xs



isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
    where 
        left = take (length xs `div` 2) xs
        right = drop (length xs `div` 2) xs 

toBits :: Int -> [Int]
toBits 0 = [0]
toBits 1 = [1]
toBits n = toBits (n `div` 2) ++ [n `mod` 2]


fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (x:xs) = x * 2^indexbit + fromBits xs
                where 
                    indexbit = length xs 

divisors :: Integer -> [Integer]
divisors n = filter (\d -> n `mod` d == 0) [1..n]

isPrimeFast2 :: Integer -> Bool 
isPrimeFast2 n = n> 1 &&
    all (\d -> n `mod` d /= 0) [2 .. limit]
    where 
        limit = floor (sqrt (fromIntegral n))

myappend :: [a] -> [a] -> [a]
myappend x y = foldr (:) y x

myconcat2 :: [[a]] -> [a]
myconcat2 [] = []
myconcat2 (x:xs) = foldr (:) (myconcat2 xs) x 

myreverse2 :: [a] -> [a]
myreverse2 [] = []
myreverse2 xs = foldr (\x acc -> acc ++ [x]) [] xs

myreverse3 :: [a] -> [a]
myreverse3 [] = []
myreverse3 x = foldl (flip(:)) [] x

myelem2 :: Eq a => a -> [a] -> Bool
myelem2 x xs = any (==x) xs


mygroup :: Eq a => [a] -> [[a]]
mygroup [] = [] 
mygroup (x:xs) = (x : takeWhile (== x) xs) : mygroup (dropWhile (==x) xs)

myFromBits :: [Int] -> Int
myFromBits = foldl (\acc bit -> acc * 2 + bit) 0

myIntercalate :: a -> [a] -> [[a]]
myIntercalate x [] = [[x]]
myIntercalate x (y:ys) = (x:y:ys) : map (y:) (myIntercalate x ys)



