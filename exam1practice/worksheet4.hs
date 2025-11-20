calcPi1, calcPi2 :: Int -> Double


calcPi1 n = sum (take n terms)
    where 
        signs = cycle [1, -1]
        denominators = [1,3 ..]
        terms = zipWith (\s d -> fromIntegral s * 4 / fromIntegral d) signs denominators


calcPi2 n = 3 + sum (take n terms)
    where
        signs = cycle [1, -1]
        triples = [(2*i, 2*i+1, 2*i+2) | i <- [1..]]
        denominators = [fromIntegral (a*b*c) | (a,b,c) <- triples]
        terms = zipWith (\s d -> fromIntegral s * 4 / d) signs denominators



twinPrimes :: [(Integer, Integer)]
twinPrimes = [(p,q) | (p,q) <- zip primes(tail primes), q - p == 2]

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

hammingForSum :: Int -> [Integer]
hammingForSum n = [2^i * 3^j * 5^k | i <- [0..n], j <- [0..(n - i)], let k = n - i - j]

hamming :: [Integer]
hamming = concat [hammingForSum n | n <- [0..1]]

merge :: [Integer] -> [Integer] -> [Integer]
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge xs ys

myhamming :: [Integer]

myhamming = 1 :merge(map (2*) myhamming) 
                (merge (map (3*) myhamming) 
                     (map(5*) myhamming)) 


