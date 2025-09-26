incr :: Int -> Int
incr x = x+1
triple :: Int -> Int
triple x = 3*x
welcome :: String -> String
welcome name = "Hello, " ++ name ++ "!"
count :: String -> String
count str = show (length str) ++ " characters."



leftHalf :: [Int] -> [Int]
leftHalf xs = take (length xs `div` 2) xs 

rightHalf :: [Int] -> [Int]
rightHalf xs = drop (length xs `div` 2) xs

second :: [Int] -> Int
second s = head (tail s)

lastlast :: [Int] -> Int
lastlast l = head (drop (length l - 1) l)


removelast :: [Int] -> [Int]
removelast i = take (length i - 1) i

pickmiddle :: [Int] -> Int
pickmiddle p = head (drop(length p `div` 2) p)

checkPalindrome :: String -> Bool
checkPalindrome cp = cp == reverse cp


checkTriangle :: Float -> Float -> Float -> Bool
checkTriangle a b c = (a < b + c) && (b < a + c) && (c < a + b)

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c 
    |checkTriangle a b c = 
        let s = (a + b + c) / 2
        in sqrt(s * ((s - a)*(s - b)*(s - c)))
    |otherwise = 0



