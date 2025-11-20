import Distribution.FieldGrammar (List)
incr :: Int -> Int
incr x = x +1

triple :: Int -> Int
triple x = 3*x

welcome :: String -> String
welcome name = "Hello, " ++ name ++ "!"

count :: String -> String
count str = show (length str) ++ " characters."

leftHalf :: [Int] -> [Int]
leftHalf lista = take (length lista `div` 2) lista

rightHalf :: [Int] -> [Int]
rightHalf lista = drop (length lista `div` 2) lista

second :: [Int] -> Int
second s = head (tail s)

lastlast :: [Int] -> Int
lastlast l = head(take (length l - 1) l)

init :: [Int] -> [Int]
init i = take (length i - 1) i

middle :: [Int] -> Int
middle m = head (leftHalf m)

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



