-- TP5

-- 5.1

data List a = Empty | Cons a (List a)

toList :: [a] -> List a 

toList [] = Empty

toList (x:xs) = Cons x (toList xs)

fromList :: List a -> [a]

fromList Empty = []

fromList (const x xs) = x : fromList xs

