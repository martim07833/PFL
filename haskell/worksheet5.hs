-- TP5

-- 5.1

data List a = Empty | Cons a (List a)

toList :: [a] -> List a 

toList [] = Empty

toList (x:xs) = Cons x (toList xs)

fromList :: List a -> [a]

fromList Empty = []

fromList (Cons x xs) = x : fromList xs

-- 5.2)

data Suit = Clubs | Spades | Hearts | Diamonds
    deriving(Show, Eq, Ord, Enum)

data Face = TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT | NINE | TEN | J | Q | K | A
    deriving(Show, Eq, Ord, Enum)

data Card = Card Face Suit
    deriving(Show, Eq, Ord)

allCards :: [Card]

allCards = [Card f s | f <- [TWO .. A] , s <- [Clubs .. Diamonds]]

-- 5.3)
-- a)

cmp1 :: Card -> Card -> Ordering 

cmp1 (Card f1 s1) (Card f2 s2) =
    case compare s1 s2 of
        EQ -> compare f1 f2
        other -> other

-- b)

cmp2 :: Card -> Card -> Ordering 

cmp2 (Card f1 s1)(Card f2 s2) =
    case compare s1 s2 of
        EQ