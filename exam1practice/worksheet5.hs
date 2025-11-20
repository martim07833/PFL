data List a = Empty | Cons a (List a)

toList :: [a] -> List a 

fromList :: List a -> [a]

toList [] = Empty

toList (x : xs) = Cons x (toList xs)

fromList Empty = []

fromList (Cons x xs) = x : fromList xs

data Suit = Clubs | Spades | Hearts | Diamonds 
    deriving (Show, Eq, Ord, Enum)


data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K | A
    deriving (Show, Eq, Ord, Enum)

data Card = Card Face Suit
    deriving (Show, Eq, Ord)


allCards :: [Card]

allCards = [Card f s | f <- [Two .. A], s <- [Clubs .. Diamonds]]


data Ordering = LT | EQ | GT 
    deriving (Show, Eq, Ord)

cmp1 :: Card -> Card -> Ordering

