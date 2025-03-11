module Lib
  (
    deck, goFish, updateFreqMap, makeFreqMap, sortHand, deal, randomNums,
    pair, twoPair, threeOfAKind, straight, flush, fullHouse, fourOfAKind, straightFlush, royalFlush, Suit (..), Value (..), Card (..), shuffleDeck, scoreHand, scoreHands
  ) where

--example implementation of all possible suits
data Suit = Club | Diamond | Heart | Spade
  deriving (Read, Show, Enum, Eq)

-- Students will finish an incomplete implementation in the handout
data Value = Two   | Three | Four | Five | Six
           | Seven | Eight | Nine | Ten  | Jack
           | Queen | King  | Ace
  deriving (Read, Show, Enum, Eq, Ord)

--Card data type
data Card = Card {value :: Value, suit :: Suit}
  deriving (Read, Show, Eq)

-- Make a 52 card deck using list comprehension
deck :: [Card]
deck = [Card v s | s <- [Club .. Spade], v <- [Two .. Ace]]

--random number generator (for decks of cards modulo 52 is reccomended)
randomNums :: Integral p => p -> p -> p -> [p]
randomNums mult m seed = aux seed
   where aux acc = acc:(aux ((mult * acc + 1) `mod` m))

--interleave: takes two lists and interleaves them together 
ileave :: [Card] -> [Card] -> [Card]
ileave xs []  = xs
ileave [] ys  = ys
ileave (x:xs) (y:ys) = x : y : ileave xs ys

ileave' xs ys = (foldr (\(x,y) acc -> x:y:acc) []) $ zip xs ys

ileave'' = (combine . ) . zip 
  where
    combine = foldr (\(x,y) acc -> x:y:acc) []

shuffleDeck :: [Card] -> [Int] -> [Card]
shuffleDeck deck [] = deck         
shuffleDeck deck (x:xs) = shuffleDeck newDeck xs
  where
    cutLoc = x `mod` 52
    (top, bot) = splitAt cutLoc deck
    -- (top, bot) = (take cutLoc deck, drop cutLoc deck)
    newDeck = ileave top bot

deal :: [a] -> Int -> [[a]]
deal deck n = dealRound deck 5
  where
    dealRound _ 0     = replicate n []
    dealRound cards k = zipWith (:) nextCards $ dealRound remCards (k-1)
      where
        nextCards = take n cards
        remCards  = drop n cards

-- Given a list of cards and a value, return all cards that have the value
goFish :: [Card] -> Value -> [Card]
goFish cards v = [c | c <- cards, value c == v]

-- goFish cards v = aux cards v []
--   where aux :: [Card] -> Value -> [Card] -> [Card]
--         aux [] _ acc = acc
--         aux (c:cs) val acc
--           | value c == val  = aux cs v (c:acc)
--           | otherwise       = aux cs v acc

-- goFish' :: [Card] -> Value -> [Card]
-- goFish' cards v = filter (\c -> value c == v) cards

-- This type is provided
type Hand = [Card]

-- Counter data type
data Counter t = Counter { item :: t, frequency :: Integer}
  deriving (Read, Show, Eq)
type FreqMap t = [Counter t]

updateFreqMap :: (Eq t) => t -> FreqMap t -> FreqMap t
updateFreqMap t [] = [Counter t 1]
updateFreqMap t (Counter i f : cs)
    | t == i    = Counter i (f+1) : cs
    | otherwise = Counter i f : updateFreqMap t cs

-- updateFreqMap t (c@(Counter i f) : cs)
--     | t == i    = Counter i (f+1) : cs
--     | otherwise = c : updateFreqMap t cs

--makeFreqMap returns a Counter of type t, mapping value to the number of times it occurs
makeFreqMap :: (Eq t) => [t] -> FreqMap t
makeFreqMap = foldl (flip updateFreqMap) []
-- makeFreqMap = foldr updateFreqMap []

-- Students implement sorting for the custom data structure
sortHand :: Hand -> Hand
sortHand [] = []
sortHand (c:cs) = (sortHand lt) ++ [c] ++ (sortHand gt)
  where
    lt = [x | x <- cs, value x <= value c]
    gt = [x | x <- cs, value x > value c]

-- sortHand [] = []
-- sortHand (h:t) = insertCard h (sortHand t)
--
-- insertCard c [] = [c]
-- insertCard c (h:t)
--     | value c <= value h = c:h:t
--     | otherwise = h: insertCard h t

-- helper functions for classifying hand types
valCounts = makeFreqMap . (map value)
suitCounts = makeFreqMap . (map suit)

-- Return true iff there is at least one pair of VALUES
pair :: Hand -> Bool
pair hand = foldr (\(Counter i f) b -> b || f == 2) False $ valCounts hand

-- Return true if a hand contains two distinct pairs
twoPair :: Hand -> Bool
twoPair hand = length handPairs == 2
  where
    handPairs = [i | (Counter i f) <- valCounts hand, f == 2]

-- Return true iff there is at least one triplet of VALUES
threeOfAKind :: Hand -> Bool
threeOfAKind hand = not $ null [i | (Counter i f) <- valCounts hand, f == 3]

-- Return true iff there is at least one quintuplet of VALUES
fourOfAKind :: Hand -> Bool
fourOfAKind hand = not $ null [i | (Counter i f) <- valCounts hand, f == 4]

-- Return true iff the cards form a sequence
-- Note that Ace is a high card: it cannot connect with One
-- [Ten, Jack, Queen, King, Ace] is a straight
-- [Ace, Two, Three, Four, Five] is NOT a straight for our purposes
straight :: Hand -> Bool
straight hand = and $ zipWith (\x y -> succ x == y) sVals (tail sVals)
  where
    sVals = map value $ sortHand hand

-- Return true if all cards in a hand are the same
-- Should NOT return true if the hand is empty!
flush :: Hand -> Bool
flush hand = not $ null [i | (Counter i f) <- suitCounts hand, f == 5]

-- Return true if there is a pair AND a three of a kind in the hand
fullHouse :: Hand -> Bool
fullHouse hand = threeOfAKind hand && pair hand

-- return True if all values are sequentiald and of the same ssuit
straightFlush :: Hand -> Bool
straightFlush hand = straight hand && flush hand

-- return True if all cards are of the same suit and sequential starting from Ten and ending with Ace
royalFlush :: Hand -> Bool
royalFlush hand = straightFlush hand && (value $ head $ sortHand hand) == Ten

--returns a ranking for a hand 
--1) Royal flush
--2) Straight Flush
--3) Four of a Kind
--4) Full House
--5) Flush
--6) Straight
--7) Three of a Kind
--8) Two Pair
--9) Pair
--10 Anything else
scoreHand :: [Card] -> Int
scoreHand hand
    | royalFlush hand    = 1
    | straightFlush hand = 2
    | fourOfAKind hand   = 3
    | fullHouse hand     = 4
    | flush hand         = 5
    | straight hand      = 6
    | threeOfAKind hand  = 7
    | twoPair hand       = 8
    | pair hand          = 9
    | otherwise          = 10

--Return a list of scores for each Hand in a list of Hands
scoreHands :: [[Card]] -> [Int]
scoreHands = map scoreHand

