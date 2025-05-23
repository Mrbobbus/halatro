module CourseworkOne where

import Halatro.Constants
import Halatro.Types
import Data.List ( sort, nub, subsequences, maximumBy )

import GHC.Base (TrName(TrNameD))
import GHC.Conc (ThreadStatus(ThreadBlocked))

--------------------------------------------------------------------------------
-- Part 1: check whether a played hand is a certain hand type

-- Checks if a hand contains cards, all of the same suit
suitChecker :: Hand -> Suit -> Bool
suitChecker [] _ = True
suitChecker (x:xs) s = suit x == s && suitChecker xs s

-- Counts the number of occurences of a specified rank in a hand 
rankCounter :: Hand -> Rank -> Int
rankCounter h r = length (filter (\c -> rank c == r) h)

-- Checks if there is a straight in a hand
straightChecker :: Hand -> Bool
straightChecker h =
    let ranks = sort (nub (map rank h))  -- Sort and remove duplicates to stop succ on Ace
        isConsecutive xs = and (zipWith (\a b -> succ a == b) xs (tail xs))
        aceLow = [Ace, Two, Three, Four, Five]
    in length ranks == 5 && (isConsecutive ranks || ranks == aceLow)


-- Checks if each card in a hand has a different rank
rankChecker :: Hand -> Bool
rankChecker [] = True
rankChecker [x] = True
rankChecker (x:x1:xs)
    | rank x1 /= rank x = rankChecker $ x1:xs
    | rank x1 == rank x = False
rankChecker _ = False

-- Checks if a hand has exactly 2 pairs
twoPairChecker :: Hand -> Bool
twoPairChecker h = length (filter (\r -> rankCounter h r >= 2) [Two .. Ace]) >= 2


contains :: Hand -> HandType -> Bool
contains a b
    | null a && b == None   = True
    | rankChecker a && not (any (suitChecker a) [Hearts, Spades, Diamonds, Clubs]) && b == HighCard    = True
    | any (\r -> rankCounter a r == 2) [Two .. Ace] && b == HighCard    = False
    | any (suitChecker a) [Hearts, Spades, Diamonds, Clubs] && b == Flush    = True
    | (any (\r -> rankCounter a r == 2) [Two .. Ace] || any (\r -> rankCounter a r == 3) [Two .. Ace]) && b == Pair = True
    | twoPairChecker a && b == TwoPair    = True
    | any (\r -> rankCounter a r == 3) [Two .. Ace] && b == ThreeOfAKind  = True
    | any (\r -> rankCounter a r == 4) [Two .. Ace] && b == FourOfAKind  = True
    | straightChecker a && b == Straight    = True
    | any (\r -> rankCounter a r == 2) [Two .. Ace] && any (\r -> rankCounter a r == 3) [Two .. Ace] && b == FullHouse  = True
    | any (suitChecker a) [Hearts, Spades, Diamonds, Clubs] && straightChecker a && b == StraightFlush    = True
    | (a == [Card Ten Hearts, Card Jack Hearts, Card Queen Hearts, Card King Hearts, Card Ace Hearts] || a == [Card Ten Spades, Card Jack Spades, Card Queen Spades, Card King Spades, Card Ace Spades] || a == [Card Ten Clubs, Card Jack Clubs, Card Queen Clubs, Card King Clubs, Card Ace Clubs] || a == [Card Ten Diamonds, Card Jack Diamonds, Card Queen Diamonds, Card King Diamonds, Card Ace Diamonds]) && b == RoyalFlush    = True
    | otherwise     = False


--------------------------------------------------------------------------------
-- Part 2: identify the highest value hand type in a played hand

bestHandType :: Hand -> HandType
bestHandType [] = None
bestHandType h
    | contains h RoyalFlush     = RoyalFlush
    | contains h StraightFlush  = StraightFlush
    | contains h FourOfAKind    = FourOfAKind
    | contains h FullHouse      = FullHouse
    | contains h Flush          = Flush
    | contains h Straight       = Straight
    | contains h ThreeOfAKind   = ThreeOfAKind
    | contains h TwoPair        = TwoPair
    | contains h Pair           = Pair
    | otherwise                 = HighCard

--------------------------------------------------------------------------------
-- Part 3: score a played hand

-- Takes cards of the same rank from a hand
rankRetriever :: Hand -> [Card]
rankRetriever [] = []
rankRetriever (x:xs) =
    let duplicates = filter (\c -> rank c == rank x) xs
    in if not (null duplicates)
        then x : duplicates ++ rankRetriever (filter (\c -> rank c /= rank x) xs)
        else rankRetriever xs

-- Takes the single highest card from a hand
highestCard :: Hand -> [Card]
highestCard [x] = [x]
highestCard (x:x1:xs) = highestCard ((if rank x >= rank x1 then x else x1) : xs)


whichCardsScore :: Hand -> [Card]
whichCardsScore h
    | bestHandType h == RoyalFlush      = h
    | bestHandType h == StraightFlush   = h
    | bestHandType h == FourOfAKind     = rankRetriever h
    | bestHandType h == FullHouse       = h
    | bestHandType h == Flush           = h
    | bestHandType h == Straight        = h
    | bestHandType h == ThreeOfAKind    = rankRetriever h
    | bestHandType h == TwoPair         = rankRetriever h
    | bestHandType h == Pair            = rankRetriever h
    | bestHandType h == HighCard        = highestCard h
    | otherwise                         = []

-- Takes base chips of a hand type
baseChips :: HandType -> Chips
baseChips h = fst (handTypeValues h)

-- Take multiplier of a hand type 
multiplier :: HandType -> Mult
multiplier h = snd (handTypeValues h)

-- Calculates the total hand score according to their rank
handRankScore :: Hand -> Int
handRankScore = foldr ((+) . rankScore . rank) 0

scoreHand :: Hand -> Int
scoreHand h
    | bestHandType h == RoyalFlush      = (baseChips RoyalFlush + handRankScore h) * multiplier RoyalFlush
    | bestHandType h == StraightFlush   = (baseChips StraightFlush + handRankScore h) * multiplier StraightFlush
    | bestHandType h == FourOfAKind     = (baseChips FourOfAKind + handRankScore (whichCardsScore h)) * multiplier FourOfAKind
    | bestHandType h == FullHouse       = (baseChips FullHouse + handRankScore h) * multiplier FullHouse
    | bestHandType h == Flush           = (baseChips Flush + handRankScore h) * multiplier Flush
    | bestHandType h == Straight        = (baseChips Straight + handRankScore h) * multiplier Straight
    | bestHandType h == ThreeOfAKind    = (baseChips ThreeOfAKind + handRankScore (whichCardsScore h)) * multiplier ThreeOfAKind
    | bestHandType h == TwoPair         = (baseChips TwoPair + handRankScore (whichCardsScore h)) * multiplier TwoPair
    | bestHandType h == Pair            = (baseChips Pair + handRankScore (whichCardsScore h)) * multiplier Pair
    | bestHandType h == HighCard        = (baseChips HighCard + handRankScore (whichCardsScore h)) * multiplier HighCard
    | otherwise                         = 0

--------------------------------------------------------------------------------
-- Part 4: find the highest scoring hand of 5 cards out of n>=5 cards

-- Generate all valid hands of exactly 5 cards (or fewer if less than 5 are given)
pHands :: [Card] -> [Hand]
pHands c = filter ((<= 5) . length) (subsequences c)


-- Find the highest scoring hand from a given set of cards
highestScoringHand :: [Card] -> Hand
highestScoringHand c
    | null c = []  -- No cards provided
    | otherwise = maximumBy (\a b -> compare (scoreHand a) (scoreHand b)) (pHands c)

--------------------------------------------------------------------------------
-- Part 5: implement an AI for maximising score across 3 hands and 3 discards

simpleAI :: [Move] -> [Card] -> Move
simpleAI _ b = Move Play (take 5 (reverse (sort b)))

sensibleAI :: [Move] -> [Card] -> Move
sensibleAI _ b = Move Play (highestScoringHand b) 



myAI :: [Move] -> [Card] -> Move
myAI = sensibleAI