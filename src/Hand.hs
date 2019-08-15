module Hand where

import Data.List
import Rank
import Suit
import Card

data Hand = Hand Card Card Card Card Card

instance Eq Hand where
    (Hand a1 b1 c1 d1 e1) == (Hand a2 b2 c2 d2 e2) =
        sort [a1, b1, c1, d1, e1] == sort [a2, b2, c2, d2, e2]

instance Show Hand where
    show (Hand a b c d e) = show a ++ " " ++
                            show b ++ " " ++
                            show c ++ " " ++
                            show d ++ " " ++
                            show e

pair :: [Card] -> Bool
pair [Card _ r1, Card _ r2, _, _, _] = r1 >= Ten && r2 >= Ten && r1 == r2

twoPairs :: [Card] -> Bool
twoPairs [c1, c2, c3, c4, _] = c1 #= c2 && c3 #= c4

threes :: [Card] -> Bool
threes [c1, c2, c3, _, _] = c1 #= c2 && c2 #= c3

straight :: [Card] -> Bool
straight [Card _ Ace, Card _ Two, Card _ Three, Card _ Four, Card _ Five] = True
straight [Card _ r1, Card _ r2, Card _ r3, Card _ r4, Card _ r5] =
    r2 `isSuccOf` r1 &&
    r3 `isSuccOf` r2 &&
    r4 `isSuccOf` r3 &&
    r5 `isSuccOf` r4

isSuccOf :: (Enum a, Bounded a, Eq a) => a -> a -> Bool
a2 `isSuccOf` a1 = (a1 /= maxBound) && (a2 == succ a1)

flush :: [Card] -> Bool
flush [c1, c2, c3, c4, c5] = c1 @= c2 && c2 @= c3 && c3 @= c4 && c4 @= c5

fullHouse :: [Card] -> Bool
fullHouse [c1, c2, c3, c4, c5] = c1 #= c2 && c3 #= c4 && c4 #= c5

fours :: [Card] -> Bool
fours [c1, c2, c3, c4, _] = c1 #= c2 && c2 #= c3 && c3 #= c4

straightFlush :: [Card] -> Bool
straightFlush h = straight h && flush h

royalFlush :: [Card] -> Bool
royalFlush h@[Card s1 Ten, Card s2 Jack, Card s3 Queen, Card s4 King, Card s5 Ace] = flush h
royalFlush _ = False

fives :: [Card] -> Bool
fives [c1, c2, c3, c4, c5] = c1 #= c2 && c2 #= c3 && c3 #= c4 && c4 #= c5

is :: Hand -> ([Card] -> Bool) -> Bool
(Hand c1 c2 c3 c4 c5) `is` pred = any pred $ permutations [c1, c2, c3, c4, c5]
