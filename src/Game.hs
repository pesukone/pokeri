module Game where

import Data.Foldable
import Data.Sequence
import Card
import Pile
import Hand

data Game = Game Card Card Pile Pile

instance Show Game where
    show (Game c1 c2 p1 p2) = show c1 ++ " " ++
                              show c2 ++ " " ++
                              show p1 ++ " " ++
                              show p2

mkGame :: Seq Card -> Game
mkGame s = Game c1 c2 (Pile c3 (c4, c5)) (Pile c6 (c7, c8))
    where [c1, c2, c3, c4, c5, c6, c7, c8] = toList s

pickLeft :: Game -> Hand
pickLeft (Game c1 c2 (Pile c3 (c4, c5)) _) = Hand c1 c2 c3 c4 c5

pickRight :: Game -> Hand
pickRight (Game c1 c2 _ (Pile c3 (c4, c5))) = Hand c1 c2 c3 c4 c5
