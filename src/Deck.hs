module Deck where

import Data.Sequence
import Data.Random.Dovetail
import Data.Random.RVar
import Rank
import Suit
import Card

type Deck = Seq Card

makeDeck :: Deck
makeDeck = fromList [ Card s r | s <- [Spade .. Diamond], r <- [Two .. Ace] ]

makeShuffledDeck :: RVar Deck
makeShuffledDeck = dovetails 20 makeDeck
