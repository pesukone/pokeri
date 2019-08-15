module Card where

import Rank
import Suit

data Card = Card Suit Rank | Joker


instance Show Card where
    show (Card s r) = case (s, r) of
        (Spade, Ace)     -> "🂡"
        (Spade, Two)     -> "🂢"
        (Spade, Three)   -> "🂣"
        (Spade, Four)    -> "🂤"
        (Spade, Five)    -> "🂥"
        (Spade, Six)     -> "🂦"
        (Spade, Seven)   -> "🂧"
        (Spade, Eight)   -> "🂨"
        (Spade, Nine)    -> "🂩"
        (Spade, Ten)     -> "🂪"
        (Spade, Jack)    -> "🂫"
        (Spade, Queen)   -> "🂭"
        (Spade, King)    -> "🂮"

        (Heart, Ace)     -> "🂱"
        (Heart, Two)     -> "🂲"
        (Heart, Three)   -> "🂳"
        (Heart, Four)    -> "🂴"
        (Heart, Five)    -> "🂵"
        (Heart, Six)     -> "🂶"
        (Heart, Seven)   -> "🂷"
        (Heart, Eight)   -> "🂸"
        (Heart, Nine)    -> "🂹"
        (Heart, Ten)     -> "🂺"
        (Heart, Jack)    -> "🂻"
        (Heart, Queen)   -> "🂽"
        (Heart, King)    -> "🂾"

        (Diamond, Ace)   -> "🃁"
        (Diamond, Two)   -> "🃂"
        (Diamond, Three) -> "🃃"
        (Diamond, Four)  -> "🃄"
        (Diamond, Five)  -> "🃅"
        (Diamond, Six)   -> "🃆"
        (Diamond, Seven) -> "🃇"
        (Diamond, Eight) -> "🃈"
        (Diamond, Nine)  -> "🃉"
        (Diamond, Ten)   -> "🃊"
        (Diamond, Jack)  -> "🃋"
        (Diamond, Queen) -> "🃍"
        (Diamond, King)  -> "🃎"

        (Club, Ace)      -> "🃑"
        (Club, Two)      -> "🃒"
        (Club, Three)    -> "🃓"
        (Club, Four)     -> "🃔"
        (Club, Five)     -> "🃕"
        (Club, Six)      -> "🃖"
        (Club, Seven)    -> "🃗"
        (Club, Eight)    -> "🃘"
        (Club, Nine)     -> "🃙"
        (Club, Ten)      -> "🃚"
        (Club, Jack)     -> "🃛"
        (Club, Queen)    -> "🃝"
        (Club, King)     -> "🃞"

instance Eq Card where
    Joker              == _            = True
    _                  == Joker        = True
    (Card s1 r1)       == (Card s2 r2) = s1 == s2 && r1 == r2

(@=) :: Card -> Card -> Bool
Joker       @= _           = True
_           @= Joker       = True
(Card s1 _) @= (Card s2 _) = s1 == s2

(#=) :: Card -> Card -> Bool
Joker       #= _           = True
_           #= Joker       = True
(Card _ r1) #= (Card _ r2) = r1 == r2

instance Ord Card where
    Joker       <= _           = False
    _           <= Joker       = True
    (Card _ r1) <= (Card _ r2) = r1 <= r2
