module Payoff where

import Hand

payoff :: Hand -> Double
payoff hand
    | hand `is` fives         = 100.00
    | hand `is` royalFlush    = 100.00
    | hand `is` straightFlush = 75.00
    | hand `is` fours         = 50.00
    | hand `is` fullHouse     = 20.00
    | hand `is` flush         = 15.00
    | hand `is` straight      = 11.00
    | hand `is` threes        = 5.00
    | hand `is` twoPairs      = 3.00
    | hand `is` pair          = 2.00
    | otherwise               = 0.00
