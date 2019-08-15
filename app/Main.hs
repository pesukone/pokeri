module Main where

import Data.Random.Source.DevRandom
import Data.Random.RVar
import Data.Sequence
import Deck
import Hand
import Game
import Payoff

main :: IO ()
main = playGame 200.00

playGame money = do
    deck <- runRVar makeShuffledDeck DevRandom

    let game = mkGame $ Data.Sequence.take 8 deck

    parseInput game money

parseInput game money = do
    putStrLn ""
    putStrLn $ show game ++ "                          " ++ show money ++ "€"

    putStrLn "Pick [l]eft or [r]ight pile (quit with [q])"
    input <- getLine

    case input of
        "l" -> handleChoice (pickLeft game) money
        "r" -> handleChoice (pickRight game) money
        "q" -> putStrLn $ "Final sum: " ++ show money ++ "€"
        _   -> parseInput game money

handleChoice hand money = do
    let po = payoff hand
    print hand
    putStrLn $ "You won " ++ show po ++ "€"
    playGame $ money - 1.0 + po
