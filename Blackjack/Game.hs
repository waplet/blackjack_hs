module Blackjack.Game where

import Data.List
import Data.Tuple.Utils
import System.Random
import Blackjack.Helpers

-- Cards
cards = [1..13]

-- index -> naming -> possible values
mapping = [
        (1, "A", [1,10]),
        (11, "J", [10]),
        (12, "Q", [10]),
        (13, "K", [10])
    ]

end s = putStrLn $ "Game ended: " ++ s

quit s = any ('isPrefixOf` s) [":q", "q", "exit"]

game betSize money
    | money <= 0        = end $ redLine "You lost!"
    | betSize > money   = do
        putStrLn $ pinkLine $ "Money too low, bet value changed to " ++ (show money) ++ "!"
        game money money
    | otherwise         = do
        putStrLn $ pinkLine $ "Money: " ++ (show money) ++ ". Bet value: " ++ (show betSize)
        putStrLn "Enter your bet (to change amount of bet enter 'change <number>')"
        input <- getLine
        processInput betSize input money

processInput betSize input money
    | quit input    = end "quit."
    -- | change        = changeValue
    | simpleBet     = generateNumber
    | otherwise     = wrongInput
    where
        wrongInput = do
            putStrLn $ redLine "Wrong input!"
            game betSize money

        simpleBet = inList input mapping

generateNumber betSize money winners quotient = do
    gen <- fmap (`mod` 13 + 1) randomIO

    