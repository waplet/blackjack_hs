import Blackjack.Helpers
import Blackjack.Game

main = gameStart

-- Main cycle
gameStart = do
    putStrLn "Enter amount"
    m <- getLine
    if quit m
        then putStrLn "Bye!"
        else do
            if isNumber m
                then game 1 $ read m
                else do
                    putStrLn $ putStrLn "Wrong input!"
                    gameStart
