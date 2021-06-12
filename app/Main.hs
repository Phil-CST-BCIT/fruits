module Main where

import Lib
import Data
import System.IO
import System.Random

--  Then entry point where the game is initialized
main :: IO ()
main = do 
    generator <- newStdGen -- generates a random generator seed
    let filledInGrid = fillBlanks generator grid -- fills out all blank spots
        myGame = createGame filledInGrid fruits -- creates a game
    hSetBuffering stdout NoBuffering -- solves the terminal printing format problem and I don't exatly know why
    playGameLoop myGame -- starts the game

-- starts the game
-- An IO monad
playGameLoop :: Game -> IO ()
playGameLoop game = do -- do notation that wraps the IO monad
    putStrLn "\n=== === === ==="
    putStrLn "* Ten Fruits *"
    putStrLn "=== === === ===\n"
    putStrLn (formatGame game)
    putStr "Please enter a fruit name in uppercase: "
    fruit <- getLine -- reads user input from the stdin
    let newGame = playGame game fruit -- call the primary logic in Lib
    if foundAll newGame then -- if all fruits are discovered
        putStrLn "All fruits have been found"
    else
        playGameLoop newGame -- loop