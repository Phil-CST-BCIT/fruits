module Lib
    ( 
      letterToChar
    , formatGrid
    , printGrid
    , searchFruitInLine
    , findFruit
    , searchFruitInLetterLinePrefix
    , bend
    , createGridOfPairs
    , zipTwoGrids
    , createGridWithCoord
    , Letter(Letter, Empty)
    , Game(gameGrid, gameNames)
    , createGame
    , totalNames
    , score
    , playGame
    , formatGame
    , foundAll
    , makeRandomGrid
    , fillBlanks
    ) where

import Data.List (transpose)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Map as M
import System.Random ( Random(randomRs), RandomGen(split) )

-- ***********************************************************************
--                            DATA TYPE DIFINITIONS
-- ***********************************************************************

-- a user defined data type
-- 
-- a Letter is essentially a wrapper for an ascii letter in the gird.
-- it consists of a pair of coordinates that designate the position of a letter in a gird
-- it also contains the letter itself
-- if a Letter doesn't have anything, we call it Empty
data Letter = Letter (Integer, Integer) Char 
            | Empty
            deriving (Eq, Ord, Show)

-- a user defined data type
-- a grid can contain any data type a
type Grid a = [[a]]

-- creates a tuple containning a fruit name 
makeTup :: a1 -> (a1, Maybe a2)
makeTup name = (name, Nothing)

-- a Game data type with constructor Game(Grid of Letter)(Data.Map)
-- a Game contains a grid of Letter and a map.
-- the strings in fruits are the keys, and each key corresponds to a series of Letters
-- if a fruit name in the game is found, the game returns a list of Letters
-- the list of Letters is essentially characters that represent the name of a fruit
data Game = Game {
                gameGrid :: Grid Letter,
                gameNames :: M.Map String (Maybe [Letter])
            } deriving Show


-- ***********************************************************************
--                                 GAME LOGIC
-- ***********************************************************************

-- creates a game
-- it takes a gird of char and a list of names and returns a game
createGame :: Grid Char -> [String] -> Game
createGame grid names = 
    let coordGrid = createGridWithCoord grid
        l = map makeTup names
        dict = M.fromList l
    in Game coordGrid dict

-- returns the number of fruit names in a game
totalNames :: Game -> Int
totalNames game = length (M.keys (gameNames game))

-- returns the current score of a game
score :: Game -> Int
score game = length (catMaybes (M.elems (gameNames game)))

-- a helper function to determine if a player has found all names
foundAll :: Game -> Bool
foundAll game = 
    if score game == totalNames game then True
    else False

-- perfomrs the game logic
playGame :: Game -> String -> Game
playGame game name | not (M.member name (gameNames game)) = game
playGame game name = 
    let grid = gameGrid game 
        foundName = findFruit grid name
        in case foundName of 
            Nothing -> game
            Just lst -> 
                let dict = gameNames game -- indentation matters
                    newDict = M.insert name foundName dict
                in Game grid newDict

-- print out a game in a fromatted way
formatGame :: Game -> String
formatGame game = formatGameGrid game
                ++ "\n\n\n"
                ++ "Your score: "
                ++ (show (score game))
                ++ "/"
                ++ (show (totalNames game))

-- generates some randome letters from 'A' to 'Z'
makeRandomGrid :: RandomGen t => t -> [[Char]]
makeRandomGrid generator = 
    let (g1, g2) = split generator
        row = randomRs ('A', 'Z') g1
    in  row : makeRandomGrid g2

-- fill all '_' with a random letter from 'A' to 'Z'
fillBlanks :: RandomGen t => t -> Grid Char -> Grid Char
fillBlanks gen grid = 
    let r = makeRandomGrid gen 
        fill '_' r = r
        fill c _ = c
    in zipTwoGrids fill grid r

-- formats a game in order to print out nicely
formatGameGrid :: Game -> String
formatGameGrid game = 
    let grid = gameGrid game 
        dict = gameNames game :: M.Map String (Maybe [Letter])
        letterSet = concat (catMaybes (M.elems dict))
        formatLetter letter =
            let char = letterToChar letter 
            in if letter `elem` letterSet then '*' else char
        charGrid = doubleMap formatLetter grid
    in unlines charGrid

-- ***********************************************************************
--                                 FORMATTING GRID
-- ***********************************************************************

-- unwraps a Letter to a char
-- the function has two cases
-- if there is a ascii char we just return the char
-- if the Letter is Empty we return underscore
letterToChar :: Letter -> Char
letterToChar (Letter _ c) = c
letterToChar Empty = '_'

-- applies map function twice to a grid in order to converts 
-- one kind of grid a into another kind of grid b
doubleMap :: (a -> b) -> Grid a -> Grid b
doubleMap f g = map (map f) g

-- appends a new line at the end of each grid line
-- returns a formatted grid
-- a grid is a list of list of Letter, but we need to convert it to a list of list of chars 
-- we apply map twice on the grid of Letter to convert it to a list of list of chars 
-- then we use unlines to append a '\n' at the end of each string.
formatGrid :: Grid Letter -> String
formatGrid grid = 
    let charGrid = doubleMap letterToChar grid in
    unlines charGrid

-- prints a grid of Letter to the terminal screen
printGrid :: Grid Letter -> IO ()
printGrid grid = putStrLn (formatGrid grid)


-- ***********************************************************************
--                                 TRANSFORMING
-- ***********************************************************************

-- indent the original grid and 
diagnolize :: Grid Letter -> Grid Letter
diagnolize grid = transpose (bend grid)

-- add indent to each line
indent :: [Letter] -> [Letter]
indent line = Empty : line

-- a helper function to add indentation to lines except the first line
-- we do this because we want to search diagonalized direction
-- For example:
-- the original grid looks like this
-- AAAAAAAAA
-- BBBBBBBBB
-- CCCCCCCCC
-- after bend
-- AAAAAAAAA
--  BBBBBBBBB
--   CCCCCCCCC
bend :: Grid Letter -> Grid Letter
bend [] = []
bend (hd:tl) = hd : bend (map indent tl)

-- ***********************************************************************
--                                 SEARCHING
-- ***********************************************************************

-- returns a concatenated grid of Letter 
getLines :: Grid Letter -> [[Letter]]
getLines grid = 
    let horizontal = grid -- the original grid
        vertical = transpose grid -- rotate the original grid -90 degree 
        diagonalPositive = diagnolize grid
        diagonalNegative = diagnolize (map reverse grid)
        lines = horizontal ++ vertical ++ diagonalPositive ++ diagonalNegative in
    lines ++ (map reverse lines)

-- a helper function to findFruit
-- recursivly searches a fruit name line by line
-- if the name exists in the current line, returns the option type of a list of Letter
-- otherwise returns nothing
searchFruitInLine :: String -> [Letter] -> Maybe [Letter] --
searchFruitInLine _ [] = Nothing -- exhausted
searchFruitInLine name line = 
    let found = searchFruitInLetterLinePrefix [] name line  in
        case found of 
            Nothing -> searchFruitInLine name (tail line) -- if it doesn't exist
            cs@(Just _) -> cs -- at pattern, cs is the content of Just

-- finds out if a fruit name exists in a grid
-- this is the core function of the program 
-- The function uses the library map function to match agaist each line of the grid
-- @param grid: a list of list of Letter
-- @param name: the name of a fruit
-- @return: a list of Maybes that represents the name of a fruit
findFruit :: Grid Letter -> String -> Maybe [Letter]
findFruit grid name = 
    let lines = getLines grid 
        found = map (searchFruitInLine name) lines
    in listToMaybe(catMaybes found)

-- a helper function to function searchFruitInLine
-- Because we don't know where the fruitName is located in a string.
-- we break up the string and the list of letter, and then we recursively 
-- match against an ascii char and a Letter
-- @param: acc accumulator
-- @param: a list of chars, a fruit name. we break up the string into pieces
-- @param: (l : ls) Letters, a list of Letters. we break up the list of Letters into individual Letter
-- guarded by ch == letterToChar
searchFruitInLetterLinePrefix :: [Letter] -> String -> [Letter] ->   Maybe [Letter]
searchFruitInLetterLinePrefix acc (ch : chs) (l : ls) 
    | ch == letterToChar l -- if an ascii char matches a Letter, we add the char to a list
    = searchFruitInLetterLinePrefix (l : acc) chs ls 
searchFruitInLetterLinePrefix acc [] _  = Just (reverse acc) -- if all chars match
searchFruitInLetterLinePrefix _ _ _ = Nothing -- chars and Letters don't match


-- ***********************************************************************
--                                 GRIDS
-- ***********************************************************************

-- a helper function to intPariGrid
-- returns a grid of pairs
createGridOfPairs :: Grid a -> Grid b -> Grid(a, b)
createGridOfPairs rows cols = zipWith zip rows cols

-- a helper function to createGridWithCoord
zipTwoGrids :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipTwoGrids f a b = (zipWith (zipWith f)) a b

-- creates a grid of a pair of integers
intPairGrid :: Grid(Integer, Integer)
intPairGrid = 
    let rows = map repeat [0..]
        cols = repeat [0..]
    in createGridOfPairs rows cols

-- creates a gird with coordinates
-- originally, a grid was simply a list of chars, there was no integer coordinates associated with each letter
-- however, we want to fill the blank spots with some randome letters, thus, we need to know the position
-- of each letter
createGridWithCoord :: Grid Char -> Grid Letter
createGridWithCoord grid = zipTwoGrids Letter intPairGrid grid

