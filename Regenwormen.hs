------------------------------------------------------------------
-- Functioneel programmeren
-- Opdracht 2: Regenwormen
--
-- Student: Joshua Van Synghel
-- Nummer: 852078066
--
------------------------------------------------------------------
module Regenwormen where

import Data.Char
import System.IO()
import System.Random

newtype Symbol = Dice Int
    deriving (Eq, Ord, Read)

instance Show Symbol where
    show (Dice 6) = "W"
    show (Dice s) = show s

type SavedDice = [Symbol] -- gs: grabbed dice
type RolledDice = [Symbol] -- rs: rolled dice
data Tactic = Tactic (SavedDice -> RolledDice -> IO Symbol)   -- Grab tactic
                        (SavedDice -> IO Bool)                -- Continue tactic

type Tile = (Int, Int)                 -- (Value, Number of worms)
type PlayerTiles = [Tile]
type GameTiles = [Tile]

type Player = (String, Tactic, [Tile]) -- (Name, Tactic, List of acquired tiles)

tacticGrab:: Tactic -> SavedDice -> RolledDice -> IO Symbol
tacticGrab (Tactic g _) = g

tacticContinue :: Tactic -> SavedDice -> IO Bool
tacticContinue (Tactic _ c) = c

-- getChar generated errors due to function reading '\n' as char
getChar' :: IO Char
getChar' = do 
    c <- getChar
    if c == '\n' then
        getChar'
    else
        return c

getDigit :: IO Char
getDigit = do 
    n <- getChar'
    if isDigit n then
        return n
    else do
        putStr "Please enter a valid digit\n"
        getDigit

main :: IO ()
main = do
    putStrLn "Welcome to the game of rainworms\n\nPlease state your name:"
    name <- getLine
    putStrLn "Against how many computer player do you want to play?"
    n <- getDigit
    endResultPlayers <- startGame (digitToInt n) name
    putStrLn "These are the final results:"
    outputAllTiles endResultPlayers []
    outputWinner endResultPlayers    

startGame :: Int -> String -> IO [Player]
startGame n name = do
    ps <- generatePlayers n name
    ts <- generateTiles
    playGame ts ps

playGame :: GameTiles -> [Player] -> IO [Player]
playGame [] ps     = return ps -- all gametiles have been taken or removed
playGame (_:_) []  = error "No players to start the game"
playGame ts (p:ps) =  do
    putStrLn ("\nNext turn: player " ++ getPlayerName p)
    outputAllTiles (p:ps) ts
    savedDices <- turn (>= smallestTile (ts ++ getAllOpenPlayerTiles (p:ps)))
                        (getPlayerTactic p) [] 
    dicesToTile <- returnTile (convertTurnScoreToTile (calculateTotalScore savedDices))
    -- case when tile is element of game tiles
    if tileElementOfGameTiles dicesToTile ts then do
        ts' <- removeSpecificTile dicesToTile ts
        p' <- addTileToPlayerTiles dicesToTile p
        playGame ts' (roundRobinPlayers (p':ps))
    -- case when tile is element of open player tiles
    else if tileElementOfOpenPlayerTiles dicesToTile ps then do
        ps' <- returnPlayersAfterRemovingSpecificOpenTile dicesToTile ps
        p' <- addTileToPlayerTiles dicesToTile p
        playGame ts (roundRobinPlayers (p':ps'))
    -- case when invalid score and player has tiles
    else if getPlayerTiles p /= [] then do
        p' <- removeOpenTileFromPlayer p
        ts' <- removeSpecificTile (convertTurnScoreToTile (biggestTile ts)) ts
        playGame ts' (roundRobinPlayers (p':ps))
    -- case when invalid score and player does not have tiles
    else do
        playGame ts (roundRobinPlayers (p:ps))


-- Player Functions
generatePlayers :: Int -> String -> IO [Player]
generatePlayers n name = return (createAllPlayers n name)

createAllPlayers :: Int -> String -> [Player]
createAllPlayers 0 name = createHumanPlayer name
createAllPlayers n name = createAllPlayers (n-1) name ++ 
                         [("Computer " ++ show n, computerTactic, [])]

createHumanPlayer :: String -> [Player]
createHumanPlayer name = [(name, playerTactic, [])]

getPlayerName :: Player -> String
getPlayerName (n, _, _) = n

getPlayerTactic :: Player -> Tactic
getPlayerTactic (_, t, _) = t

getPlayerTiles :: Player -> PlayerTiles
getPlayerTiles (_, _, ts) = ts

roundRobinPlayers :: [Player] -> [Player]
roundRobinPlayers []     = error "No players present for round robin"
roundRobinPlayers (p:ps) = ps ++ [p]

outputWinner :: [Player] -> IO ()
outputWinner ps = do
    putStrLn ("\nCongratulations " ++ getPlayerName (getWinner ps) ++ " for winning the game!")

getWinner :: [Player] -> Player
getWinner []     = error "No players present to determine winner"
getWinner (p:ps) | calculatePlayerScore (getPlayerTiles p) == calculateMaxScore ps = p
                 | otherwise                                                       = getWinner ps

-- tile Functions
returnTile :: Tile -> IO Tile
returnTile = return

generateTiles :: IO [Tile]
generateTiles = do
    return [ (x, (x - 17) `div` 4) | x <- [21..36] ]

convertTurnScoreToTile :: Int -> Tile
convertTurnScoreToTile n = (n, (n - 17) `div` 4)

biggestTile :: [Tile] -> Int
biggestTile [] = 0
biggestTile ts = maximum [ v | (v, _) <- ts ]

smallestTile :: [Tile] -> Int
smallestTile [] = 0
smallestTile ts = minimum [ v | (v, _) <- ts ]

tileElementOfGameTiles :: Tile -> GameTiles -> Bool
tileElementOfGameTiles t gts = t `elem` gts

tileElementOfOpenPlayerTiles :: Tile -> [Player] -> Bool
tileElementOfOpenPlayerTiles t ps = t `elem` getAllOpenPlayerTiles ps

getAllOpenPlayerTiles :: [Player] -> PlayerTiles
getAllOpenPlayerTiles []     = error "No player selected to generate open tiles"
getAllOpenPlayerTiles ps = concatMap (getOpenTile . getPlayerTiles) ps

getOpenTile :: PlayerTiles -> [Tile]
getOpenTile []  = []
getOpenTile pts = [head pts]

removeOpenTileFromPlayer :: Player -> IO Player
removeOpenTileFromPlayer (name, tactic, pts) = return (name, tactic, removeOpenTile pts)

removeOpenTile :: PlayerTiles -> PlayerTiles
removeOpenTile []  = []
removeOpenTile pts = tail pts

removeSpecificTile :: Tile -> [Tile] -> IO [Tile]
removeSpecificTile _ [] = return []
removeSpecificTile t ts = return (filter (/= t) ts)

addTileToPlayerTiles :: Tile -> Player -> IO Player
addTileToPlayerTiles t (name, tactic, pts) = return (name, tactic, t:pts)

returnPlayersAfterRemovingSpecificOpenTile :: Tile -> [Player] -> IO [Player]
returnPlayersAfterRemovingSpecificOpenTile t ps = return (removeTileFromOpenPlayerTiles t ps)

removeTileFromOpenPlayerTiles :: Tile -> [Player] -> [Player]
removeTileFromOpenPlayerTiles _ []                     = []
removeTileFromOpenPlayerTiles t ((name, tactic, ts):ps)
    | getOpenTile ts == [t] = (name, tactic, removeOpenTile ts) : ps
    | otherwise           = (name, tactic, ts) : removeTileFromOpenPlayerTiles t ps

addTile :: Tile -> [Tile] -> [Tile]
addTile t ts = t:ts

calculateMaxScore :: [Player] -> Int
calculateMaxScore ps = maximum [ calculatePlayerScore ts | (_, _, ts) <- ps ]

-- tile IO functions
outputAllTiles :: [Player] -> GameTiles -> IO ()
outputAllTiles ps ts = do
    putStrLn (showAllPlayerTiles ps)
    putStrLn (showGameTiles ts)

showAllPlayerTiles :: [Player] -> String
showAllPlayerTiles [] = error "No player selected to generate all tiles"
showAllPlayerTiles ps = concatMap showPlayerTiles ps

showPlayerTiles :: Player -> String
showPlayerTiles p = "\nPlayer: " ++ getPlayerName p ++ ", score: " 
                    ++ show (calculatePlayerScore (getPlayerTiles p))
                    ++ ", tiles:" ++ showTiles (getPlayerTiles p)

showGameTiles :: GameTiles -> String
showGameTiles ts = "\nRow: " ++ showTiles ts

showTiles :: [Tile] -> String
showTiles []     = ""
showTiles (t:ts) = " [" ++ show (fst t) ++ ", " ++ show (snd t) ++ "W]" ++ showTiles ts

calculatePlayerScore :: PlayerTiles -> Int
calculatePlayerScore []  = 0
calculatePlayerScore pts = sum [ w | (_, w) <- pts ]

-- turn functions
turn :: (Int -> Bool) -> Tactic -> SavedDice -> IO SavedDice
turn p t gs = do
    rs <- throwDices (8 - length gs)
    outputThrow rs
    if grabIsValid rs gs then
        do outputGrab gs
           outputScore gs
           putStr "   grab? "
           s <- tacticGrab t gs rs
           gs' <- throwToGrab s rs gs
           outputScore gs'
           if p (calculateTotalScore gs') &&
                grabbedDiceContainWorms gs' then do
                b <- tacticContinue t gs'
                if b then
                    turn p t gs'
                else do
                    outputScore gs'
                    return gs'
            else
                turn p t gs'
    else
        do return [Dice 0]

grabIsValid :: RolledDice -> SavedDice -> Bool
grabIsValid rs gs = not (null [s | s <- rs, s `notElem` gs])

grabbedDiceContainWorms :: SavedDice -> Bool
grabbedDiceContainWorms = elem (Dice 6)

throwDices :: Int -> IO RolledDice
throwDices 0 = return []
throwDices n = do 
    x <- randomSymbol
    xs <- throwDices (n-1)
    return (x:xs)

randomSymbol :: IO Symbol
randomSymbol = do 
    s <- randomRIO (1, 6 :: Int)
    return (Dice s)

outputThrow :: RolledDice -> IO ()
outputThrow rs = do putStrLn ("\nthrow  : " ++ showDices rs)

outputGrab :: SavedDice -> IO ()
outputGrab gs = do putStrLn ("\ntaken   : " ++ showDices gs)

showDices :: RolledDice -> String
showDices [] = "nothing"
showDices (x:xs) | not (null xs) = show x ++ "," ++ showDices xs
                        | otherwise     = show x
      
throwToGrab :: Symbol -> RolledDice -> SavedDice -> IO SavedDice
throwToGrab s rs gs = return ([ x | x <- rs, s == x ] ++ gs)

outputScore :: SavedDice -> IO ()
outputScore xs = do putStrLn ("score : " ++ show (calculateTotalScore xs))

calculateTotalScore :: SavedDice -> Int
calculateTotalScore = foldr ((+) . calculateScoreSymbol) 0

calculateScoreSymbol :: Symbol -> Int
calculateScoreSymbol (Dice 6) = 5
calculateScoreSymbol (Dice n) = n

-- Computer Tactics
computerTactic :: Tactic
computerTactic = Tactic computerTacticGrab computerTacticContinue

computerTacticGrab :: SavedDice -> RolledDice -> IO Symbol
computerTacticGrab gs rs = do
    return (maximum [ x | x <- rs, x `notElem` gs ])

computerTacticContinue :: SavedDice -> IO Bool
computerTacticContinue _ = do
    return False

-- Player Tactics
playerTactic :: Tactic
playerTactic = Tactic playerTacticGrab playerTacticContinue

playerTacticGrab :: SavedDice -> RolledDice -> IO Symbol
playerTacticGrab gs rs = do 
    chooseDicePlayer gs rs

--
-- this function needs to change and take into account if selected dice is part of the throw
--
chooseDicePlayer :: SavedDice -> RolledDice -> IO Symbol
chooseDicePlayer gs rs = do 
    c <- getDigit
    if charToSymbol c `elem` gs then
        do  putStrLn "this dice has all ready been taken"
            putStrLn "please choose a different dice"
            chooseDicePlayer gs rs
    else
        if charToSymbol c `notElem` rs then
            do  putStrLn "input does not match thrown dice"
                putStrLn "please select a dice that is part of the throw"
                chooseDicePlayer gs rs
        else
            return (charToSymbol c)

charToSymbol :: Char -> Symbol
charToSymbol c | toUpper c == 'W' = Dice 6
               | otherwise        = Dice (digitToInt c)

playerTacticContinue :: SavedDice -> IO Bool
playerTacticContinue _ = continuePlayer

continuePlayer :: IO Bool
continuePlayer = do 
    putStr "   continue? (y/n)  "
    c <- getChar'
    if (toLower c /= 'y') && (toLower c /= 'n') then
        do putStrLn "invalid choice\nplease choose between y/n"
           putChar c
           continuePlayer
    else if toLower c == 'y' then
            return True
        else
            return False