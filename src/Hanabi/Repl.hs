{-# LANGUAGE OverloadedStrings #-}

module Hanabi.Repl where

import Hanabi
import Hanabi.Game
import Hanabi.Print
import Hanabi.Transitions

import Control.Monad (when)
import Data.IORef
import qualified Data.Text.IO as IO
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE gameRef #-}
gameRef :: IORef [Game]
gameRef = unsafePerformIO (newIORef [])

defaultGame :: IO ()
defaultGame = startGame ["1", "2", "3"]

startGame :: [PlayerId] -> IO ()
startGame [] = error "no players"
startGame (x:xs) = do
  game <- initState x xs
  writeIORef gameRef [game]
  IO.putStrLn $ fairPrint game

turn :: (Game -> Either GameOver Game) -> IO ()
turn f = do
  game1 <- getLastState
  case f game1 of
    Right game2 -> IO.putStrLn (fairPrint game2) >> recordGame game2
    Left over -> print over

recordGame :: Game -> IO ()
recordGame game = modifyIORef gameRef (game :)

getLastState :: IO Game
getLastState = fmap head getAllStates

getAllStates :: IO [Game]
getAllStates = readIORef gameRef

getStateAt :: Int -> IO Game
getStateAt i = fmap (!! i) getAllStates

printStateAt :: Int -> IO ()
printStateAt i = getStateAt i >>= prettyPrint

undo :: IO ()
undo = do
  states <- getAllStates
  when (length states >= 2) (writeIORef gameRef (drop 1 states))

hint
  :: IsHint a
  => PlayerId -> a -> IO ()
hint pId h = turn (hintGiven h pId)

play :: Int -> IO ()
play i = turn (\game -> cardPlayed (cardAt i game) game)

discard :: Int -> IO ()
discard i = turn (\game -> cardDiscarded (cardAt i game) game)
