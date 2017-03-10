module Main where

import Hanabi.Client
import Hanabi.Ui (startApp)

main :: IO ()
-- main = startClient
main = startApp >> return ()
