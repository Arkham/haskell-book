module Main where

import Control.Monad (forever, when)
import Data.List (intercalate)
import Data.Traversable (traverse)
import Morse (morseToChar, stringToMorse)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stdin)

convertToMorse :: IO ()
convertToMorse =
  forever $ do
    weAreDone <- isEOF stdin
    when weAreDone exitSuccess
    line <- getline stdin
    convertLine line
  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        (Just str) -> putStrLn (unwords str)
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse =
  forever $ do
    weAreDone <- isEOF stdin
    when weAreDone exitSuccesso
    line <- getline stdin
    convertLine line
