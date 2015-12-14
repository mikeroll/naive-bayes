module Main where

import Options.Applicative
import Options

program :: Options -> IO ()
program (Options dummy) = putStrLn "Hello Bayes"

main = execParser bayesOpts >>= program
