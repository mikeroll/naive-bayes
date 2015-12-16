module Main where

import Options.Applicative
import Options

program :: Options -> IO ()
program opts = putStrLn "Hello Bayes"

main = execParser bayesOpts >>= program
