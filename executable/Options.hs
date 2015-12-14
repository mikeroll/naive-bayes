module Options
( Options(..)
, bayesOpts
) where

import Options.Applicative

data Options = Options
    { dummy :: String
    }

opts :: Parser Options
opts = Options
    <$> option str
        ( long "dummy"
       <> value "" )

bayesOpts :: ParserInfo Options
bayesOpts = info (helper <*> opts)
           ( fullDesc
          <> progDesc "Classify objects from INFILE using Naïve Bayes method"
          <> header "Naïve Bayes clustering sample program" )
