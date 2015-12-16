module Options
( Options(..)
, bayesOpts
) where

import Options.Applicative

data Options = Options
    { infile :: FilePath
    , dataSplit :: Int
    }

opts :: Parser Options
opts = Options
    <$> argument str
        ( metavar "INFILE" )
    <*> option auto
        ( long "data-split"
       <> short 's'
       <> value 80
       <> showDefault
       <> help "Training data share percent." )

bayesOpts :: ParserInfo Options
bayesOpts = info (helper <*> opts)
            ( fullDesc
           <> progDesc "Classify objects from INFILE using Naïve Bayes method"
           <> header "Naïve Bayes clustering sample program" )
