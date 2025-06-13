module InputParser where

import Encoder
import Solver
import GHC.Generics (Generic)
import Options.Applicative
import Text.Read (readMaybe)

data Options = Options
  { inputFile  :: Maybe FilePath
  , outputFile :: Maybe FilePath
  , encoder    :: EncoderType
  , solver     :: Maybe Solver
  , solveMode  :: SolveMode
  --, silentMode :: Bool
  , verbose :: Bool
  , commenter  :: Bool
  , hypoPrinter:: Maybe [Integer]
  }

defaultEncoder = Disjunctive
defaultSolveMode = First
defaultAspSolver = Clingo
optionalString = "(optional)"

-- encoder parser
encoderParser :: Parser EncoderType
encoderParser = option (eitherReader parseEncoder)
  ( long "encoder"
 <> short 'e'    
 <> metavar "ENCODER"
 <> help ("Specifies the encoding type to be "++ show Exponential++" or "++show Disjunctive++".")
 <> value defaultEncoder
 <> showDefaultWith show )

parseEncoder :: String -> Either String EncoderType
parseEncoder "exponential" = Right Exponential
parseEncoder "exp"         = Right Exponential
parseEncoder "expo"        = Right Exponential
parseEncoder "disjunctive" = Right Disjunctive
parseEncoder "dis"         = Right Disjunctive
parseEncoder "disj"        = Right Disjunctive
parseEncoder s             = Left $ "Wrong encoding type." 

-- solver parser
solverParser :: Parser (Maybe Solver)
solverParser = optional $ option (eitherReader parseSolver)
  ( long "solve"
 <> metavar "SOLVER"
 <> help ("Solves the encoding with "++ (show Clingo) ++ " or "++(show Dlv)++". ")
  )

parseSolver :: String -> Either String Solver
parseSolver "clingo" = Right Clingo
parseSolver "dlv"    = Right Dlv
parseSolver ""       = Right defaultAspSolver
parseSolver s        = Left $ "Wrong solver type."


-- solveMode parser
solveModeParser :: Parser SolveMode
solveModeParser = option (eitherReader parseSolveMode)
  ( long "solve-mode"
 <> metavar "MODE"
 <> help ("Looks for "++ show First++" / "++show All++" / "++ show Optimum++" hypothesis. MODE can be also an integer representing the number of hypothesis to search.")
 <> value defaultSolveMode
 <> showDefaultWith show )

isInt :: String -> Bool
isInt s = case readMaybe s :: Maybe Int of
            Just _  -> True
            Nothing -> False

parseSolveMode :: String -> Either String SolveMode
parseSolveMode "first"   = Right First
parseSolveMode "some"    = Right First
parseSolveMode "all"     = Right All
parseSolveMode "optimum" = Right Optimum
parseSolveMode "opt"     = Right Optimum
parseSolveMode "best"    = Right Optimum
parseSolveMode s | isInt s = Right (NumberModels (read s::Int))
parseSolveMode s         = Left $ "Wrong solver mode type."

-- hypoParser
hypoParser :: Parser (Maybe [Integer])
hypoParser = optional $ option (eitherReader parseHypo)
  ( long "show-hypos"
 <> metavar "[i1,i2,i3,...]"
 <> help ("Prints the hypothesis of specified indexes and exits."))

parseHypo :: String -> Either String ([Integer])
parseHypo s =  case readMaybe s :: Maybe [Integer] of
    Just xs -> Right xs
    Nothing -> Left $ "The string \"" ++ s ++ "\" is not a valid list of integers."

-- main parser
optionsParser :: Parser Options
optionsParser = Options
  <$> (optional $ strOption
        ( long "input"
       <> short 'i'
       <> metavar "IFILE"
       <> help "Specifies the input file containing the learning task to solve." ))
  <*> (optional $ strOption
        ( long "output"
       <> short 'o'
       <> metavar "OFILE"
       <> help "Specifies the output file which will contain the output encoding." ))
  <*> encoderParser
  <*> solverParser
  <*> solveModeParser
  <*> switch
      ( long "verbose"
       <> short 'v'
       <> help "Prints intermediate steps and comments useful for debugging." )
  <*> switch
      ( long "no-comments"
       <> help "Disables comments in the encoding." )
  <*> hypoParser

-- parser info
opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Compiles a Learning from Answer Sets (LAS) task into an ASP program"
 <> header "lasco: the LAS → ASP compiler" )