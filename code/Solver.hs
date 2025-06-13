module Solver where
import System.Process
import Parser.AbsGrammar
import EncoderUtilities
import System.IO 
import GHC.Generics (Generic)

data Solver = Clingo | Dlv 
 deriving (Eq, Generic)

data SolveMode = First | All | Optimum | NumberModels Int
  deriving (Eq)

instance Show Solver where
  show Clingo = "'clingo'"
  show Dlv = "'dlv'"

instance Show SolveMode where
  show First = "'first'"
  show All = "'all'"
  show Optimum = "'optimum'"
  show (NumberModels int) = show int

class Solve a where 
  solve :: SolveMode -> a -> String -> IO String

instance Solve Solver where
  solve solverMode solverType asp = do
    let prog = if solverType == Clingo then "clingo" else "dlv"
    let cmd = proc prog [
                case solverMode of
                   First -> "-n1"
                   All -> "-n0"
                   Optimum -> "-n0"
                   NumberModels int -> "-n"++(show int)
               ]
    (Just hin, Just hout, Just herr, ph) <- createProcess cmd { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    hPutStr hin asp
    hClose hin
    out <- hGetContents hout
    err <- hGetContents herr
    exitCode <- waitForProcess ph
    return out

optimize :: SolveMode -> (Maybe Solver) -> [Declaration] -> Program -> Program
optimize (Optimum) (Just Dlv) hypoSpace (Task decls) = 
    (Task (decls ++
           [Comment "search for the best hypothesis"] ++
           [WeakContraint [(atomH $ [intTerm index])] weight 1 index | (index,weight) <- getHypoIndexWeightList hypoSpace ]) )

optimize (Optimum) _ hypoSpace (Task decls) = 
    (Task (decls ++
           [Comment "search for the best hypothesis"] ++
           [ (Minimize [(weight, index, (atomH $ [intTerm index] )) | (index,weight) <-  getHypoIndexWeightList hypoSpace ]) ]))
optimize _ _ _ task = task