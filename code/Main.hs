{-# LANGUAGE DeriveGeneric #-}

module Main where
import Options.Applicative
import System.Exit (exitFailure)
import System.IO 
import Parser.AbsGrammar
import Parser.ParGrammar (parse)
import Parser.LexUtilities ( LexerModality(..))
import Parser.PrintGrammar (printTree)
import Normalizer (normalize)
import Encoder
import Solver
import Grounder
import InputParser
import EncoderUtilities
import Printer
import Control.DeepSeq (deepseq, NFData)
import System.CPUTime (getCPUTime)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)


-- time measures
timePure :: NFData a => a -> IO (Double, a)
timePure expr = do
  start <- getCurrentTime
  expr `deepseq` return ()
  end <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime end start) :: Double
  return (elapsed, expr)


timeIO :: IO a -> IO (Double, a)
timeIO action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime end start) :: Double
  return (elapsed, result)


-- main program
main :: IO ()
main = do


  putStrLn "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
  putStrLn "%% -------------- LASCO --------------- %%\n"
  putStrLn "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
  tStart <- getCurrentTime
  Options{inputFile, outputFile, encoder, solver, solveMode, verbose, commenter, hypoPrinter} <- execParser opts
  let initialText = (case inputFile of 
                     (Nothing) -> "%% reading from stdin %%%%%%%%%%%%%%%%%%%\n"
                     (Just inPath) -> "")
  let printMode = verbose
  putStr initialText

  source <- case inputFile of
    (Nothing)     -> getContents
    (Just inPath) -> readFile inPath

  case (parse Tight) source of
    Left err -> do
      putStrLn "%% parsing failed:%%%%%%%%%%%%%%%%%%%%%%%%"
      putStrLn err
      exitFailure
    Right prog -> do
      let hypoProg = transformHypoSpaceP prog (\hypo -> [ (Hypothesis num w h) | (num, (Hypothesis _ w h)) <- zip [1..] hypo])
      let hypoSpace = getHypoSpaceP hypoProg
      (tNorm, (normProg, lastIndex)) <- timePure ((normalize 0) hypoProg)

      printStrLn printMode ("%% normalized program: %%%%%%%%%%%%%%%%%%%\n"++(printTree normProg))

      case hypoPrinter of 
        (Just list) -> do
            let prePrint = if printMode == False then (\x -> "") else (\a -> "%% hypothesis "++show a++"\n")
            let postPrint = if printMode == False then ("\n") else ("\n\n")
            putStrLn 
              ( foldr (++) [] 
                  (map  (\(Hypothesis a b c) -> ((prePrint a)++(printTree (Hypothesis a b c))++postPrint) )
                    ( filter (\(Hypothesis index _ _) -> elem index list) hypoSpace )
                  )
              )
        Nothing -> do
            (tGround,tEnc,tSolve) <- case getExamplesP normProg of
              [] -> do
                      putStrLn "%% The task contain no examples %%%%%%%%%%"
                      return (0,0,0)
              _->   do
                      (tGround, groundProg) <- timeIO (ground lastIndex printMode normProg)
                      let finalProg = groundProg
                      let groundHypoSpace = getHypoSpaceP finalProg
                      (tEnc, aspCode) <- timePure (  (printTree) 
                                                   . (removeComments (not commenter))
                                                   . (optimize solveMode solver groundHypoSpace)
                                                   . (printSolution solver hypoSpace)
                                                   . (encode encoder) 
                                                   $ finalProg)
                      putStrLn "\n\n%% encoding completed %%%%%%%%%%%%%%%%%%%%\n\n"


                      case outputFile of
                        Just outPath -> writeFile outPath aspCode
                        Nothing      -> printStrLn (printMode || solver == Nothing) aspCode

                      tSolve <- case solver of
                        (Nothing) -> return 0
                        (Just solverType)  -> do
                          (tSolve, out) <- timeIO (solve solveMode solverType aspCode)
                          putStrLn out
                          return tSolve

                      return (tGround, tEnc,tSolve)

            tEnd <- getCurrentTime
            let tTotal:: Double 
                tTotal = realToFrac (diffUTCTime tEnd tStart) :: Double

            putStrLn "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
            putStrLn "%% ---------- ELAPSED TIME (s) -------- %%\n"
            printf   "%% normalization:     %.6f s\n" tNorm
            printf   "%% grounding:         %.6f s\n" tGround
            printf   "%% encoding:          %.6f s\n" tEnc
            printf   "%% tsolve:            %.6f s\n" tSolve
            putStrLn "%% ____________________________________ %%\n"
            printf   "%% total:             %.6f s\n" tTotal
            putStrLn "%% ----------------- END -------------- %%\n"
            putStrLn "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"