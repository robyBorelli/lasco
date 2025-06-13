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
-- main program
main :: IO ()
main = do
  Options{inputFile, outputFile, encoder, solver, solveMode, verbose, commenter, hypoPrinter} <- execParser opts
  let initialText = (case inputFile of 
                     (Nothing) -> "lasco\nReading from stdin\n"
                     (Just inPath) -> "")
  --let printMode = not silentMode
  let printMode = verbose
  putStr initialText

  source <- case inputFile of
    (Nothing)     -> getContents
    (Just inPath) -> readFile inPath

  case (parse Tight) source of
    Left err -> do
      putStrLn "parsing failed:"
      putStrLn err
      exitFailure
    Right prog -> do
      let hypoProg = transformHypoSpaceP prog (\hypo -> [ (Hypothesis num w h) | (num, (Hypothesis _ w h)) <- zip [1..] hypo])
      let hypoSpace = getHypoSpaceP hypoProg
      let (normProg, lastIndex) = (normalize 0) hypoProg

      printStrLn printMode ("@@@@@@@@@@@@@@@ normalized program:\n"++(printTree normProg))

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
            groundProg <- ground lastIndex printMode normProg
            let finalProg = groundProg
            let groundHypoSpace = getHypoSpaceP finalProg
            let aspCode = (printTree) 
                           . (removeComments commenter)
                           . (optimize solveMode solver groundHypoSpace)
                           . (printSolution solver hypoSpace)
                           . (encode encoder) 
                           $ finalProg

            case outputFile of
              Just outPath -> writeFile outPath aspCode
              Nothing      -> printStrLn (printMode || solver == Nothing) aspCode

            case solver of
              (Nothing) -> return ()
              (Just solverType)  -> do
                out <- solve solveMode solverType aspCode
                putStrLn "\n\n@@@@@@@@@@@@@@@ encoding completed\n\n"
                putStrLn out