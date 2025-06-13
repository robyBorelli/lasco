module Grounder where
import System.Process
import System.Exit (exitFailure)
import System.IO 
import Parser.AbsGrammar
import Parser.PrintGrammar
import EncoderUtilities
import Parser.ParGrammar (parse)
import Parser.LexUtilities ( LexerModality(..))
import Printer
import Normalizer
import Data.List (isPrefixOf, elem)
import qualified Data.Set as Set

ground :: Int -> Bool -> Program  -> IO Program
ground lastIndex printMode p = do 
   let (p',choiceStructures) = filterChoiceRules ((getBackgroundP p) ++ (encodeHypos . getHypoSpaceP $ p))
   let examples = getExamplesP p
   let gr = (isGroundProgam (Task p'))
   case gr of
    True -> do 
          return p
    False -> do
          printStrLn printMode "@@@@@@@@@@@@@@@ input gringo program:"
          let (p'', facts) = filterFacts . encodeGroundAtoms $ p'
          printStrLn printMode (printTree (Task p''))
          printStrLn printMode "@@@@@@@@@@@@@@@ parsing after gringo: "
          grounded <- execGringo . printTree $ (Task p'')
          case (parse Shallow) grounded of
            Left err -> do
              putStrLn "Grounding failed"
              putStrLn err
              exitFailure
            Right (Task parsedGrounded) -> do
              printStrLn printMode (printTree (Task parsedGrounded))
              let (parsedGrounded',guardedHeads) = filterGuardedHeads parsedGrounded
              let (newProgram) =  (Task ( ((decodeCardConstraints choiceStructures guardedHeads) . decodeHypos $ parsedGrounded') ++ examples ++ facts) )
              newProgram' <- case choiceStructures of
                [] -> return newProgram
                _ -> do
                  printStrLn printMode "@@@@@@@@@@@@@@@ ground task with ground cardinality constraints: "
                  printStrLn printMode (printTree newProgram)
                  let (pr,_) = normalize (lastIndex+1) newProgram
                  return pr
              printStrLn printMode "@@@@@@@@@@@@@@@ final ground task: "
              printStrLn printMode (printTree newProgram')
              putStrLn "@@@@@@@@@@@@@@@ grounding ended"
              return newProgram'

filterFacts :: [Declaration] -> ([Declaration], [Declaration])
filterFacts [] = ([],[])
filterFacts (x:xs) = (prog, guards)
  where flag = isFact x
        (prog', guards') = filterFacts xs
        (prog, guards) = case flag of
          True -> (prog', x:guards')
          False -> (x:prog', guards')

isFact :: Declaration -> Bool
isFact (AspRule (NormalRule (SimpleHead _ ) [])) = True
isFact (AspRule (Fact _)) = True
isFact _ = False

filterGuardedHeads:: [Declaration] -> ([Declaration],[Declaration])
filterGuardedHeads [] = ([],[])
filterGuardedHeads (x:xs) = (prog, guards)
  where flag = isGuard x
        (prog', guards') = filterGuardedHeads xs
        (prog, guards) = case flag of
          True -> (prog', x:guards')
          False -> (x:prog', guards')

isGuard :: Declaration -> Bool
isGuard (AspRule (NormalRule (SimpleHead (CompositeAtom (BasicSymbol str) lits)) body)) | (choiceRuleGuardedHead  `isPrefixOf` str) = True
isGuard _ = False
  
filterChoiceRules :: [Declaration] -> ([Declaration],[Declaration])
filterChoiceRules [] = ([],[])
filterChoiceRules (x:xs) = (prog, choices)
  where flag = isChoiceRule x
        (prog', choice') = filterChoiceRules xs
        (prog, choices) = case flag of
          True -> (prog', x:choice')
          False -> (x:prog', choice')

isChoiceRule :: Declaration -> Bool
isChoiceRule (AspRule (NormalRule (SimpleHead (CompositeAtom (BasicSymbol str) lits)) body)) | str == choiceRuleStructureHead = True
                                                                               | otherwise = False
isChoiceRule (AspRule (NormalRule  _ _)) = False
isChoiceRule _ = error "error during grounding"

decodeCardConstraints :: [Declaration] -> [Declaration] -> [Declaration] -> [Declaration]
decodeCardConstraints choices guards (decls) = [ decodeCardDecl choices guards d | d <- decls]

decodeCardDecl :: [Declaration] -> [Declaration] -> Declaration -> Declaration
decodeCardDecl choices guards (AspRule r) = (AspRule (decodeCardRule choices guards r))
decodeCardDecl choices guards (Hypothesis i w r) =  (Hypothesis i w (decodeCardRule choices guards r))
decodeCardDecl _ _ _ = error "error during grounding"

decodeCardRule :: [Declaration] -> [Declaration] -> AspDeclaration -> AspDeclaration
decodeCardRule choices guards ( NormalRule (SimpleHead (CompositeAtom (BasicSymbol s) (idx:var) )) (body) ) | choiceRuleHead `isPrefixOf` s = ret
  where relevantChoice@(AspRule (NormalRule (SimpleHead (CompositeAtom _ (x:elb:eub:terms) )) _)) = head ( filter (\(AspRule (NormalRule (SimpleHead (CompositeAtom _ (idx':var) )) _)) -> idx == idx') choices )
        relevantGuards =  
          map (\(AspRule (NormalRule (SimpleHead (CompositeAtom (BasicSymbol str) (idx':var) )) body)) -> 
                (AspRule (NormalRule (SimpleHead (CompositeAtom (BasicSymbol (drop (length choiceRuleGuardedHead) str)) (var))) (removeLascoHypo . removeLascoTrue $ body))))
            (filter (\(AspRule (NormalRule (SimpleHead (CompositeAtom _ (idx':var) )) _)) -> idx == idx') guards)
        (ArithmeticTerm (IntExpr lb)) = elb
        (ArithmeticTerm (IntExpr ub)) = eub

        removeLascoTrue lits = filter (\x -> x /= ((PositiveLiteral (SimpleAtom (BasicSymbol "lasco_true"))))) lits
        removeLascoHypo lits = filter (\x -> case x of 
          ((PositiveLiteral (CompositeAtom (BasicSymbol str) _ ))) | str == hypoHead -> False
          _ -> True) lits
        body' = removeLascoTrue body
        ret = (GroundCardConstraint lb heads ub body') 
        bindings = createBindings var
        createBindings :: [Term] -> [(ArithmeticExpr, Either Integer String)]
        createBindings [] = []
        createBindings [x] = error "error during grounding"
        createBindings (name:value:xs) = (name',value') : (createBindings xs)
          where extractVarName (Constant (BasicSymbol var_name)) | predicatePrefix `isPrefixOf` var_name = drop (length predicatePrefix) var_name
                extractVarName _ = error "error during grounding"
                name' = (Variable (VariableSymbol (extractVarName name)))
                value' = case value of
                  (ArithmeticTerm (IntExpr i)) -> Left i
                  (Constant (BasicSymbol s)) -> Right s
                  _ ->  error "error during grounding"
        heads = [ PositiveLiteral (bindVarsAtom a b) | (FunctionalTerm a) <- terms, b <- bindings ]
            ++  (foldr (++) [] [ createGuardedLiteral at body | r@(AspRule (NormalRule (SimpleHead at) body)) <- relevantGuards])
        
        createGuardedLiteral at body | checkGuards at body = [(PositiveLiteral at)]
                                     | otherwise = []
        checkGuards at bd | (isGroundAtom at) && (isValid bd) = True
                          | otherwise = False
        isValid [] = True
        isValid bd =  all (`elem` bd) body' 
        --isValid bd =  all (`elem` body') bd

decodeCardRule _ _ r@(NormalRule _ _) = r
decodeCardRule _  _ (Fact at) = (NormalRule (SimpleHead at) [])
decodeCardRule _ _ _ = error ("error during grounding")

decodeHypos :: [Declaration] -> [Declaration]
decodeHypos (decls) = [ decodeHypoRule (AspRule aspdecl) | (AspRule aspdecl) <- decls]

decodeHypoRule :: Declaration -> Declaration
decodeHypoRule rule@(AspRule (NormalRule h bs)) = ret
  where 
    ret = case getHypo bs of 
      Nothing ->  rule
      (Just (newBs,index, weigth)) -> Hypothesis index weigth (NormalRule h newBs)
    getHypo :: [Literal] -> Maybe ([Literal], Integer, Integer)
    getHypo [] = Nothing
    getHypo [x] = let r =  checkLiteral x in
      case r of 
        (Nothing) -> Nothing
        (Just (index, weigth)) -> Just ([], index, weigth)
    getHypo (x:xs) = lst
       where this = checkLiteral x
             other = getHypo xs
             lst = case (this, other) of
                    (Just (index, weigth), Nothing) -> (Just (xs, index, weigth))
                    (Nothing, (Just (ls, index, weigth))) -> (Just (x:ls, index, weigth))
                    (Just (index, weigth), (Just (ls, index1, weigth1))) -> error ("grounding error")
                    (Nothing, Nothing) -> Nothing
    checkLiteral :: Literal -> Maybe (Integer, Integer)
    checkLiteral (PositiveLiteral ((CompositeAtom (BasicSymbol bs) [ArithmeticTerm (IntExpr index), ArithmeticTerm (IntExpr weigth)]))) | bs == hypoHead = (Just (index, weigth))
    checkLiteral _ = Nothing
decodeHypoRule d@(AspRule (Fact _)) = d
decodeHypoRule (AspRule _) = error "error during grounding"

encodeHypos :: [Declaration] -> [Declaration]
encodeHypos decls = [ encodeHypoRule index weigth (AspRule rule) | (Hypothesis index weigth  rule ) <- decls ]

encodeHypoRule :: Integer -> Integer -> Declaration -> Declaration
encodeHypoRule index weigth (AspRule (NormalRule (SimpleHead h) body)) = 
  (AspRule (NormalRule (SimpleHead h) (body++[PositiveLiteral . atomH $ [ intTerm  index, intTerm  weigth]])))
encodeHypoRule _ _ r = error ("Unsupported hypothesis rule: "++(printTree r))

encodeGroundAtoms :: [Declaration] -> [Declaration]
encodeGroundAtoms decls = choices ++ decls
 where atoms = collectGroundAtoms decls
       choices = [ AspRule $ ( NormalRule (ChoiceHead (ExplicitBound 0) [(SimpleChoiceElem a)] (ExplicitBound 1)) [] )| a <- (Set.toList atoms)]

execGringo::String -> IO String 
execGringo asp = do
    let prog = "gringo"
    let cmd = proc prog ["--warn=no-atom-undefined", "--text"]
    (Just hin, Just hout, Just herr, ph) <- createProcess cmd { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    hPutStr hin asp
    hClose hin
    out <- hGetContents hout 
    return out
