module Printer where
import System.IO 

printStrLn :: Bool -> String -> IO ()
printStrLn True str = putStrLn str
printStrLn False _ = return ()


printStr :: Bool -> String -> IO ()
printStr True str = putStrLn str
printStr False _ = return ()