{-|

Finding all "uncrossed knight's paths" 
(http://en.wikipedia.org/wiki/Longest_uncrossed_knight%27s_path) on a
NxM rectangular board. 

This part consists of I/O related components.

-}
module Main(main) where

import UncrossedPaths
import System.IO
import System.Environment(getArgs, getProgName)
import Text.Regex.Posix((=~))
import Control.Monad(liftM, ap)

{-|
Write the paths to a file.

This requires four command line arguments:
- filename (to be written to the current working directory)
- minimum length for an accepted path
- Starting point between (0, 0) and (3, 3)
- Limits of the board between (4, 4) and (9, 9)

It does not matter in which order the input arguments are given.
-}
main :: IO ()
main = do
  argv <- getArgs
  case parseInput argv of
    Just input -> do
      outh <- openFile (outFile input)  WriteMode
      mainloop outh (filter ((>(minLen input)) . length) 
                     (allpaths (limits input) (start input)))
      hClose outh
    Nothing -> do 
      prog <- getProgName
      hPutStrLn stderr $ "Invalid input arguments\n"
         ++ "Usage: " ++ prog ++ " filename N \"(x1,y1)\" \"(x2,y2)\""

mainloop :: Handle -> [Path]  -> IO ()
mainloop _ [] = return ()
mainloop outh paths = do
  hPutStrLn outh $ showPath $ head paths
  mainloop outh $ tail paths 
        
        
allpaths :: Limits -> StepData -> [Path]
allpaths limits start = possiblePaths limits [] (StepWrapper start 
                                                 (Step 0 0) 
                                                 (SumPoint 0 0))

showPath :: Path -> String
showPath path = let start = (show ((length path) - 1)) ++ " steps: "
                    end   = show (map (\sw -> (x (p sw), y (p sw))) path)
               in start ++ end ++ "\n"
                  
{-|
Regexes for the input arguments
-}
expectedInput :: [String]
expectedInput = [ "^[a-z0-9\\._\\-]+" 
                , "^[1-9][0-9]*" 
                , "^\\([4-9],[4-9]\\)" 
                , "^\\([0-3],[0-3]\\)"]

{-|
Trying to match one of expected inputs to given string. 
-}
maybeGetInput :: [String] -> String -> Maybe String
maybeGetInput argv ptrn =  
  case dropWhile (not . (\x -> x =~ ptrn :: Bool)) argv of
    x:xs -> Just x
    [] -> Nothing

{-|
Helper to read a tuple of two integers make an arbitrary 'pair'
data structure from it.
-}
inputTuple :: (Int -> Int -> a) -> (String -> a)
inputTuple pair = (\x -> pair (fst x) (snd x)) . 
               (\x -> read x :: (Int,Int))

{-|
Parse all the inputs from a list of arguments.  
If one or more of the expected arguments are missing, returns nothing.
-}
parseInput :: [String] -> Maybe Inputs
parseInput argv = Inputs `liftM` outFile 
                            `ap` minLen 
                            `ap` limits 
                            `ap` start
  where 
    mayInputs = map (maybeGetInput argv) expectedInput
    outFile = mayInputs !! 0
    minLen  = (\x -> read x :: Int) `liftM` (mayInputs !! 1) 
    limits  = inputTuple Limits `liftM` (mayInputs !! 2)
    start   = inputTuple Point `liftM` (mayInputs !! 3)
                     
data Inputs = Inputs { outFile :: String,
                       minLen  :: Int,        
                       limits  :: Limits,
                       start   :: StepData }
