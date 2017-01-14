{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Main where

import InterpreterBase

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans
  (liftIO, lift)
import qualified Data.Map as Map
import Data.IORef
import Prelude hiding
  (lookup)

{-----------------------------------------
------------------------------------------
LineNumber:
  Maintains a pointer to the current line
  of the program being run.
  It will not increase past the number of
  lines of code, and will not decrease
  below 0.
------------------------------------------
-----------------------------------------}
type LineNumber = IORef (Int,Int)

newNum :: Int -> IO (LineNumber)
{-
Create a new line pointer. The pointer
also stores the total number of lines
in the code.
-}
newNum limit = newIORef (0, limit)


nextLine :: LineNumber -> IO Bool
{-
Move the pointer to point to the next line
of code, or notify the user if the
pointer has reached the end of the program.
-}
nextLine num = do
  (nowN, limit) <- readIORef num
  print nowN
  case (nowN == limit - 1) of
    True        -> do
      putStrLn "End of program"
      return True
    otherwise   -> do
      modifyIORef num (\(x,y) -> (x + 1,y))
      return False

prevLine :: LineNumber -> IO Bool
{-
Move the pointer to point to the last line
of code, or notify the user if the
pointer has reached the start of the program.
-}
prevLine num = do
  (nowN, limit) <- readIORef num
  case (nowN == 0) of
    True        -> do
      putStrLn "Start of program"
      return True
    otherwise   -> do
      modifyIORef num (\(x,y) -> (x - 1,y))
      return False

now :: LineNumber -> IO Int
{-
Return the current position of the
pointer.
-}
now num = do
  (nowN, _) <- readIORef num
  return nowN

{-----------------------------------------
------------------------------------------
Main Interpreter Behaviour:
  The interpreter will run and wait for
  user input. Depending on this input, it
  will carry out different actions.
------------------------------------------
-----------------------------------------}

main :: IO ()
{-
Initialise the line number pointer.
Run the first line of code and then enter
the interpreter loop.
-}
main = do
  program          <- readProgram
  lineNum          <- newNum $ length program
  pos              <- now lineNum
  Right (_, env)   <- runRun (evaluate program pos) [(0, Map.empty)]
  interpreterLoop program env lineNum
  putStrLn "DONE!"

interpreterLoop :: [Statement] -> [(Int, Env)] -> LineNumber -> IO ()
{-
The main functionality of the interpreter.
The function carried out depends on the input
from the user.
-}
interpreterLoop program env lineNum = do
  putStr "\nInput: "
  input <- getLine
  case (head input) of
    -- Move to the next line
    'n' -> do
      limitCheck <- nextLine lineNum
      -- Check to see if we have not run the last line of code
      case limitCheck of
        False     -> do
                        pos <- now lineNum
                        putStr $ "\nRUNNING LINE: " ++ (show pos) ++ "\n"
                        -- Run the line of code
                        Right (_, nEnv) <- runRun (evaluate program pos) env
                        -- Repeat the loop
                        interpreterLoop program nEnv lineNum
        otherwise -> do
                        -- We have reached the end of the program
                        -- Do nothing.
                        interpreterLoop program env lineNum
    -- Move to the previous line of code
    'b' -> do
      limitCheck <- prevLine lineNum
      -- Check to see if we are not on the first line of code
      case limitCheck of
        False     -> do
                        pos <- now lineNum
                        putStr $ "\nRETURNING TO LINE: " ++ (show pos) ++ "\n"
                        -- Filter out the approproate environments
                        let nEnv = filterHist pos env
                        -- Repeat the loop
                        interpreterLoop program nEnv lineNum
        otherwise -> do
                        -- We are at the first line of code
                        -- Do nothing
                        interpreterLoop program env lineNum
    -- Inspect a variable
    'i' -> do
      let varName = drop 2 input
      -- Find the value of the variable in the current environment.
      let Right i = runEval (snd $ head env) (inspect varName)
      print i
      interpreterLoop program env lineNum
    -- Get the history of a variable
    'h' -> do
      let varName = drop 2 input
      -- Find the value of the variable in all environments.
      history varName env
      interpreterLoop program env lineNum
    -- Exit the interpreter
    'q' -> do
      return ()
    -- Deals with unknown input, does nothing.
    otherwise -> interpreterLoop program env lineNum

readProgram :: IO ([Statement])
{- Read the program from the text file. -}
readProgram = do
  contents <- readFile "src/program.txt"
  let expressions = read contents :: [Statement]
  return expressions

evaluate :: [Statement] -> Int -> Run ()
{- Evaluate a line of code. -}
evaluate st line = do
  liftIO $ print (st !! line)
  exec (st !! line) line

inspect :: String -> Eval Val
{- Look up a value from the environment. -}
inspect name = do
  env <- ask
  lookup name env

history :: String -> [(Int, Env)] -> IO ()
{- Look up a value from all environments, and print them. -}
history name env = printHist name $ head (mapM (\(i,e) -> do
    let l = Map.lookup name e
    case l of
      Just v -> return (i,v)
      Nothing -> return (i,Nil)) env)

filterHist :: Int -> [(Int, Env)] -> [(Int, Env)]
{- Filter out the environments according to line number. -}
filterHist line env = filter (\(i,_) -> i <= line) env

printHist :: String -> [(Int, Val)] -> IO ()
{- A function to help print the history of a variable. -}
printHist name env = do
  putStrLn $ "History " ++ name ++ ": \n"
  mapM_ (\(i,v) -> putStrLn $ "Line " ++ (show i) ++ " -> " ++ (show v)) env
