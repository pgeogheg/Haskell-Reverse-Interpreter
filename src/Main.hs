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
LineNumber:
  Maintains a pointer to the current line
  of the program being run.
  It will not increase past the number of
  lines of code, and will not decrease
  below 0.
-----------------------------------------}
type LineNumber = IORef (Int,Int)

newNum :: Int -> IO (LineNumber)
newNum limit = newIORef (0, limit)

nextLine :: LineNumber -> IO Bool
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
now num = do
  (nowN, _) <- readIORef num
  return nowN

{-----------------------------------------
Main Interpreter Behaviour:
  The interpreter will run and wait for
  user input. Depending on this input, it
  will carry out different actions.
-----------------------------------------}

main :: IO ()
main = do
  program          <- readProgram
  lineNum          <- newNum $ length program
  pos              <- now lineNum
  Right (_, env)   <- runRun (evaluate program pos) [(0, Map.empty)]
  interpreterLoop program env lineNum
  putStrLn "DONE!"

interpreterLoop :: [Statement] -> [(Int, Env)] -> LineNumber -> IO (Either String ((), [(Int, Env)]))
interpreterLoop program env lineNum = do
  putStr "\nInput: "
  input <- getLine
  case (head input) of
    'n' -> do
      limitCheck <- nextLine lineNum
      case limitCheck of
        False     -> do
                        pos <- now lineNum
                        putStr $ "\nRUNNING LINE: " ++ (show pos) ++ "\n"
                        Right (_, nEnv) <- runRun (evaluate program pos) env
                        interpreterLoop program nEnv lineNum
        otherwise -> do
                        interpreterLoop program env lineNum
    'b' -> do
      limitCheck <- prevLine lineNum
      case limitCheck of
        False     -> do
                        pos <- now lineNum
                        putStr $ "\nRUNNING LINE: " ++ (show pos) ++ "\n"
                        let nEnv = filterHist pos env
                        interpreterLoop program nEnv lineNum
        otherwise -> do
                        interpreterLoop program env lineNum
    'i' -> do
      let varName = drop 2 input
      let Right i = runEval (snd $ head env) (inspect varName)
      print i
      interpreterLoop program env lineNum
    'h' -> do
      let varName = drop 2 input
      history varName env
      interpreterLoop program env lineNum
    'q' -> do
      runQuit
    otherwise -> interpreterLoop program env lineNum

readProgram :: IO ([Statement])
readProgram = do
  contents <- readFile "src/program.txt"
  let expressions = read contents :: [Statement]
  return expressions

runQuit :: IO (Either String ((), [(Int, Env)]))
runQuit = runRun (exec Pass 0) [(0, Map.empty)]

evaluate :: [Statement] -> Int -> Run ()
evaluate st line = do
  liftIO $ print (st !! line)
  exec (st !! line) line

inspect :: String -> Eval Val
inspect name = do
  env <- ask
  lookup name env

history :: String -> [(Int, Env)] -> IO ()
history name env = printHist name $ head (mapM (\(i,e) -> do
    let l = Map.lookup name e
    case l of
      Just v -> return (i,v)
      Nothing -> return (i,Nil)) env)

filterHist :: Int -> [(Int, Env)] -> [(Int, Env)]
filterHist line env = filter (\(i,_) -> i <= line) env

printHist :: String -> [(Int, Val)] -> IO ()
printHist name env = do
  putStrLn $ "History " ++ name ++ ": \n"
  mapM_ (\(i,v) -> putStrLn $ "Line " ++ (show i) ++ " -> " ++ (show v)) env
