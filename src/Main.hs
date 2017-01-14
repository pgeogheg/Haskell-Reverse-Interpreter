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
History:

-----------------------------------------}

{- Previous Version

type History = IORef (Map.Map Name ([(Int, Val)]))

newHistory :: IO (History)
newHistory = newIORef Map.empty

addHist :: History -> Env -> Name -> Int -> IO ()
addHist hist env name line = do
  table <- readIORef hist
  let Right val = runEval env (inspect name)
  let newHist = Map.insert name [(line, val)] table
  writeIORef hist newHist

backUpdateHist :: History -> Int -> Env -> IO Env
backUpdateHist hist line env = do
  table <- readIORef hist
  let list = Map.toList table
  let nList = fmap (filterEntries . snd) list
  let names = fmap fst list
  let newHist = zip names nList
  writeIORef hist (Map.fromList newHist)
  adjustVars hist env where
    filterEntries :: [(Int, Val)] -> [(Int, Val)]
    filterEntries list = do
      filter (\(n,_) -> n <= line) list

    adjustVars :: History -> Env -> IO Env
    adjustVars hist env = do
      Right (_, variables) <- runRun (getEnvironment) env
      let variableList = Map.toList variables
      print "VARIABLE LIST FOR ADJUSTING"
      print variableList
      nEnvs <- mapM (\(x,_) -> backUpdate hist x env) variableList
      return $ last nEnvs

    traverseVarList :: History -> [(String, Int)] -> Env -> IO Env
    traverseVarList hist [(name,_)] env = do
      backUpdate hist name env
    traverseVarList hist ((name,_):xs) env = do
      nEnv <- backUpdate hist name env
      traverseVarList hist xs nEnv

    backUpdate :: History -> String -> Env -> IO Env
    backUpdate hist name env = do
      print $ "ADJUSTING " ++ name
      histTable <- readIORef hist
      case (Map.lookup name histTable) of
        Nothing -> do
          print "ASSIGNING NIL"
          Right (_, nEnv) <- runRun (exec $ Assign name (Const Nil)) env
          return nEnv
        Just v -> do
          case v of
            [] -> do
              print "ASSIGNING NIL"
              Right (_, nEnv) <- runRun (exec $ Assign name (Const Nil)) env
              return nEnv
            otherwise -> do
              print $ "ASSIGNING " ++ show (snd (head v))
              Right (_, nEnv) <- runRun (exec $ Assign name (Const (snd $ head v))) env
              return nEnv


updateHist :: History -> Env -> Int -> IO ()
updateHist hist env line = do
  Right (_, variables) <- runRun (getEnvironment) env
  let variableList = Map.toList variables
  print "VARIABLE LIST"
  print variableList
  mapM_ (\(x,y) -> updateEntries hist x env line y) variableList where
    updateEntries :: History -> String -> Env -> Int -> Val -> IO ()
    updateEntries hist name env line val = do
      old <- readIORef hist
      let newH = Map.insertWith (++) name [(line, val)] old
      writeIORef hist newH

filterHist :: History -> String -> IO [(Int, Val)]
filterHist hist name = do
  table <- readIORef hist
  print table
  lookup name table

getEnvironment :: Run ()
getEnvironment = do
  st <- get
  return ()

-}


{-----------------------------------------
Main Interpreter Behaviour:

-----------------------------------------}

main :: IO ()
main = do
  program         <- readProgram
  lineNum         <- newNum $ length program
  hist            <- newHistory
  pos             <- now lineNum
  Right (_, env)  <- runRun (evaluate program pos hist) Map.empty
  updateHist hist env pos
  interpreterLoop program env lineNum hist
  --runRun (inspect "var1") env
  putStrLn "DONE!"

interpreterLoop :: [Statement] -> Env -> LineNumber -> History -> IO (Either String ((), Env))
interpreterLoop program env lineNum hist = do
  putStr "Input: "
  input <- getLine
  case (head input) of
    'n' -> do
      limitCheck <- nextLine lineNum
      case limitCheck of
        False     -> do
                        pos <- now lineNum
                        putStr $ "\nRUNNING LINE: " ++ (show pos) ++ "\n"
                        Right (_, nEnv) <- runRun (evaluate program pos hist) env
                        updateHist hist env pos
                        interpreterLoop program nEnv lineNum hist
        otherwise -> do
                        interpreterLoop program env lineNum hist
    'b' -> do
      limitCheck <- prevLine lineNum
      case limitCheck of
        False     -> do
                        pos <- now lineNum
                        nEnv <- backUpdateHist hist pos env
                        putStr $ "\nRUNNING LINE: " ++ (show pos) ++ "\n"
                        --Right (_, nEnv) <- runRun (evaluate program pos hist) env
                        interpreterLoop program nEnv lineNum hist
        otherwise -> do
                        interpreterLoop program env lineNum hist
    'i' -> do
      let varName = drop 2 input
      let Right i = runEval env (inspect varName)
      print i
      interpreterLoop program env lineNum hist
    'h' -> do
      let varName = drop 2 input
      h <- filterHist hist varName
      print h
      interpreterLoop program env lineNum hist
    'q' -> do
      runQuit
    otherwise -> interpreterLoop program env lineNum hist

readProgram :: IO ([Statement])
readProgram = do
  contents <- readFile "src/program.txt"
  let expressions = read contents :: [Statement]
  return expressions

runQuit :: IO (Either String ((), Env))
runQuit = runRun (exec Pass) Map.empty

evaluate :: [Statement] -> Int -> History -> Run ()
evaluate st line hist = do
  liftIO $ print (st !! line)
  exec (st !! line)

inspect :: String -> Eval Val
inspect name = do
  env <- ask
  lookup name env

{- TEST FUNCTIONS -}
exampleExpr = (Const (I 6) `Add` (Const (I 10) `Add` Const (I 5)))
test = runEval (Map.empty) (eval exampleExpr)
