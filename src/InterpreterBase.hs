-- I want these language extensions for my syntactic sugaring tricks at the end

{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

-- I want my own definition of lookup and I want to write my own function
-- named "print".
module InterpreterBase where
import Prelude hiding (lookup, print)

import qualified Data.Map as Map
import Data.Maybe

-- I want to get at the standard "print" function using the name System.print

import qualified System.IO as System

-- I plan to use these monads to construct the parts of my interpreter
import Control.Monad
  (when)
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

{-------------------------------------------------------------------}
{- The pure expression language                                    -}
{-------------------------------------------------------------------}

data Val = I Int | B Bool | Nil
           deriving (Eq, Show, Read)

data Expr = Const Val
     | Add Expr Expr  | Sub Expr Expr   | Mul Expr Expr   | Div Expr Expr
     | And Expr Expr  | Or Expr Expr    | Not Expr
     | Eq Expr Expr   | Gt Expr Expr    | Lt Expr Expr
     | Var String
   deriving (Eq, Show, Read)

type Name = String
type Env = Map.Map Name Val

lookup :: Monad m => String -> Map.Map String a -> m a
lookup k t = case Map.lookup k t of
               Just x   -> return x
               Nothing  -> fail ("Unknown variable " ++ k)

{-- Monadic style expression evaluator,
 -- with error handling and Reader monad instance to carry dictionary
 --}

type Eval a = ReaderT Env (ExceptT String Identity) a

runEval :: Env -> Eval a -> Either String a
runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )

-- This evaluator could be a little neater

-- Integer typed expressions
evali :: (Int -> Int -> Int) -> Expr -> Expr -> Eval Val
evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (I i0, I i1) -> return $ I (i0 `op` i1)
                         _            -> fail "type error in arithmetic expression"

-- Boolean typed expressions
evalb :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Eval Val
evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (B i0, B i1) -> return $ B (i0 `op` i1)
                         _            -> fail "type error in boolean expression"

-- Operations over integers which produce booleans
evalib :: (Int -> Int -> Bool) -> Expr -> Expr -> Eval Val
evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (I i0, I i1) -> return $ B (i0 `op` i1)
                          _            -> fail "type error in arithmetic expression"

-- Evaluate an expression

eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = do evali (+) e0 e1
eval (Sub e0 e1) = do evali (-) e0 e1
eval (Mul e0 e1) = do evali (*) e0 e1
eval (Div e0 e1) = do evali div e0 e1

eval (And e0 e1) = do evalb (&&) e0 e1
eval (Or e0 e1) = do evalb (||) e0 e1

eval (Not e0  ) = do evalb (const not) e0 (Const (B True))
                       where not2 a _ = not a -- hack, hack

eval (Eq e0 e1) = do evalib (==) e0 e1
eval (Gt e0 e1) = do evalib (>) e0 e1
eval (Lt e0 e1) = do evalib (<) e0 e1

eval (Var s) = do env <- ask
                  lookup s env


{-------------------------------------------------------------------}
{- The statement language                                          -}
{-------------------------------------------------------------------}

data Statement = Assign String Expr
              | If Expr Statement Statement
              | While Expr Statement
              | Print Expr
              | Seq Statement Statement
              | Try Statement Statement
              | Pass
     deriving (Eq, Show, Read)

{-
  This Run monadic transformer is taken from the lecture notes, and adapted
  so that the state stores a list of tuples contain an int representing a
  line of code and the associated environment.
-}

type Run a = StateT [(Int, Env)] (ExceptT String IO) a

-- Interpret the run monad transformer.
runRun :: Run a -> [(Int, Env)] -> IO (Either String (a, [(Int, Env)]))
runRun p env = runExceptT (runStateT p env)

-- Add a line, environment tuple to the state.
set :: (Name, Val, Int) -> Run ()
set (s, i, l) = state $ (\table -> ((), (l, Map.insert s i (snd $ head table)):table))

-- Execute a statement.
exec :: Statement -> Int -> Run ()
exec (Assign s v) l = do
                            st <- get
                            Right val <- return $ runEval (snd $ head st) (eval v)
                            set (s,val,l)

exec (If cond s0 s1) l = do
                            st <- get
                            Right (B val) <- return $ runEval (snd $ head st) (eval cond)
                            if val then do exec s0 l else do exec s1 l

exec (While cond s) l = do
                            st <- get
                            Right (B val) <- return $ runEval (snd $ head st) (eval cond)
                            when val $ exec s l >> exec (While cond s) l

exec (Print e) l = do
                            st <- get
                            Right (val) <- return $ runEval (snd $ head st) (eval e)
                            liftIO $ System.print val
                            return ()

exec (Seq s0 s1)  l = do    exec s0 l >> exec s1 l
exec (Try s0 s1)  l = do    catchError (exec s0 l) (\e -> exec s1 l)
exec Pass         _ =       return ()
