module Main
  ( main
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict
  ( StateT
  , modify'
  , gets
  , runStateT
  )
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)

import qualified Data.Map.Strict as Map

-----------------------------------------
-- Data Types
--

type Id = String

data Stm =
    CompoundStm Stm Stm
  | AssignStm Id Exp
  | PrintStm [Exp]

data Exp =
    IdExp Id
  | NumExp Int
  | OpExp Exp Binop Exp
  | EseqExp Stm Exp

data Binop = Plus | Minus | Times | Div


-----------------------------------------
-- Interpreter state manipulators
--

type Bindings = Map Id Int

updateBindings :: Id -> Int -> Bindings -> Bindings
updateBindings = Map.insert
{-# INLINE updateBindings #-}

newBindings :: Bindings
newBindings = Map.empty
{-# INLINE newBindings #-}

-- Will throw if var is not found
lookupBindings :: Id -> Bindings -> Int
lookupBindings var map = fromJust $ Map.lookup var map
{-# INLINE lookupBindings #-}


-----------------------------------------
-- Main
--

main :: IO ()
main = do
  putStrLn "Max args:"
  putStrLn . show $ maxargs prog
  putStrLn "\nInterpreting program:"
  interp prog


-----------------------------------------
-- Exercises (interp and maxargs)
--

-- | Interprets (runs) the given program represented
-- by the AST stm.  Variable bindings are encapsulated
-- by the StateT monad.
interp :: Stm -> IO ()
interp stm = runStateT (interpS stm) newBindings >> return ()


-- | Auxiliary function for interp; interprets the statement
-- rooted at stm in a specific context (i.e., the context of
-- current variable bindings).
interpS :: Stm -> StateT Bindings IO ()
interpS stm = case stm of
  (CompoundStm a b) -> interpS a >> interpS b

  (AssignStm var e) -> do
    eVal <- interpE e
    modify' (updateBindings var eVal)

  (PrintStm es) -> do
    forM_ es $ \e -> do
      eVal <- interpE e
      liftIO . putStr $ show eVal ++ " "
    liftIO $ putStrLn ""


-- | Auxiliary function for interp; interprets the expression
-- rooted at e in a specific context (i.e., the context of
-- current variable bindings).
interpE :: Exp -> StateT Bindings IO Int
interpE e = case e of
  (IdExp var) -> gets $ lookupBindings var
   
  (NumExp n) -> return n 

  (EseqExp s e) -> interpS s >> interpE e

  (OpExp e1 op e2) -> do
    e1Val <- interpE e1
    e2Val <- interpE e2
    let op' = getop op
    return $ op' e1Val e2Val


-- | Mapping between Binop and the corresponding
-- binary operator (function).
getop :: Binop -> (Int -> Int -> Int)
getop op = case op of
  Plus -> (+)
  Minus -> (-)
  Times -> (*)
  Div -> div


-- | Finds the maximum number of arguments to any
-- PrintStm found anywhere in the AST rooted at stm.
maxargs :: Stm -> Int
maxargs stm = case stm of
  (CompoundStm a b) -> max (maxargs a) (maxargs b)
  (AssignStm _ e) -> maxargsE e
  (PrintStm es) -> maximum $ length es : map maxargsE es

maxargsE :: Exp -> Int
maxargsE e = case e of
  (OpExp e1 _ e2) -> max (maxargsE e1) (maxargsE e2)
  (EseqExp s e) -> max (maxargs s) (maxargsE e)
  _ -> 0
  

-----------------------------------------
-- Sample Programs
--

prog :: Stm
prog =
  CompoundStm
    (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
    (CompoundStm
      (AssignStm
         "b"
         (EseqExp
           (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
           (OpExp (NumExp 10) Times (IdExp "a"))))
      (PrintStm [IdExp "b"]))
