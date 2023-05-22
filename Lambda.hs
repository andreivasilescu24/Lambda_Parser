module Lambda where

import Expr
import Data.List

-- TODO 1.1. find free variables of a Expr

-- free_vars_aux :: Expr -> [String] -> [String]
-- free_vars_aux x list = case x of
--                         (Variable x) -> (x:list)
--                         (Function x expression) -> free_vars_aux (x:xs) expression
--                         (Application e1 e2) -> (free_vars_aux e1 list) || (free_vars_aux e2 list)

free_vars :: Expr -> [String]
free_vars x = case x of
                (Variable y) -> [y]
                (Function y expression) -> y `delete` free_vars expression
                (Application e1 e2) -> union (free_vars e1) (free_vars e2)

                

-- TODO 1.2. reduce a redex
reduce :: Expr -> String -> Expr -> Expr
reduce = undefined

-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN = undefined

-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN = undefined

reduceAllN :: Expr -> [Expr]
reduceAllN = undefined

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA = undefined

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA = undefined

reduceAllA :: Expr -> [Expr]
reduceAllA = undefined

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros = undefined

-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode = undefined
