module Lambda where

import Expr
import Data.List

free_vars :: Expr -> [String]
free_vars x = case x of
                (Variable y) -> [y]
                (Function y expression) -> y `delete` free_vars expression
                (Application e1 e2) -> union (free_vars e1) (free_vars e2)


reduce :: Expr -> String -> Expr -> Expr
reduce expr x replacement =
    case expr of
        (Variable v) -> if v == x then replacement else Variable v
        (Function param body) -> if param == x then Function param body 
                                    else if param `notElem` free_vars replacement 
                                                    then Function param $ reduce body x replacement
                                                else reduce (Function "a" $ reduce body param (Variable "a")) x replacement 

        (Application expr1 expr2) -> Application (reduce expr1 x replacement) (reduce expr2 x replacement)


-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN expression =
    case expression of
        (Variable x) -> Variable x
        (Function x e) -> Function x $ stepN e
        (Application (Function x e1) e2) -> reduce e1 x e2
        (Application (Application e' e'') e2) -> Application (stepN (Application e' e'')) e2
        (Application (Variable y) e2) -> Application (Variable y) (stepN e2)

-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN expression 
    | expression == stepN expression = expression
    | otherwise = reduceN $ stepN expression

reduceAllN :: Expr -> [Expr]
reduceAllN expression
    | expression == stepN expression = [expression]
    | otherwise = expression : reduceAllN (stepN expression)

-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA expression = 
    case expression of
        (Variable x) -> Variable x
        (Function x e) -> Function x $ stepA e
        (Application (Application e2 e3) e1) -> Application (stepA $ Application e2 e3) e1
        (Application e1 (Application e2 e3)) -> Application e1 $ stepA (Application e2 e3)
        (Application (Function x e1) e2) -> reduce e1 x $ stepA e2
        (Application (Variable y) e2) -> Application (Variable y) (stepA e2)

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA expression
    | expression == stepA expression = expression
    | otherwise = reduceA $ stepA expression

reduceAllA :: Expr -> [Expr]
reduceAllA expression
    | expression == stepA expression = [expression]
    | otherwise = expression : reduceAllA (stepA expression)

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros list expr =
     



-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode = undefined
