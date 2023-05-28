module Lambda where

import Expr
import Data.List

free_vars :: Expr -> [String]
free_vars x = case x of
                (Variable y) -> [y]
                (Function y expression) -> y `delete` free_vars expression
                (Application e1 e2) -> (free_vars e1) `union` (free_vars e2)


unusedVariableSymbol :: [String] -> [Char] -> Char
unusedVariableSymbol vars chars = head $ filter (\c -> [c] `notElem` vars) chars

reduce :: Expr -> String -> Expr -> Expr
reduce expr x replacement =
    case expr of
        (Variable v) -> if v == x then replacement else Variable v
        
        (Function param body) ->
            if param == x then Function param body
            
            else if param `notElem` free_vars replacement then Function param $ reduce body x replacement
                
                else reduce (Function [unusedChar] $ reduce body param (Variable [unusedChar])) x replacement
                    where 
                        vars = free_vars replacement `union` free_vars body
                        unusedChar = unusedVariableSymbol vars ['a'..'z']
        
        (Application expr1 expr2) -> Application (reduce expr1 x replacement) (reduce expr2 x replacement)

-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN expression =
    case expression of
        (Variable x) -> Variable x
        (Function x e) -> Function x $ stepN e
        (Application (Function x e1) e2) -> reduce e1 x e2
        (Application (Application e1 e2) e3) -> Application (stepN (Application e1 e2)) e3
        (Application (Variable x) e) -> Application (Variable x) (stepN e)

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
        (Application (Application e1 e2) e3) -> Application (stepA $ Application e1 e2) e3
        (Application e1 (Application e2 e3)) -> Application e1 $ stepA (Application e2 e3)
        (Application (Function x e1) e2) -> reduce e1 x $ stepA e2
        (Application (Variable x) e) -> Application (Variable x) (stepA e)

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
evalMacros dict expr =
    case expr of
        (Variable x) -> Variable x
        (Function x e) -> Function x $ evalMacros dict e
        (Application e1 e2) -> Application (evalMacros dict e1) (evalMacros dict e2)
        (Macro x) -> case lookup x dict of
                        (Just value) ->
                            case value of
                                (Macro y) -> evalMacros dict value
                                _ -> value
                        Nothing -> error ("couldn't find Macro")



-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode eval_strategy code = aux_code eval_strategy code []
        where
            aux_code :: (Expr -> Expr) -> [Code] -> [(String, Expr)] -> [Expr]
            aux_code eval_strategy code dict =
                case code of
                    [] -> []
                    (Evaluate expr) : xs -> (eval_strategy $ evalMacros dict expr) : (aux_code eval_strategy xs dict)
                    (Assign s expr) : xs -> 
                        case lookup s dict of
                            (Just ex_expr) -> aux_code eval_strategy xs ((s, expr) : (delete (s, ex_expr) dict))
                            Nothing -> aux_code eval_strategy xs ((s, expr) : dict)
