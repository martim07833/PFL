{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Parsing
import Data.Char
import Foreign.C (e2BIG)
import Distribution.Simple.Utils (xargs)
import Data.Void (vacuous)
import Data.Graph (outdegree)

type Name = String
type Env = [(Name, Integer)]
--
-- a data type for expressions
-- made up from integer numbers, + and *
--

updateEnv :: Name -> Integer -> Env -> Env 
updateEnv n i [] = [(n, i)]
updateEnv n i ((x,v) : xs)
  | n == x = (x,i) : xs
  | otherwise = (x,v) : updateEnv n i xs



data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Rem Expr Expr
          | Var Name
          deriving Show

data Command 
  = Eval Expr
  | Assign Name Expr
  deriving Show

-- a recursive evaluator for expressions
--
eval :: Expr -> Integer
eval (Num n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Div e1 e2) = eval e1 `div` eval e2
eval (Rem e1 e2) = eval e1 `rem` eval e2

eval2 :: Env -> Expr -> Integer
eval2 env (Num n) = n
eval2 env (Add e1 e2) = eval2 env e1 + eval2 env e2
eval2 env (Mul e1 e2) = eval2 env e1 * eval2 env e2
eval2 env (Sub e1 e2) = eval2 env e1 - eval2 env e2
eval2 env (Div e1 e2) = eval2 env e1 `div` eval2 env e2
eval2 env (Rem e1 e2) = eval2 env e1 `rem` eval2 env e2
eval2 env (Var n) =
    case lookup n env of
      Just v -> v
      Nothing -> error ("undefined variable: " ++ n)


-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')'

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = (do char '+'
                   t <- term
                   exprCont (Add acc t))
               <|> 
               (do char '-'
                   t <- term
                   exprCont (Sub acc t))
               <|> return acc

term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc =
  (do { char '*'; f <- factor; termCont (Mul acc f) })
  <|> (do { char '/'; f <- factor; termCont (Div acc f) })
  <|> (do { char '%'; f <- factor; termCont (Rem acc f) })
  <|> return acc

factor :: Parser Expr
factor = (do n <- natural
             return (Num n))
         <|>
         (do v <- variable
             return (Var v))
         <|>
         (do char '('
             e <- expr
             char ')'
             return e)
             

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

variable :: Parser Name
variable = do xs <- many1 (satisfy isAlpha)
              return xs 

command :: Parser Command 
command = do v <- variable


----------------------------------------------------------------             
  
main :: IO ()
main
  = do txt <- getContents
       calculator (lines txt)

-- | read-eval-print loop
calculator :: [String] -> IO ()
calculator []  = return ()
calculator (l:ls) = do putStrLn (evaluate l)
                       calculator ls  

calculator2 :: Env -> [String] -> IO ()
calculator2 env [] = return ()
calculator2 env (l:ls) = 
  let (out, env') = execute env l
  in do
      putStrLn out
      calculator2 env' ls

-- | evaluate a single expression
evaluate :: String -> String
evaluate txt
  = case parse expr txt of
execute :: Env -> String -> (String, Env)
execute env txt =
    case parse command txt of
      [(Eval e, "")] ->
        (show (eval2 env e), env)
      [(Assign v e, "")] ->
        let val  = eval2 env e
            env' = updateEnv v val env
        in (show val, env')
      _ ->
        ("parse error; try again", env)
            in (show val , env')
        _ ->
            ("parse error; try again", env)
