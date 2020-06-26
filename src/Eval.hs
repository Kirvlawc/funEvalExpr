module Eval
    (evalExpr
    ) where

import Lib

evalAdd :: Maybe Float -> Maybe Float -> Maybe Float
evalAdd Nothing _ = Nothing
evalAdd _ Nothing = Nothing
evalAdd (Just a) (Just b) = Just (a + b)

evalSub :: Maybe Float -> Maybe Float -> Maybe Float
evalSub Nothing _ = Nothing
evalSub _ Nothing = Nothing
evalSub (Just a) (Just b) = Just (a - b)

evalMul :: Maybe Float -> Maybe Float -> Maybe Float
evalMul Nothing _ = Nothing
evalMul _ Nothing = Nothing
evalMul (Just a) (Just b) = Just (a * b)

evalDiv :: Maybe Float -> Maybe Float -> Maybe Float
evalDiv Nothing _ = Nothing
evalDiv _ Nothing = Nothing
evalDiv (Just a) (Just 0) = Nothing
evalDiv (Just a) (Just b) = Just (a / b)

evalPow :: Maybe Float -> Maybe Float -> Maybe Float
evalPow Nothing _ = Nothing
evalPow _ Nothing = Nothing
evalPow (Just a) (Just b) = Just (a ** b)

eval :: Expr -> Maybe Float
eval expr = case expr of
    Add a b -> evalAdd (eval a) (eval b)
    Sub a b -> evalSub (eval a) (eval b)
    Mul a b -> evalMul (eval a) (eval b)
    Div a b -> evalDiv (eval a) (eval b)
    Pow a b -> evalPow (eval a) (eval b)
    Neg a   -> evalMul (Just (-1.0)) (eval a)
    Lit n   -> Just n

evalExpr :: [Token] -> Maybe Float
evalExpr tokens = eval =<< runParser expr tokens