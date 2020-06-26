{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Applicative
import Data.Char
import Text.Read

newtype TkParser a = TkParser {
    parse :: [Token] -> Maybe (a, [Token])
}

data Expr
        = Lit   Float
        | Add   Expr Expr
        | Sub   Expr Expr
        | Mul   Expr Expr
        | Div   Expr Expr
        | Pow   Expr Expr
        | Neg   Expr
        deriving (Show)

data Token
        = AddTok
        | SubTok
        | MulTok
        | DivTok
        | NegTok
        | PosTok
        | PowTok
        | LParTok
        | RParTok
        | LitTok Float
        deriving (Eq, Show)

instance Functor TkParser where
    fmap func (TkParser psr) = TkParser $ \tok -> case psr tok of
        Just (a, tok') -> Just (func a, tok')
        Nothing -> Nothing

instance Applicative TkParser where
    pure x = TkParser $ \s -> Just (x, s)
    (TkParser p1) <*> (TkParser p2) = TkParser $ \tok -> case p1 tok of
        Just (func, tok') -> case p2 tok' of
            Just (a, s'') -> Just (func a, s'')
            Nothing       -> Nothing
        Nothing -> Nothing

instance Alternative TkParser where
    empty = TkParser $ const Nothing
    (TkParser p1) <|> (TkParser p2) = TkParser $ \tok -> p1 tok <|> p2 tok

instance Monad TkParser where
    return x = TkParser (\tok -> Just (x, tok))
    (TkParser psr) >>= f = TkParser $ \tok -> case psr tok  of
        Just (a, restTok) -> parse (f a) restTok
        Nothing -> Nothing

lexer :: String -> [Maybe Token]
lexer = lexerHelper 0

lexerHelper :: Int -> String -> [Maybe Token]
lexerHelper i [] = []
lexerHelper 0 ('-': restStr) = Just NegTok : lexerHelper 1 restStr
lexerHelper 0 ('+': restStr) = Just PosTok : lexerHelper 1 restStr
lexerHelper i ('+' : '-' : restStr) = Just AddTok : Just NegTok : lexerHelper (i + 1) restStr
lexerHelper i ('-' : '-' : restStr) = Just SubTok : Just NegTok : lexerHelper (i + 1) restStr
lexerHelper i ('*' : '-' : restStr) = Just MulTok : Just NegTok : lexerHelper (i + 1) restStr
lexerHelper i ('^' : '-' : restStr) = Just PowTok : Just NegTok : lexerHelper (i + 1) restStr
lexerHelper i ('/' : '-' : restStr) = Just DivTok : Just NegTok : lexerHelper (i + 1) restStr
lexerHelper i ('(' : '-' : restStr) = Just LParTok : Just NegTok : lexerHelper (i + 1) restStr
lexerHelper i ('+' : '+' : restStr) = Just AddTok : Just PosTok : lexerHelper (i + 1) restStr
lexerHelper i ('-' : '+' : restStr) = Just SubTok : Just PosTok : lexerHelper (i + 1) restStr
lexerHelper i ('*' : '+' : restStr) = Just MulTok : Just PosTok : lexerHelper (i + 1) restStr
lexerHelper i ('^' : '+' : restStr) = Just PowTok : Just PosTok : lexerHelper (i + 1) restStr
lexerHelper i ('/' : '+' : restStr) = Just DivTok : Just PosTok : lexerHelper (i + 1) restStr
lexerHelper i ('(' : '+' : restStr) = Just LParTok : Just PosTok : lexerHelper (i + 1) restStr
lexerHelper i ('+' : restStr) = Just AddTok : lexerHelper (i + 1) restStr
lexerHelper i ('-' : restStr) = Just SubTok : lexerHelper (i + 1) restStr
lexerHelper i ('*' : restStr) = Just MulTok : lexerHelper (i + 1) restStr
lexerHelper i ('^' : restStr) = Just PowTok : lexerHelper (i + 1) restStr
lexerHelper i ('/' : restStr) = Just DivTok : lexerHelper (i + 1) restStr
lexerHelper i ('(' : restStr) = Just LParTok : lexerHelper (i + 1) restStr
lexerHelper i (')' : restStr) = Just RParTok : lexerHelper (i + 1) restStr
lexerHelper i str@(chr : restStr) | isDigit chr  = case readMaybe floatStr of
    Just num -> Just (LitTok num) : lexerHelper (i + 1) restStr'
    Nothing -> Nothing : lexerHelper (i + 1)  restStr
    where (floatStr, restStr') = span (`elem` ".0123456789") str
lexerHelper i (_ : restStr) = Nothing : lexerHelper (i + 1)  restStr

satisfyTk :: (Token -> Bool) -> TkParser Token
satisfyTk pred = TkParser $ \case
    (x : xs) | pred x -> Just (x, xs)
    _                 -> Nothing

token :: Token -> TkParser Token
token tok = satisfyTk (== tok)

chainl :: TkParser a -> TkParser (a -> a -> a) -> TkParser a
psr `chainl` optr = do {a <- psr; rest a}
    where rest a = do {costr <- optr; b <- psr; rest $ costr a b} <|> return a

unaryOp :: (Expr -> Expr) -> Token -> TkParser Expr -> TkParser Expr
unaryOp costr optr psr = costr <$> (token optr *> psr)

parseLit :: TkParser Expr
parseLit = TkParser $ \case
    (LitTok n : restTok) -> Just (Lit n, restTok)
    _ -> Nothing

parseParLit :: TkParser Expr
parseParLit = parseNegPar <|> parsePar <|> parseNegLit <|> parsePosLit <|> parseLit
    where
        parseNegPar = unaryOp Neg NegTok parsePar
        parsePar = token LParTok *> expr <* token RParTok
        parseNegLit = unaryOp Neg NegTok parseLit
        parsePosLit = token PosTok *> parseLit

parsePow :: TkParser (Expr -> Expr -> Expr)
parsePow = infixOp Pow PowTok

parseProdQuot :: TkParser (Expr -> Expr -> Expr)
parseProdQuot = infixOp Mul MulTok <|> infixOp Div DivTok

parseSumDiff :: TkParser (Expr -> Expr -> Expr)
parseSumDiff = infixOp Add AddTok <|> infixOp Sub SubTok

infixOp :: (a -> a -> a) -> Token -> TkParser (a -> a -> a)
infixOp f x = token x >> return f

expr :: TkParser Expr
expr = term `chainl` parseSumDiff

term :: TkParser Expr
term = power `chainl` parseProdQuot

power :: TkParser Expr
power = parseParLit `chainl` parsePow

runParser :: TkParser a -> [Token] -> Maybe a
runParser (TkParser p) toks = case p toks of
    Just (r, []) -> Just r
    _            -> Nothing