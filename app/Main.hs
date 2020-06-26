module Main where

import Data.Maybe
import Eval
import Lib
import System.Environment
import System.Exit
import Text.Printf

findNothing :: [Maybe a] -> Bool
findNothing [] = False
findNothing (Nothing : rest) = True
findNothing (_ : xs) = findNothing xs

roundRes :: Float -> Float
roundRes d
    | dp > 0 = if dp1 - dp > (4 * 10 ^^ (-3)) && dp1 - dp < (5 * 10 ^^ (-3)) then dp + 0.01 else dp
    | dp < 0 = if dp1 - dp < (-4 * 10 ^^ (-3)) && dp1 - dp > (-5 * 10 ^^ (-3)) then dp - 0.01 else dp
    | otherwise = 0
    where
        dp = fromInteger (round (d * 10 ^ 2)) / 10 ^ 2
        dp1 = fromInteger (round (d * 10 ^ 3)) / 10 ^ 3

main :: IO ()
main = do
    args <- getArgs
    case length args of
        1 -> do
            let
                noSpaceStr = concat $ words (head args)
                tokens = lexer noSpaceStr
            if findNothing tokens
                then exitWith (ExitFailure 84)
                else case evalExpr (catMaybes tokens) of
                    Just res -> printf "%.2f\n" (roundRes res)
                    Nothing -> exitWith (ExitFailure 84)
        _ -> exitWith (ExitFailure 84)
