module EvalExpr.UnarySpec where

import Data.Maybe
import SpecHelper

spec :: Spec
spec = do
    describe "Unary Operators: Negative with blank" $ do
        context "with - 3" $ do
            it "Just (-3.00)" $ do
                let tokens = catMaybes $ lexer $ concat $ words "- 3"
                evalExpr tokens `shouldBe` Just (-3.00)
    describe "Unary Operators: Negative no spaces" $ do
        context "with -3" $ do
            it "Just (-3.00)" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-3"
                evalExpr tokens `shouldBe` Just (-3.00)
    describe "Unary Operators: Number with blank" $ do
        context "with \" 3 \"" $ do
            it "Just 3.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words " 3 "
                evalExpr tokens `shouldBe` Just 3.00
    describe "Unary Operators: Integer no spaces" $ do
        context "with 3" $ do
            it "Just 3.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3"
                evalExpr tokens `shouldBe` Just 3.00
    describe "Unary Operators: Float no spaces" $ do
        context "with 5.345" $ do
            it "Just 5.345" $ do
                let tokens = catMaybes $ lexer $ concat $ words "5.345"
                evalExpr tokens `shouldBe` Just 5.345
    describe "Unary Operators: Power with space" $ do
        context "with 2^ 2" $ do
            it "Just 4.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "2^ 2"
                evalExpr tokens `shouldBe` Just 4.00
    describe "Unary Operators: Power with spaces" $ do
        context "with 2 ^ 2" $ do
            it "Just 4.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "2 ^ 2"
                evalExpr tokens `shouldBe` Just 4.00
    describe "Unary Operators: Power: Negative exponent" $ do
        context "with 2^-2" $ do
            it "Just 0.25" $ do
                let tokens = catMaybes $ lexer $ concat $ words "2^-2"
                evalExpr tokens `shouldBe` Just 0.25
    describe "Unary Operators: Power: Negative base" $ do
        context "with -2^2" $ do
            it "Just 4.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-2^2"
                evalExpr tokens `shouldBe` Just 4.00
    describe "Unary Operators: Power: Negative base and exponent" $ do
        context "with -2^2" $ do
            it "Just 0.25" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-2^-2"
                evalExpr tokens `shouldBe` Just 0.25
    describe "Unary Operators: Power: Base with parentheses" $ do
        context "with (1+1)^2" $ do
            it "Just 4.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "(1+1)^2"
                evalExpr tokens `shouldBe` Just 4.00
    describe "Unary Operators: Power: Negative base with parentheses" $ do
        context "with -(1+1)^2" $ do
            it "Just 4.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-(1+1)^2"
                evalExpr tokens `shouldBe` Just 4.00
    describe "Unary Operators: Power: Base with parentheses and negative exponent" $ do
        context "with (1+1)^-2" $ do
            it "Just 0.25" $ do
                let tokens = catMaybes $ lexer $ concat $ words "(1+1)^-2"
                evalExpr tokens `shouldBe` Just 0.25
    describe "Unary Operators: Power: Negative base with parentheses and negative exponent" $ do
        context "with -(1+1)^-2" $ do
            it "Just 0.25" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-(1+1)^-2"
                evalExpr tokens `shouldBe` Just 0.25
    describe "Unary Operators: Power: Base with parentheses and exponent with parentheses" $ do
        context "with (1+1)^(1+1)" $ do
            it "Just 4.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "(1+1)^(1+1)"
                evalExpr tokens `shouldBe` Just 4.00
    describe "Unary Operators: Power: Negative base with parentheses and exponent with parentheses" $ do
        context "with -(1+1)^(1+1)" $ do
            it "Just 4.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-(1+1)^(1+1)"
                evalExpr tokens `shouldBe` Just 4.00
    describe "Unary Operators: Power: Base with parentheses and negative exponent with parentheses" $ do
        context "with (1+1)^-(1+1)" $ do
            it "Just 0.25" $ do
                let tokens = catMaybes $ lexer $ concat $ words "(1+1)^-2"
                evalExpr tokens `shouldBe` Just 0.25
    describe "Unary Operators: Power: Negative base with parentheses and negative exponent with parentheses" $ do
        context "with -(1+1)^2" $ do
            it "Just 0.25" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-(1+1)^-(1+1)"
                evalExpr tokens `shouldBe` Just 0.25
    describe "Unary Operators: Power: Exponent with parentheses" $ do
        context "with 2^(1+1)" $ do
            it "Just 4.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "2^(1+1)"
                evalExpr tokens `shouldBe` Just 4.00
    describe "Unary Operators: Power: Negative base and exponent with parentheses" $ do
        context "with -2^(1+1)" $ do
            it "Just 4.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-2^(1+1)"
                evalExpr tokens `shouldBe` Just 4.00
    describe "Unary Operators: Power: Negative exponent with parentheses" $ do
        context "with 2^-(1+1)" $ do
            it "Just 0.25" $ do
                let tokens = catMaybes $ lexer $ concat $ words "2^-2"
                evalExpr tokens `shouldBe` Just 0.25
    describe "Unary Operators: Power: Negative base and negative exponent with parentheses" $ do
        context "with -2^2" $ do
            it "Just 0.25" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-2^-(1+1)"
                evalExpr tokens `shouldBe` Just 0.25


main :: IO ()
main = hspec spec