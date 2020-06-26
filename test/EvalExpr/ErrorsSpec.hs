module EvalExpr.ErrorsSpec where

import Data.Maybe
import SpecHelper

spec :: Spec
spec = do
    describe "Error management: Division by 0" $ do
        context "with 3 / 0" $ do
            it "Nothing" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 / 0"
                evalExpr tokens `shouldBe` Nothing
    describe "Error management: Empty parenthesis" $ do
        context "with ()" $ do
            it "Nothing" $ do
                let tokens = catMaybes $ lexer $ concat $ words "()"
                evalExpr tokens `shouldBe` Nothing
    describe "Error management: Empty string" $ do
        context "with \"\"" $ do
            it "Nothing" $ do
                let tokens = catMaybes $ lexer $ concat $ words ""
                evalExpr tokens `shouldBe` Nothing
    describe "Error management: Illegal character" $ do
        context "with 3 + j" $ do
            it "Nothing" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 + j"
                evalExpr tokens `shouldBe` Nothing
    describe "Error management: Missing number" $ do
        context "with 3 + " $ do
            it "Nothing" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 + "
                evalExpr tokens `shouldBe` Nothing
    describe "Error management: Missing parenthesis" $ do
        context "with (3 + 3" $ do
            it "Nothing" $ do
                let tokens = catMaybes $ lexer $ concat $ words "(3 + 3"
                evalExpr tokens `shouldBe` Nothing
    describe "Error management: Reverse parentheses" $ do
        context "with )3 + 3(" $ do
            it "Nothing" $ do
                let tokens = catMaybes $ lexer $ concat $ words ")3 + 3("
                evalExpr tokens `shouldBe` Nothing
    describe "Error management: Enchained operators" $ do
        context "with 3 **** 3" $ do
            it "Nothing" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 **** 3"
                evalExpr tokens `shouldBe` Nothing

main :: IO ()
main = hspec spec