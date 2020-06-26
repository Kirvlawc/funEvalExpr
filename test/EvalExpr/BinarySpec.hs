module EvalExpr.BinarySpec where

import Data.Maybe
import SpecHelper

spec :: Spec
spec = do

{--
    -------------------------------------------
    |               Addition                  |
    -------------------------------------------
--}

    describe "Binary Operators: Addition" $ do
        context "with 3 + 3" $ do
            it "Just 6.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 + 3"
                evalExpr tokens `shouldBe` Just 6.00
    describe "Binary Operators: Addition" $ do
        context "with 3 + -3" $ do
            it "Just 0.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 + -3"
                evalExpr tokens `shouldBe` Just 0.00
    describe "Binary Operators: Addition" $ do
        context "with -3 + 3" $ do
            it "Just 0.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-3 + 3"
                evalExpr tokens `shouldBe` Just 0.00
    describe "Binary Operators: Addition" $ do
        context "with -3 + -3" $ do
            it "Just (-6.00)" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-3 + -3"
                evalExpr tokens `shouldBe` Just (-6.00)
    describe "Binary Operators: Addition" $ do
        context "with 3 + +3" $ do
            it "Just 6.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 + +3"
                evalExpr tokens `shouldBe` Just 6.00
    describe "Binary Operators: Addition" $ do
        context "with +3 + 3" $ do
            it "Just 6.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "+3 + 3"
                evalExpr tokens `shouldBe` Just 6.00
    describe "Binary Operators: Addition" $ do
        context "with +3 + +3" $ do
            it "Just 6.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "+3 + +3"
                evalExpr tokens `shouldBe` Just 6.00
    describe "Binary Operators: Addition" $ do
        context "with +3 + -3" $ do
            it "Just 0.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "+3 + -3"
                evalExpr tokens `shouldBe` Just 0.00
    describe "Binary Operators: Addition" $ do
        context "with -3 + +3" $ do
            it "Just 0.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-3 + +3"
                evalExpr tokens `shouldBe` Just 0.00
{--
    -------------------------------------------
    |               Subtraction               |
    -------------------------------------------
--}

    describe "Binary Operators: Subtraction" $ do
        context "with 3 - 3" $ do
            it "Just 0.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 - 3"
                evalExpr tokens `shouldBe` Just 0.00
    describe "Binary Operators: Subtraction" $ do
        context "with 3 - -3" $ do
            it "Just 6.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 - -3"
                evalExpr tokens `shouldBe` Just 6.00
    describe "Binary Operators: Subtraction" $ do
        context "with -3 - 3" $ do
            it "Just (-6.00)" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-3 - 3"
                evalExpr tokens `shouldBe` Just (-6.00)
    describe "Binary Operators: Subtraction" $ do
        context "with -3 - -3" $ do
            it "Just 0.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-3 - -3"
                evalExpr tokens `shouldBe` Just 0.00
    describe "Binary Operators: Subtraction" $ do
        context "with 3 - +3" $ do
            it "Just 0.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 - +3"
                evalExpr tokens `shouldBe` Just 0.00
    describe "Binary Operators: Subtraction" $ do
        context "with +3 - 3" $ do
            it "Just 0.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "+3 - 3"
                evalExpr tokens `shouldBe` Just 0.00
    describe "Binary Operators: Subtraction" $ do
        context "with +3 - +3" $ do
            it "Just 0.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "+3 - +3"
                evalExpr tokens `shouldBe` Just 0.00
    describe "Binary Operators: Subtraction" $ do
        context "with +3 - -3" $ do
            it "Just 6.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "+3 - -3"
                evalExpr tokens `shouldBe` Just 6.00
    describe "Binary Operators: Subtraction" $ do
        context "with -3 + +3" $ do
            it "Just (-6.00)" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-3 - +3"
                evalExpr tokens `shouldBe` Just (-6.00)
{--
    -------------------------------------------
    |               Division                  |
    -------------------------------------------
--}

    describe "Binary Operators: Division" $ do
        context "with 3 / 3" $ do
            it "Just 1.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 / 3"
                evalExpr tokens `shouldBe` Just 1.00
    describe "Binary Operators: Division" $ do
        context "with 3 / -3" $ do
            it "Just (-1.00)" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 / -3"
                evalExpr tokens `shouldBe` Just (-1.00)
    describe "Binary Operators: Division" $ do
        context "with -3 / 3" $ do
            it "Just (-1.00)" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-3 / 3"
                evalExpr tokens `shouldBe` Just (-1.00)
    describe "Binary Operators: Division" $ do
        context "with -3 / -3" $ do
            it "Just 1.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-3 / -3"
                evalExpr tokens `shouldBe` Just 1.00
    describe "Binary Operators: Division" $ do
        context "with 3 / +3" $ do
            it "Just 1.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 / +3"
                evalExpr tokens `shouldBe` Just 1.00
    describe "Binary Operators: Division" $ do
        context "with +3 / 3" $ do
            it "Just 1.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "+3 / 3"
                evalExpr tokens `shouldBe` Just 1.00
    describe "Binary Operators: Division" $ do
        context "with +3 / +3" $ do
            it "Just 1.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "+3 / +3"
                evalExpr tokens `shouldBe` Just 1.00
    describe "Binary Operators: Division" $ do
        context "with +3 / -3" $ do
            it "Just (-1.00)" $ do
                let tokens = catMaybes $ lexer $ concat $ words "+3 / -3"
                evalExpr tokens `shouldBe` Just (-1.00)
    describe "Binary Operators: Division" $ do
        context "with -3 / +3" $ do
            it "Just (-1.00)" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-3 / +3"
                evalExpr tokens `shouldBe` Just (-1.00)

{--
    -------------------------------------------
    |               Multiplication            |
    -------------------------------------------
--}

    describe "Binary Operators: Multiplication" $ do
        context "with 3 * 3" $ do
            it "Just 9.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 * 3"
                evalExpr tokens `shouldBe` Just 9.00
    describe "Binary Operators: Multiplication" $ do
        context "with 3 * -3" $ do
            it "Just (-9.00)" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 * -3"
                evalExpr tokens `shouldBe` Just (-9.00)
    describe "Binary Operators: Multiplication" $ do
        context "with -3 * 3" $ do
            it "Just (-9.00)" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-3 * 3"
                evalExpr tokens `shouldBe` Just (-9.00)
    describe "Binary Operators: Multiplication" $ do
        context "with -3 * -3" $ do
            it "Just 9.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-3 * -3"
                evalExpr tokens `shouldBe` Just 9.00
    describe "Binary Operators: Multiplication" $ do
        context "with 3 * +3" $ do
            it "Just 9.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "3 * +3"
                evalExpr tokens `shouldBe` Just 9.00
    describe "Binary Operators: Multiplication" $ do
        context "with +3 * 3" $ do
            it "Just 9.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "+3 * 3"
                evalExpr tokens `shouldBe` Just 9.00
    describe "Binary Operators: Multiplication" $ do
        context "with +3 * +3" $ do
            it "Just 9.00" $ do
                let tokens = catMaybes $ lexer $ concat $ words "+3 * +3"
                evalExpr tokens `shouldBe` Just 9.00
    describe "Binary Operators: Multiplication" $ do
        context "with +3 * -3" $ do
            it "Just (-9.00)" $ do
                let tokens = catMaybes $ lexer $ concat $ words "+3 * -3"
                evalExpr tokens `shouldBe` Just (-9.00)
    describe "Binary Operators: Multiplication" $ do
        context "with -3 * +3" $ do
            it "Just (-9.00)" $ do
                let tokens = catMaybes $ lexer $ concat $ words "-3 * +3"
                evalExpr tokens `shouldBe` Just (-9.00)

main :: IO ()
main = hspec spec