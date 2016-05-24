module EvalSpec where

import Ast
import Scope
import Eval (eval)
import Parser (parseExpr)
import Prologue (builtins, loadlib)
import Control.Monad (foldM)
import qualified Data.Map as M
import qualified Text.PrettyPrint as PP
import Test.Hspec

runEvalSpecCases :: [(String, String)] -> IO ()
runEvalSpecCases exprExpects = do
    scope <- emptyScope
    mapM_ (\(k, v) -> (insertValue scope (ESymbol k) v)) builtins
    loadlib scope
    (vals, expects) <- foldM (\(vals, expects) (expr, expect) -> do
                               let ast = parseExpr expr
                               val <- eval ast scope
                               return (vals ++ [val], expects ++ [expect]))
                             ([], []) exprExpects
    (map (PP.text . show) vals) `shouldBe` map PP.text expects

spec :: Spec
spec = describe "evaluation test" $ do
        it "should get value of expr" $ do
          runEvalSpecCases [("(+ 1 2)", "3"),
                            ("(+ 5 (* 2 3))", "11"),
                            ("(/ (- (+ 5 (* 2 3)) 3) 4)", "2"),
                            ("(/ (- (+ 515 (* 222 311)) 302) 27)", "2565"),
                            ("(* -3 6)", "-18"),
                            ("(/ (- (+ 515 (* -222 311)) 296) 27)", "-2549"),
                            ("[1 2 (+ 1 2)]", "[1 2 3]"),
                            ("{\"a\" (+ 7 8)}", "{\"a\" 15}"),
                            ("{:a (+ 7 8)}", "{\"a\" 15}"),
                            ("(let* [z (+ 2 3)] (+ 1 z))", "6"),
                            ("(let [z (+ 2 3)] (+ 1 z))", "6"),
                            ("(let* [p (+ 2 3) q (+ 2 p)] (+ p q))", "12"),
                            ("(let [p (+ 2 3) q (+ 2 p)] (+ p q))", "12"),
                            ("(let* [a 5 b 6] [3 4 a [b 7] 8])", "[3 4 5 [6 7] 8]"),
                            ("(let [a 5 b 6] [3 4 a [b 7] 8])", "[3 4 5 [6 7] 8]"),
                            ("(if (≥ (count (list 1 2 3)) 3) \"yes\" \"no\")", "\"yes\""),
                            ("(reverse '(1 2 3))", "(3 2 1)"),
                            ("(map (λ [x] (+ x 1)) [1 2 3])", "(2 3 4)"),
                            ("(filter (λ [x] (≠ x 1)) [1 2 3])", "(2 3)"),
                            ("(reduce + '(1 2 3))", "6"),
                            ("(append '(1 2 3) '(4 5 6))", "(1 2 3 4 5 6)"),
                            ("(def a (atom 2))", "(atom 2)"),
                            ("(atom? a)", "True"),
                            ("(atom? 1)", "False"),
                            ("(deref a)", "2"),
                            ("(reset! a 3)", "3"),
                            ("@a", "3"),
                            ("(swap! a (λ [x] (+ x 3)))", "6"),
                            ("@a", "6"),
                            ("(reset! a 2)", "2"),
                            ("(unless false 7 8)", "7"),
                            ("(unless true 7 8)", "8"),
                            ("(defmacro unless2 [pred a b] `(if (not ~pred) ~a ~b))", "(λ (pred a b) (syntaxquote (if (not (unquote pred)) (unquote a) (unquote b))))"),
                            ("(macroexpand (unless2 true 3 4))", "(if (not True) 3 4)"),
                            ("(meta (with-meta (λ (a) a) {\"b\" 1}))", "{\"b\" 1}"),
                            ("(= (gensym) (gensym))", "False"),
                            ("(-> 7)", "7"),
                            ("(-> (list 7 8 9) first)", "7"),
                            ("(-> (list 7 8 9) rest (rest) first (- 7))", "2"),
                            ("(->> 7)", "7"),
                            ("(->> (list 7 8 9) first)", "7"),
                            ("(->> (list 7 8 9) rest (rest) first (- 7))", "-2"),
                            ("(cond)", "nil"),
                            ("(cond true 7 true 8)", "7"),
                            ("(cond false 7 (= 2 2) 8 else 9)", "8"),
                            ("(cond false 7 false 8 false 9)", "nil"),
                            ("(cond false 7 false 8 else 9)", "9")]

