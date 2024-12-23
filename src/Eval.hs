module Eval (evalExpr, CalcState) where

import Expr
import qualified Data.Map.Strict as M
import qualified Control.Monad.Trans.State.Strict as T

type CalcState = M.Map String Double

addContext :: Expr -> (Double -> Double -> Either SimpleEvalError Double) -> Double -> Double -> Either EvalError Double
addContext expr f x y = either (\res -> Left $ EvalError res expr) Right (f x y)

evalBinary :: (Double -> Double -> Either EvalError Double) -> Either EvalError Double -> Either EvalError Double -> Either EvalError Double
evalBinary _ _ (Left e) = Left e
evalBinary _ (Left e) _ = Left e
evalBinary f (Right x) (Right y) = f x y

wrap :: (Double -> Double -> Double) -> Double -> Double -> Either EvalError Double
wrap f x y = Right $ f x y

safeDiv :: Double -> Double -> Either SimpleEvalError Double
safeDiv x y | y == 0 = Left DivByZero
            | otherwise = Right $ x / y

safePow :: Double -> Double -> Either SimpleEvalError Double
safePow x y | x == 0 && y <= 0 = Left $ ZeroNonPositivePow y
            | x < 0 && (y < 0 || y /= fromIntegral (floor y :: Integer)) = Left $ NegNonNaturalPow x y
            | otherwise = Right $ x ** y

evalBinop :: (Double -> Double -> Either EvalError Double) -> Expr -> Expr -> T.State CalcState (Either EvalError Double)
evalBinop op x y = T.gets $ \s -> evalBinary op (T.evalState (evalExpr x) s) (T.evalState (evalExpr y) s)

evalUnop :: (Double -> Double) -> Expr -> T.State CalcState (Either EvalError Double)
evalUnop op x = T.gets $ \s -> evalBinary (wrap (flip $ const op)) (T.evalState (evalExpr x) s) (Right 0)

evalExpr :: Expr -> T.State CalcState (Either EvalError Double)
evalExpr (Num x) = return $ Right x
evalExpr expr@(Var var) = T.gets $ \s ->
    case M.lookup var s of
        Nothing -> Left $ EvalError (UnknownVariable var) expr
        Just value -> Right value
evalExpr (Plus x y) = evalBinop (wrap (+)) x y
evalExpr (Minus x y) = evalBinop (wrap (-)) x y
evalExpr (Mult x y) = evalBinop (wrap (*)) x y
evalExpr expr@(Div x y) = evalBinop (addContext expr safeDiv) x y
evalExpr expr@(Pow x y) = evalBinop (addContext expr safePow) x y
evalExpr (Abs x) = evalUnop abs x
evalExpr (UnaryMinus x) = evalUnop negate x
