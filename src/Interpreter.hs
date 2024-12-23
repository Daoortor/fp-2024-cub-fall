module Interpreter (runInterpreter) where

import Expr
import Parser
import Eval
import System.IO ( hFlush, stdout )
import qualified Data.Map.Strict as M
import qualified Control.Monad.Trans.State.Strict as T
import Control.Monad.IO.Class
import Text.Printf (printf)
import Control.Monad (unless)

type REPL = T.StateT (M.Map String Double) IO

myRead :: IO String
myRead = do
    putStr "[scientific-calculator]$ "
    hFlush stdout
    getLine

myPrint :: String -> IO ()
myPrint = putStrLn

printREPL :: String -> REPL ()
printREPL = liftIO . myPrint

getVar :: String -> REPL (Either EvalError Double)
getVar name = maybe (Left $ EvalError (UnknownVariable name) (Var name)) Right . M.lookup name <$> T.get

myShow :: Either EvalError Double -> String
myShow (Left e) = show e
myShow (Right x) = show x

evalCommand :: String -> REPL ()
evalCommand ":help" = do
    printREPL "************************************************************************************************************************"
    printREPL "EXPRESSIONS"
    printREPL "    Enter an arithmetic expression to evaluate it. Currently supported operations are +, -, *, /, **, abs()."
    printREPL "    Expressions may also include variables (more on this below)."
    printREPL ""
    printREPL "VARIABLES"
    printREPL "    Currently, only the internal variables `ans` and `mem` are supported:"
    printREPL "        - `ans` stores the result of the last successful computation; defaults to 0."
    printREPL "        - `mem` is the value currently stored in memory; defaults to 0."
    printREPL "    Variables can be accessed in expressions, with their names in quotes (e. g. `(\"mem\" + 2) * \"ans\"`)"
    printREPL ""
    printREPL "COMMANDS"
    printREPL "    If input begins with ':', it's treated as a command. Currently supported commands include:"
    printREPL "        - `:help`   prints this message"
    printREPL "        - `:ms`     `mem := ans` (stores the result of the last successful computation to memory)"
    printREPL "        - `:mc`     `mem := 0` (clears the value stored in memory)"
    printREPL "        - `:mr`     outputs \"mem\""
    printREPL "        - `:m+`     `mem += ans`"
    printREPL "        - `:m-`     `mem -= ans`"
    printREPL "        - `:quit`   exits the interpreter"
    printREPL "************************************************************************************************************************"

evalCommand ":ms" = do
    ans <- getVar "ans"
    case ans of
        Left e -> printREPL $ show e
        Right x -> do
            T.modify (M.insert "mem" x)
            printREPL $ "Saved value " ++ show x ++ " into memory"
evalCommand ":mc" = do
    T.modify (M.insert "mem" 0)
    printREPL "Cleared value from memory"
evalCommand ":mr" = do
    mem <- getVar "mem"
    printREPL $ myShow mem
evalCommand ":m+" = do
    ans <- getVar "ans"
    mem <- getVar "mem"
    case (+) <$> mem <*> ans of
        Left e -> printREPL $ show e
        Right x -> T.modify (M.insert "mem" x)
evalCommand ":m-" = do
    ans <- getVar "ans"
    mem <- getVar "mem"
    case (-) <$> mem <*> ans of
            Left e -> printREPL $ show e
            Right x -> T.modify (M.insert "mem" x)
evalCommand input@(':':_) = printREPL $ "Unknown command: `" ++ input ++ "`"
evalCommand input = let parseResult = parseExpr input in
    case parseResult of
        Left parseError -> printREPL $ "Parse error: " ++ show parseError
        Right expr -> do
            result <- T.gets $ T.evalState (evalExpr expr)
            case result of
                Left e -> printREPL $ show e
                Right x -> do
                    T.modify (M.insert "ans" x)
                    ans <- getVar "ans"
                    printREPL $ myShow ans

runInterpreter :: REPL ()
runInterpreter = do
    printREPL "Welcome to scientific-calculator. Type :help for help."
    interpreterCycle where
        interpreterCycle = do
            input <- liftIO myRead
            unless (input == ":quit") $ do
                evalCommand input
                interpreterCycle
