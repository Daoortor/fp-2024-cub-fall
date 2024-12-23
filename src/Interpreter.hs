module Interpreter (runInterpreter) where

import Expr
import Parser
import Eval
import CommandParser
import System.IO ( hFlush, stdout )
import qualified Data.Map.Strict as M
import qualified Control.Monad.Trans.State.Strict as T
import Control.Monad.IO.Class
import Text.Printf (printf)
import Control.Monad (unless)

type REPL = T.StateT (M.Map String Double) IO

internalVars :: [String]
internalVars = ["mem", "ans"]

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

evalCommand :: Command -> REPL ()
evalCommand Pass = return ()
evalCommand Quit = return ()
evalCommand Help = do
    printREPL "************************************************************************************************************************"
    printREPL "EXPRESSIONS"
    printREPL "    Enter an arithmetic expression to evaluate it. Currently supported operations are +, -, *, /, **, abs()."
    printREPL "    Expressions may also include variables (more on this below)."
    printREPL ""
    printREPL "VARIABLES"
    printREPL "    The internal variables `ans` and `mem` are available:"
    printREPL "        - `ans` stores the result of the last successful computation; defaults to 0."
    printREPL "        - `mem` is the value currently stored in memory; defaults to 0."
    printREPL "    Variables can also be (un-)set with `set` and `unset` commands (see below)."
    printREPL "    Variables can be accessed in expressions, with their names in quotes (e. g. `(\"mem\" + 2) * \"ans\"`)"
    printREPL ""
    printREPL "COMMANDS"
    printREPL "    If input begins with ':', it's treated as a command. Currently supported commands include:"
    printREPL "        - `:help`              outputs this message"
    printREPL "        - `:set` <var> <val>   sets the variable <var> to floating-point value <val>. If <var> doesn't exist, it will "
    printREPL "                               be created."
    printREPL "        - `:unset` <var>       unsets (deletes) variable var"
    printREPL "        - `:ms`                `mem := ans` (stores the result of the last successful computation to memory)"
    printREPL "        - `:mc`                `mem := 0` (clears the value stored in memory)"
    printREPL "        - `:mr`                outputs \"mem\""
    printREPL "        - `:m+`                `mem += ans`"
    printREPL "        - `:m-`                `mem -= ans`"
    printREPL "        - `:quit`              exits the interpreter"
    printREPL "************************************************************************************************************************"
evalCommand MSave = do
    ans <- getVar "ans"
    case ans of
        Left e -> printREPL $ show e
        Right x -> do
            T.modify (M.insert "mem" x)
            printREPL $ "Saved value " ++ show x ++ " into memory"
evalCommand MClear = do
    T.modify (M.insert "mem" 0)
    printREPL "Cleared value from memory"
evalCommand MRead = do
    mem <- getVar "mem"
    printREPL $ myShow mem
evalCommand MAdd = do
    ans <- getVar "ans"
    mem <- getVar "mem"
    case (+) <$> mem <*> ans of
        Left e -> printREPL $ show e
        Right x -> T.modify (M.insert "mem" x)
evalCommand MSub = do
    ans <- getVar "ans"
    mem <- getVar "mem"
    case (-) <$> mem <*> ans of
            Left e -> printREPL $ show e
            Right x -> T.modify (M.insert "mem" x)
evalCommand (Evaluate input) = let parseResult = parseExpr input in
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
evalCommand (Set varName value) = if elem varName internalVars
    then
        printREPL $ "Internal variable \"" ++ varName ++ "\" cannot be modified directly"
    else do
        T.modify (M.insert varName value)
        printREPL $ "Set variable \"" ++ varName ++ "\" to " ++ show value
evalCommand (Unset varName) = if elem varName internalVars
    then
        printREPL $ "Internal variable \"" ++ varName ++ "\" cannot be modified directly"
    else do
        T.modify (M.delete varName)
        printREPL $ "Unset variable \"" ++ varName ++ "\""

runInterpreter :: REPL ()
runInterpreter = do
    printREPL $ "Welcome to scientific-calculator. Type :help for help."
    interpreterCycle where
        interpreterCycle = do
            input <- liftIO myRead
            case parseCommand input of
                (Left err) -> do
                    printREPL err
                    interpreterCycle
                (Right command) -> unless (command == Quit) $ do
                    evalCommand command
                    interpreterCycle
