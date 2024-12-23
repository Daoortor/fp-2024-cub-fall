module CommandParser where

import GHC.Utils.Misc (split)
import Data.String.Utils (strip)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as M

data Command = Pass
               | Quit
               | Help
               | MSave
               | MClear
               | MRead
               | MAdd
               | MSub
               | Evaluate String
               | Set String Double
               | Unset String
   deriving Eq

parseCommand :: String -> Either String Command
parseCommand input = case strip input of
    (':':xs) -> parseTokensToCommand $ filter ((/=) "") $ split ' ' xs
    xs -> Right $ Evaluate xs

parseTokensToCommand :: [String] -> Either String Command
parseTokensToCommand [] = Right Pass
parseTokensToCommand (t:argv) = case t of
    "quit" -> Right Quit
    "help" -> Right Help
    "ms" -> Right MSave
    "mc" -> Right MClear
    "mr" -> Right MRead
    "m+" -> Right MAdd
    "m-" -> Right MSub
    "set" -> if (argc /= 2)
        then Left ("set expected 2 arguments, but got " ++ show argc)
        else case readMaybe (argv !! 1) of
            Nothing -> Left ((argv !! 1) ++ " is not a number")
            (Just value) -> Right (Set (head argv) value)
    "unset" ->  if (argc /= 1)
        then Left ("unset expected 1 argument, but got " ++ show argc)
        else Right (Unset (head argv))
    _ -> Left ("unknown command: " ++ t)
    where argc = length argv