module Main where

import Eval (eval, Scope)
import Parser (parseExpr)
import Prologue (builtins)
import Control.Lens
import Control.Monad.Trans
import System.Environment
import System.Console.Haskeline
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Exception as E

process :: String -> IO ()
process expr = E.catch (do
                        let ast = parseExpr expr
                        result <- eval ast builtins
                        putStrLn $ show result)
                       (\(E.ErrorCall e) -> do
                        putStrLn e)

loop :: InputT IO ()
loop = do
  minput <- getInputLine "λ> "
  case minput of
    Nothing -> do
      outputStrLn "Goodbye."
    Just input -> (liftIO $ process input) >> loop

prologueMessage :: String
prologueMessage = intercalate "\n"
  ["   ___  __    ____   __",
   "  / __)(  )  (  __) / _\\",
   " ( (__ / (_/\\ ) _) /    \\",
   "  \\___)\\____/(____)\\_/\\_/",
   ""
   ]

main :: IO ()
main = do
  args <- getArgs
  case (args ^? element 0) of
    Just arg -> if arg == "repl"
               then do
                putStrLn prologueMessage
                runInputT defaultSettings loop
               else do
                file <- readFile arg
                process file
    Nothing -> do
      input <- getContents
      process input
