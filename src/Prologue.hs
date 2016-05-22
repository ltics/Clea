module Prologue where

import Ast
import Debug.Trace
import qualified Data.Map as M

numOp op [ENum a, ENum b] = return $ ENum $ op a b
numOp _ _ = error "illegal arguments to number operation"

builtins = M.fromList [("+", mkFunc $ numOp (+)),
                       ("-", mkFunc $ numOp (-)),
                       ("*", mkFunc $ numOp (*)),
                       ("/", mkFunc $ numOp (div))]
