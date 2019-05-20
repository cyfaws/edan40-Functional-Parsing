module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
import Data.Maybe (fromMaybe)

newtype T = Program [Statement.T] deriving Show-- to be defined
instance Parse T where
  parse = (iter Statement.parse) >-> Program
  -- Pretty hacky toString because we chose to add newlines in the beginning in
  -- Statement.toString to simplify indentation. The first char that is dropped
  -- will always be a '\n'
  toString (Program list) = drop 1 ((concatMap Statement.toString list)++"\n")
             
exec (Program stmts) input = Statement.exec stmts Dictionary.empty input
