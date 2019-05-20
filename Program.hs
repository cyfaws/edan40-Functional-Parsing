module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
import Data.Maybe (fromMaybe)

newtype T = Program [Statement.T] -- to be defined
instance Parse T where
  parse = (iter Statement.parse) >-> Program
  toString = error "Program.toString not implemented"
             
exec (Program stmts) input = Statement.exec stmts Dictionary.empty input
