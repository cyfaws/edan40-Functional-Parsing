module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail, read)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |                      -- tillagd   
    Begin [Statement] |       -- tillagd    
    If Expr.T Statement Statement |
    While Expr.T Statement |    -- tillagd 
    Read Expr.T |               -- tillagd   
    Write Expr.T                -- tillagd 
    deriving Show


assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin = Begin 

ifThenElse = accept "if" -# Expr.parse #- require "then" # parse #- 
                require "else" # parse >-> buildIfThenElse
-- Detta är ganska fult. Kan man lösa på något sätt? :/
buildIfThenElse ((expr, stat1), stat2) = If expr stat1 stat2

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (expr, stat) = While expr stat

read = accept "read" -# Expr.parse #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = assignment ! skip ! begin ! ifThenElse ! while ! read ! write
  toString = error "Statement.toString not implemented"
