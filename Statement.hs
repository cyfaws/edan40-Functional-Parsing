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
exec [] _ _ = []
-- Assignment
exec (Assignment str expr:stmts) dict input =
    exec stmts (Dictionary.insert (str, Expr.value expr dict) dict) input

-- Skip
exec (Skip:stmts) dict input = exec stmts dict input

-- Begin TODO måste kunna gå ur blocket.
exec (Begin stmt:stmts) dict input = exec stmts dict input

-- If
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

-- While 
exec (While cond stmt:stmts) dict input =
    if (Expr.value cond dict)>0 
    then exec (While cond stmt:stmts) dict input 
    else exec stmts dict input

-- Read (Antar att det input är en lista som förbrukas. (ett element läses bara en gång.))
exec (Read expr:stmts) dict (input:inputs) =
    exec stmts (Dictionary.insert (Expr.toString expr, input) dict) inputs
    
-- Write
exec (Write expr:stmts) dict input = 
    (Expr.value expr dict):(exec stmts dict input)

instance Parse Statement where
  parse = assignment ! skip ! begin ! ifThenElse ! while ! read ! write
  toString = error "Statement.toString not implemented"
