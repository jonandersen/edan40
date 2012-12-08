module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Skip | 
    Assignment String Expr.T |
    Read String |
    Write Expr.T | 
    Begin [Statement] | 
    While Expr.T Statement |
    If Expr.T Statement Statement
    deriving Show

assignment, skip, reading, writing, ifs, begin, while :: Parser Statement
statement = assignment ! reading ! writing ! skip ! ifs ! begin ! while

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

reading = accept "read" -# word #- require ";" >-> Read

writing = accept "write" -# Expr.parse #- require ";" >-> Write

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

ifs = accept "if" -# Expr.parse #- require "then" # statement #- require "else" # statement >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2

while = accept "while" -# Expr.parse #- require "do" # statement >-> buildWhile
buildWhile (e, s) = While e s

begin = accept "begin" -# iter statement #- require "end" >-> Begin


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []

exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (Skip:stmts) dict input = exec stmts dict input

exec (Assignment v expr:stmts) dict input = exec stmts (Dictionary.insert (v, Expr.value expr dict) dict ) input

exec (While cond doStmts:stmts) dict input =
  if (Expr.value cond dict)>0 
  then exec ([doStmts] ++ [While cond doStmts] ++ stmts) dict input
  else exec stmts dict input
  
exec (Read v: stmts) dict (current:rest) =  exec stmts (Dictionary.insert (v, current) dict ) rest

exec (Write expr: stmts) dict input = [Expr.value expr dict] ++ (exec stmts dict input)

exec (Begin stmt:stmts) dict input = (exec (stmt++stmts) dict input)

-- I  decided I wanted indentation, the first arugment to this function is the indentation from the parent. 

toString' :: String -> Statement -> String
toString' ind (Skip) = ind ++ "skip;\n"
toString' ind (Assignment v e)  = ind ++ v ++ ":=" ++ Expr.toString e ++ ";\n"
toString' ind (While cond stmts) = ind ++ "while " ++ Expr.toString cond ++ " do\n" ++ (toString' (ind ++ "  ") stmts)
toString' ind (Read string) = ind ++ "read " ++ string ++ ";\n"
toString' ind (Write expr) = ind ++ "write " ++ Expr.toString expr ++ ";\n"
toString' ind (Begin stmts) = ind ++ "begin\n" ++ (foldr1 (++) $  map (toString' (ind ++ "  ")) stmts) ++ ind ++ "end\n"
toString' ind (If cond s1 s2) = ind ++ "if " ++ Expr.toString cond ++ " then \n"  ++ (toString' (ind ++ "  ") s1) ++ ind ++ "else\n" ++ (toString' (ind ++ "  ") s2)


instance Parse Statement where
  parse = statement
  toString = toString' "" 

    


  