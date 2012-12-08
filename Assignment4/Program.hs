module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show

constructProgram a = Program a

instance Parse T where
  parse = (iter Statement.parse) >-> constructProgram
  toString (Program stmts) =  foldr1 (++) (map Statement.toString stmts)
             
exec (Program stmts) input = Statement.exec stmts Dictionary.empty input
