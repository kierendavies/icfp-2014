module AST (
            Expression(..),
            Instruction (..),
            PreProcMap
           ) where

import Data.Int
import Data.List
import qualified Data.Map as M

type PreProcMap = M.Map String Expression

data Expression = ListExp String [Expression]
                | QListExp [Expression]
                | IntExp Int32
                | VarExp String
                | VarHash Int32
                | Closure [String] Expression

data Instruction = LDC Int32
                 | LD Int32 Int32
                 | ADD
                 | SUB
                 | MUL
                 | DIV
                 | CEQ
                 | CGT
                 | CGTE
                 | ATOM
                 | CONS
                 | CAR
                 | CDR
                 | SEL Int32 Int32
                 | JOIN
                 | LDF Int32
                 | AP Int32
                 | RTN
                 | DUM Int32
                 | RAP Int32
                 | STOP
                 | TSEL Int32 Int32
                 | TAP Int32
                 | TRAP Int32
                 | ST Int32 Int32
                      deriving (Show,Eq,Ord,Read)

instance Show Expression where
  show (ListExp x xs) = "(" ++ (intercalate " " $ x : map show xs) ++ ")"
  show (QListExp xs) = "'(" ++ (intercalate " " . map show $ xs) ++ ")"
  show (IntExp n) = show n
  show (VarExp x) = "$" ++ x
  show (VarHash x) = "$" ++ show x
  show (Closure args exp) = "(λ (" ++ intercalate " " args ++ ") (" ++ show exp ++ ")"
