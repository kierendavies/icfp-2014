module AST (
            Expression(..),
            Instruction (..),
            PreProcMap
           ) where

import Data.List
import qualified Data.Map.Strict as M

type PreProcMap = M.Map String Expression

data Expression = ListExp String [Expression]
                | QListExp [Expression]
                | IntExp Int
                | VarExp String
                | VarHash Int
                | Closure [String] Expression

data Instruction = LDC Int
                 | LD Int Int
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
                 | SEL Int Int
                 | JOIN
                 | LDF Int
                 | AP Int
                 | RTN
                 | DUM Int
                 | RAP Int
                 | STOP
                 | TSEL Int Int
                 | TAP Int
                 | TRAP Int
                 | ST Int Int
                      deriving (Show,Eq,Ord)

instance Show Expression where
  show (ListExp x xs) = "(" ++ (intercalate " " $ x : map show xs) ++ ")"
  show (QListExp xs) = "'(" ++ (intercalate " " . map show $ xs) ++ ")"
  show (IntExp n) = show n
  show (VarExp x) = "$" ++ x
  show (VarHash x) = "$" ++ show x
  show (Closure args exp) = "(λ (" ++ intercalate " " args ++ ") (" ++ show exp ++ ")"
