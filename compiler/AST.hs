
module AST
( Expression(..)
) where

import Data.List

data Expression = ListExp [Expression] | IntExp Int | VarExp String | Closure [String] Expression

instance Show Expression where
  show (ListExp xs) = "(" ++ (intercalate " " . map show $ xs) ++ ")"
  show (IntExp n) = "#" ++ show n
  show (VarExp x) = "$" ++ x
  show (Closure args exp) = "(lambda (" ++ intercalate " " args ++ ") (" ++ show exp ++ ")"

