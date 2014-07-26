module Compile where

import AST
import qualified Data.Map.Strict as M
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA (integerDigest, sha256)

{- Pre conditioning of source code -} 
variableRename :: Expression -> Expression
variableRename (ListExp s e) = ListExp s $ map variableRename e
variableRename (QListExp e) = QListExp $ map variableRename e
variableRename (VarExp s) = 

-- Do a traversal to find leaves that are VarExp String and see if
-- they have been #define 'd
constUnroll :: PreProcMap -> Expression -> Expression
constUnroll ppm (ListExp s e) = ListExp s $ map (constUnroll ppm) e
constUnroll ppm (QListExp e) = QListExp $ map (constUnroll ppm) e
constUnroll ppm e@(VarExp s) = M.findWithDefault e s ppm
constUnroll _ e = e 

--preParse :: Expression -> Expression
--preParse = variableRename . constUnroll

-- Instruction emitting
emit :: Expression -> [Instruction]
emit (IntExp i) = [LDC i]
