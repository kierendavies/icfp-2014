module Compile where

import AST
import Control.Monad
import Data.Int
import qualified Data.Map.Strict as M
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA (integerDigest, sha256)

{- Pre conditioning of source code -} 

-- Change variables to numbers, deterministically
ensureAtLeastValue :: Int32 -> Int32 -> Int32
ensureAtLeastValue min = head . dropWhile (min>) . iterate gen
                       where
                         gen = hash . show

hash :: String -> Int32
hash = fromIntegral . integerDigest . sha256 . pack


variableRename :: Int32 -> Expression -> Expression
variableRename n (ListExp s e) = ListExp s $ map (variableRename $ succ n) e
variableRename n (QListExp e) = QListExp $ map (variableRename $ succ n) e
variableRename n (VarExp e) = VarHash n . ensureAtLeastValue 128 . hash $ e
variableRename _ e = e

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
len :: [a] -> Int32
len = fromIntegral . length

assertN :: (Monad m) => Int -> [Expression] -> m [Expression]
assertN n e = if length e < n
                 then return e
                 else error $ "Expected " ++ show n ++ " arguments, but found " ++ show e

assertVarHash :: (Monad m) => Expression -> m Expression
assertVarHash v@(VarHash _ _) = return v
assertVarHash e = error $ "Expected named variable but found "++show e

emit :: (Monad m) => Expression -> m [Instruction]
emit (IntExp i) = return [LDC i]
{- 
  (if pred true false) becomes:
  <compiled pred>
  tselR 1 (len <compiled true> + 2)
  <compiled true>
  ldc 1
  tselR (len <compiled false> + 1) 0
  <compiled false>
-}
emit (ListExp "if" es) = do assertN 3 es
                            let (p:t:f: []) = es
                            p' <- emit p
                            t' <- emit t
                            f' <- emit f
                            return $ concat [p'
                                            ,[TSELRel 1 . (+2) $ len t']
                                            ,t'
                                            ,[LDC 1, TSELRel (succ $ len f') 0]
                                            ,f']
{- 
   (let "foo" bar) which we have as
   ListExp "let" (VarHash depth var#):foo:[]
   becomes:
   <compiled foo>
   ST depth var#
-}                                               
emit (ListExp "let" es) = do assertN 2 es
                             let (v:e:[]) = es
                             assertVarHash v
                             let VarHash d vn = v
                             e' <- emit e
                             return $ concat [e', [ST d vn]]

{-
  (cons a b) becomes:
  <compile a>
  <compile b>
  CONS
-}
emit (ListExp "cons" es) = do assertN 2 es
                              let (a:b: []) = es
                              liftM concat $ sequence [emit a, emit b, return [CONS]]

