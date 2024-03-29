-- TODO: Preparse for let -> lambda, defun to lambda stuff, loops

module Compile where

import AST
import Data.Int
import Data.Maybe
import Data.Function
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as I
{- Pre conditioning of source code -} 

-- Change variables to numbers, deterministically
isVarExp :: Expression -> Bool
isVarExp (VarExp _) = True
isVarExp _ = False 

variableRename' :: Expression -> Expression
variableRename' = variableRename I.empty 0

variableRename :: I.IntMap (M.Map String (Int,Int)) -> Int -> Expression -> Expression
variableRename w n (ListExp "lambda" (v@(QListExp es):body)) = ListExp "lambda" . (v:) $ map (variableRename w' n') body
                                                             where
                                                               n' = succ n
                                                               m = fromMaybe M.empty $ I.lookup n w
                                                               stuff = case all isVarExp es of
                                                                         True -> map (\(VarExp x) -> x) es
                                                                         _ -> error $ "Lambad expects variables but given "++show es
                                                               m' = foldl (\f (s,i) -> M.insert s (n,i) f) m $ zip stuff [0..]
                                                               w' = I.insert n' m' w
variableRename w n (ListExp s es) = ListExp s $ map (variableRename w n) es
variableRename w n (QListExp es) = QListExp $ map (variableRename w n) es
variableRename w n (VarExp s) = case I.lookup n w of
                                  Just m -> case M.lookup s m of
                                            Just (d,v) -> (on VarRef fromIntegral) d v
                                            _ -> error $ "Attempt to use non-existent parameter "++show s
                                  _ -> error $ "Attempt to use non-existent parameter "++show s
variableRename w n e = e
{-
variableRename :: Int32 -> Expression -> Expression
variableRename n (ListExp s e) = ListExp s $ map (variableRename $ succ n) e
variableRename n (QListExp e) = QListExp $ map (variableRename $ succ n) e
variableRename n (VarExp e) = VarHash n . ensureAtLeastValue 128 . hash $ e
variableRename _ e = e
-}

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

concatSeq :: (Monad m) =>  [m [a]] -> m [a]
concatSeq = liftM concat . sequence

assertVarRef :: Expression -> Expression
assertVarRef v@(VarRef _ _) = v
assertVarRef e = error $ "Expected named variable but found "++show e

assertValidLambda :: Expression -> Expression
assertValidLambda l@(ListExp "lambda" ((QListExp _):_:[])) = l
assertValidLambda e = error $ "Expected valid lambda expression but found "++show e

numArgsLambda :: Expression -> Int32
numArgsLambda e = let ListExp "lambda" ((QListExp q):_) = assertValidLambda e
                  in len q
                  
assertN :: Int -> [Expression] -> [Expression]
assertN n e = if length e == n
                 then e
                 else error $ "Expected " ++ show n ++ " arguments, but found " ++ show e

emit :: (Monad m) => Expression -> m [Instruction]
emit (IntExp i) = return [LDC i]
emit (VarRef d v) = return [LD d v]
emit (ListExp "+" args) = let (a:b:[]) = assertN 2 args 
                          in concatSeq [concatSeq [emit b, emit a], return [ADD]]
emit (ListExp "-" args) = let (a:b:[]) = assertN 2 args
                          in concatSeq [concatSeq [emit b, emit a], return [SUB]]
emit (ListExp "*" args) = let (a:b:[]) = assertN 2 args 
                          in concatSeq [concatSeq [emit b, emit a], return [MUL]]
emit (ListExp "/" args) = let (a:b:[]) = assertN 2 args
                          in concatSeq [concatSeq [emit b, emit a], return [DIV]]
emit (ListExp "<" args) = let (a:b:[]) = assertN 2 args
                          in concatSeq [concatSeq [emit a, emit b], return [CGT]]
emit (ListExp "<=" args) = let (a:b:[]) = assertN 2 args
                           in concatSeq [concatSeq [emit a, emit b], return [CGTE]]
emit (ListExp ">" args) = let (a:b:[]) = assertN 2 args
                           in concatSeq [concatSeq [emit b, emit a], return [CGT]]
emit (ListExp ">=" args) = let (a:b:[]) = assertN 2 args
                           in concatSeq [concatSeq [emit b, emit a], return [CGTE]]                         
emit (ListExp "=" args) = let (a:b:[]) = assertN 2 args
                          in concatSeq [concatSeq [emit a, emit b], return [CEQ]]
emit (ListExp "car" args) = return (assertN 0 args) >> return [CAR]
emit (ListExp "cdr" args) = return (assertN 0 args) >> return [CDR]
{- 
  (if pred true false) becomes:
  <compiled pred>
  tselR 1 (len <compiled true> + 3)
  <compiled true>
  ldc 1
  tselR (len <compiled false> + 1) 0
  <compiled false>
-}
emit (ListExp "if" es) = do let (p:t:f:[]) = assertN 3 es
                            p' <- emit p
                            t' <- emit t
                            f' <- emit f
                            return $ concat [p'
                                            ,[TSELRel 1 . (+3) $ len t']
                                            ,t'
                                            ,[LDC 1, TSELRel (succ $ len f') 0]
                                            ,f']
{-
  (cons a b) becomes:
  <compiled a>
  <compiled b>
  CONS
-}
emit (ListExp "cons" es) = do let (a:b:[]) = assertN 2 es
                              concatSeq [emit b, emit a, return [CONS]]
{-
  (call foo) matches over closures and variables
  and dispatches accordingly. So if we have
  ListExp "call" [VarRef d v] then this becomes:
  <compiled reversed args>
  LD d v
  AP (len args)

-}
emit (ListExp "call" (VarRef d v:args)) = concatSeq [concatSeq . map emit $ reverse args
                                               ,return [LD d v ,AP $ len args]]

emit (ListExp "call" (l@ (ListExp "lambda" _):args)) = do l' <- emit l
                                                          concatSeq [concatSeq . map emit $ reverse args
                                                               ,return [LDFRel 4
                                                                       ,AP $ numArgsLambda l
                                                                       ,LDC 0
                                                                       ,TSELRel 0 . succ . len $ l']
                                                               ,return l']
{-
  (lambda '(vars) body) which we have as
  ListExp "lambda" [QListExp [VarHash d v *], e@(ListExp _ _)]
  becomes:
  
  <compiled variableRename e>

-}
emit l@(ListExp "lambda" _) = let ListExp _ ((QListExp _):body:[]) = variableRename' $ assertValidLambda l
                              in emit body


foo = ListExp "+" [IntExp 3, IntExp 5]
bar = ListExp "lambda" [QListExp [VarExp "x"],ListExp "if" [ListExp "<" [VarExp "x", IntExp 0], VarExp "x", ListExp "*" [IntExp (-1), VarExp "x"]]]
wow = ListExp "if" [IntExp 1, IntExp 2, IntExp 3]
wew = ListExp "cons" [IntExp 1, IntExp 0]
