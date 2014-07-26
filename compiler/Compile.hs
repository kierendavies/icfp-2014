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

variableRename :: I.IntMap (M.Map String (Int,Int)) -> Int -> Expression -> Expression
variableRename w n (ListExp "lambda" ((QListExp es):body)) = ListExp "lambda" $ map (variableRename w' n') body
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

assertVarRef :: Expression -> Expression
assertVarRef v@(VarRef _ _) = v
assertVarRef e = error $ "Expected named variable but found "++show e

assertN :: (Monad m) => Int -> [Expression] -> m [Expression]
assertN n e = if length e < n
                 then return e
                 else error $ "Expected " ++ show n ++ " arguments, but found " ++ show e

emit :: (Monad m) => Expression -> m [Instruction]
emit (IntExp i) = return [LDC i]
{- 
  (if pred true false) becomes:
  <compiled pred>
  tselR 1 (len <compiled true> + 3)
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
emit (ListExp "cons" es) = do assertN 2 es
                              let (a:b: []) = es
                              liftM concat $ sequence [emit a, emit b, return [CONS]]
{-
  (call foo) matches over closures and variables
  and dispatches accordingly. So if we have
  ListExp "call" [VarHas d v] then this becomes:
  
  LD d v
  AP 0
  
  If we had ListExp "call" l@[ListExp "lambda" _]
  then this becomes:
  
  
  <compiled l>
  

-}

{-
  (lambda '(vars) body) which we have as
  ListExp "lambda" [QListExp [VarHash d v *], e@(ListExp _ _)]
  becomes
  
-}
