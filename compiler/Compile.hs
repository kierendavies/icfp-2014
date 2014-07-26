module Compile where

import AST
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA (integerDigest, sha256)

-- Pre conditioning of source code
variableRename :: Expression -> Expression
variableRename = id

constUnroll :: () Expression -> Expression
constUnroll = id

preParse :: Expression -> Expression
preParse = variableRename . constUnroll

-- Instruction emitting
emit :: Expression -> [Instruction]
emit (IntExp i) = [LDC i]
