module Compile where

import AST

-- Pre conditioning of source code
variableRename ∷ Expression → Expression
variableRename = id

constUnroll ∷ Expression → Expression
constUnroll = id

preParse ∷ Expression → Expression
preParse = variableRename ∘ constUnroll

-- Instruction emitting
emit ∷ Expression → [Instruction]
emit (IntExp i) = [LDC i]
