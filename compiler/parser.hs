
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Applicative hiding ((<|>), many)

import AST

program :: Parser Expression
program = spaces *> (fmap (ListExp . ((VarExp "do") :)) (expression `sepEndBy` spaces)) <* eof

expression :: Parser Expression
expression = list <|> number <|> variable

numericLit :: Parser Int
numericLit = (fmap (read) $ many1 (digit <?> "")) <?> "number"

number = fmap IntExp numericLit

list :: Parser Expression
list = between (char '(' *> spaces) (spaces <* char ')') (fmap ListExp $ expression `sepEndBy` (many1 space)) <?> "list"

variable :: Parser Expression
variable = fmap VarExp (liftA2 (:) letter (many alphaNum)) <?> "variable"

spaces :: Parser ()
spaces = skipMany (space <?> "")

