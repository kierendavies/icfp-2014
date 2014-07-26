
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Applicative hiding ((<|>), many)

import AST

program :: Parser Expression
program = spaces *> (fmap (ListExp . ((VarExp "do") :)) (expression `sepEndBy` spaces)) <* eof

expression :: Parser Expression
expression = list <|> qlist <|> number <|> variable

numericLit :: Parser Int
numericLit = (fmap (read) $ many1 (digit <?> "")) <?> "number"

number = fmap IntExp numericLit

list :: Parser Expression
list = fmap ListExp listLit

qlist :: Parser Expression
qlist = char '\'' *> fmap QListExp listLit

listLit :: Parser [Expression]
listLit = between (char '(' *> spaces) (spaces <* char ')') (expression `sepEndBy` (many1 space)) <?> "list"

variable :: Parser Expression
variable = fmap VarExp (liftA2 (:) letter (many alphaNum)) <?> "variable"

spaces :: Parser ()
spaces = skipMany (space <?> "")
