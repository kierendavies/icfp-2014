module Parser (parseProgramme) where

import Control.Applicative           hiding (many, (<|>))
import Text.ParserCombinators.Parsec hiding (spaces)
import AST

parseProgramme ∷ String → Either String Expression
parseProgramme = parse programme ""

programme :: Parser Expression
programme = spaces *> (fmap (ListExp . ((VarExp "do") :)) (expression `sepEndBy` spaces)) <* eof

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
listLit = between (char '(' *> spaces) (spaces <* char ')') (expression `sepEndBy` spaces1) <?> "list"

variable :: Parser Expression
variable = fmap VarExp (liftA2 (:) letter (many alphaNum)) <?> "variable"

spaces :: Parser ()
spaces = skipMany space'

spaces1 :: Parser ()
spaces1 = skipMany1 space'

space' :: Parser ()
space' = ((space >> return ()) <|> comment) <?> ""

comment :: Parser ()
comment = char ';' >> many (noneOf "\n") >> newline >> return ()
