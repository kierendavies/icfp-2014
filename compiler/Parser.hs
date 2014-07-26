module Parser (parseProgramme) where

import Control.Applicative           hiding (many, (<|>))
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Int
import Data.Char
import Data.List
import qualified Data.Map as M -- TODO: change to Data.Map.Strict
import AST

parseProgramme :: String -> Either ParseError (Expression, PreProcMap)
parseProgramme = parse fullProgramme ""

fullProgramme :: Parser (Expression, PreProcMap)
fullProgramme = do
                map <- preprocessor
                exp <- programme
                return (exp, map)

programme :: Parser Expression
programme = spaces *> (fmap (ListExp "do") (expression `sepEndBy` spaces)) <* eof

preprocessor :: Parser PreProcMap
preprocessor = fmap (foldl' (flip $ uncurry M.insert) M.empty) $ many (preprocessorDef <* newline)

preprocessorDef :: Parser (String, Expression)
preprocessorDef = do
                  char '#'
                  v <- variableLit
                  spaces
                  char '='
                  spaces
                  e <- expression
                  return (v,e)

expression :: Parser Expression
expression = list <|> qlist <|> number <|> variable

numericLit :: Parser Int32
numericLit = (fmap (read) $ many1 (digit <?> "")) <?> "number"

number = fmap IntExp numericLit

list :: Parser Expression
list = (do
       char '(' *> spaces
       x <- variableLit
       spaces1
       xs <- expression `sepEndBy` spaces1
       spaces <* char ')'
       return $ ListExp x xs) <?> "list"

qlist :: Parser Expression
qlist = char '\'' *> fmap QListExp ( between (char '(' *> spaces) (spaces <* char ')') (expression `sepEndBy` spaces1) <?> "qlist" )

variable :: Parser Expression
variable = fmap VarExp variableLit

variableLit = (liftA2 (:) letter (many alphaNum)) <?> "variable"

spaces :: Parser ()
spaces = skipMany space'

spaces1 :: Parser ()
spaces1 = skipMany1 space'

space' :: Parser ()
space' = ((space >> return ()) <|> comment) <?> ""

comment :: Parser ()
comment = char ';' >> many (noneOf "\n") >> newline >> return ()
