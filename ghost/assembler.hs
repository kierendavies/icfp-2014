import System.Environment
import Data.List.Split hiding (oneOf)
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char
import Text.ParserCombinators.Parsec

data Instruction = Mnemonic String [String] | Label String deriving (Show,Eq)
type Program = [Instruction]

--TODO make Show instance
showProgram :: Program -> String
showProgram p = intercalate "\n" $ map showInstruction p

showInstruction (Mnemonic a b) = a ++ " " ++ intercalate "," b
showInstruction (Label a) = undefined

main = do
    filename <- fmap head getArgs
    str <- readFile filename
    let transformedProgram = transformProgram $ parseProgram str
    let newFilename = intercalate "." $ (init $ splitOn "." filename) ++ ["ghc"]
    writeFile newFilename $ showProgram transformedProgram

transformProgram :: Program -> Program
transformProgram program = map (transformInstruction.transformJump labelTable) strippedProgram
    where labelTable = extractLabels program
          strippedProgram = filter (not.isLabel) program
          isLabel (Label _) = True
          isLabel _ = False

transformInstruction (Mnemonic instr xs) = Mnemonic (transformInstr instr) (map transformArg xs)

argTransformations = M.fromList [("UP", "0"),
                                 ("RIGHT", "1"),
                                 ("DOWN", "2"),
                                 ("LEFT", "3"),

                                 ("STANDARD", "0"),
                                 ("FRIGHT", "1"),
                                 ("INVISIBLE", "2"),

                                 ("WALL", "0"),
                                 ("EMPTY", "1"),
                                 ("PILL", "2"),
                                 ("POWER", "3"),
                                 ("FRUIT", "4"),
                                 ("LAMBDASTART", "5"),
                                 ("GHOSTSTART", "6")]

instrTransformations = M.fromList $ zip ["SETDIR","GETLAMBDAPOS","GETLAMBDA2POS","GETINDEX","GETGHOSTSTART","GETGHOSTPOS","GETVITDIR","GETBLOCK","TRACE"] $ map (("INT " ++).show) [1..]


transformArg a = fromMaybe a $ M.lookup a argTransformations
transformInstr i = fromMaybe i $ M.lookup i instrTransformations

jumps = S.fromList ["jlt", "jeq", "jgt"]
transformJump :: M.Map String Int -> Instruction -> Instruction
transformJump m i@(Mnemonic instr (label:xs)) = if S.member instr jumps 
                                                 then Mnemonic instr ((show $ fromJust $ M.lookup label m):xs)
                                                 else i
transformJump _ x = x

extractLabels :: Program -> M.Map String Int
extractLabels p = M.fromList $ fst $ foldl makePair ([],0) $ filter isLabel withNumbers
                    where withNumbers = zip p [1..]
                          isLabel ((Label _), _) = True
                          isLabel _ = False

makePair (list,labelCount) (Label label, lineNum) = (newPair : list, newLabelCount)
    where newLabelCount = labelCount + 1
          newPair = (label, lineNum - newLabelCount)

parseProgram :: String -> Program
parseProgram str = ans
                   where (Right ans) = parse programParser "" str

programParser :: Parser Program
programParser = sepEndBy instructionLineParser $ many1 $ comment <|> (char '\n' >> return ())

instructionLineParser :: Parser Instruction
instructionLineParser = (try labelInstructionParser) <|> mnemonicInstructionLineParser 

mnemonicInstructionLineParser :: Parser Instruction
mnemonicInstructionLineParser = do
                                i <- many1 alphaNum
                                skipMany $ oneOf " \t"
                                a <- sepEndBy (many1 alphaNum) $ char ','
                                return $ Mnemonic i a

comment :: Parser ()
comment = char ';' >> many anyChar >> newline >> return ()

labelInstructionParser :: Parser Instruction
labelInstructionParser = do
                         l <- many1 alphaNum
                         char ':'
                         return $ Label l
