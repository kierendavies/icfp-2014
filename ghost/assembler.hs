import System.Environment
import Data.List.Split
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char

data Instruction = Instruction String [String] | Label String deriving (Show,Eq)
type Program = [Instruction]

--TODO make Show instance
showProgram :: Program -> String
showProgram p = intercalate "\n" $ map showInstruction p

showInstruction (Instruction a b) = a ++ " " ++ intercalate "," b
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

transformInstruction (Instruction instr xs) = Instruction (transformInstr instr) (map transformArg xs)

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
transformJump m i@(Instruction instr (label:xs)) = if S.member instr jumps 
                                                 then Instruction instr ((show $ fromJust $ M.lookup label m):xs)
                                                 else i
transformJump _ x = x

extractLabels :: Program -> M.Map String Int
extractLabels p = M.fromList $ fst $ foldl makePair ([],0) $ filter isLabel withNumbers
                    where withNumbers = zip p [1..]
                          isLabel ((Label _), _) = True
                          isLabel _ = False

makePair (list,labelCount) (Label label, lineNum) = (newPair : list, newLabelCount)
    where newLabelCount = labelCount + 1
          newPair = (init label, lineNum - newLabelCount)

parseProgram :: String -> Program
parseProgram str = catMaybes $ map parseLine $ filter ((0<).length) lins
                    where lins = lines str

parseLine :: String -> Maybe Instruction
parseLine [] = Nothing
parseLine line
    | last line == ':' = Just $ Label line
    | otherwise        = if length spacedParts == 1
                         then Just $ Instruction line []
                         else Just $ Instruction (head spacedParts) $ removeEmpty commaedParts
        where spacedParts = splitOn " " line
              commaedParts = splitOn "," $ spacedParts !! 1
              removeEmpty ("":[]) = []
              removeEmpty xs = xs

