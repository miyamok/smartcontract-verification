module Main where
import Data.Maybe
import Data.Aeson (Value, decode)
import Data.Aeson.Key
import Data.Text as T ( Text, pack, unpack, intercalate )
import System.Directory.Internal.Prelude (getArgs)
import Reader ( reader, readerBS )
import Parser ( parse )
import Conditional ()
import Control.Lens
import Control.Lens.At
import Control.Lens.Fold
import Control.Lens.Prism
import Control.Lens.Indexed
import Data.Aeson.Lens
import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Text.Encoding as TSE
import Data.IntSet.Lens
import qualified Data.Aeson.Key as LB
-- import Profile (absolutePath, contractToEventDefinitions, functionDefinitions, contracts, contractNameAndFunctionNameToBody, contractNameToVariableDeclarations, contractNameToFunctionDefinitions, contractNameToFunctionNames, contractNameAndFunctionNameToArgumentVariableNames, contractNameAndFunctionNameToArgumentTypes, contractNameAndFunctionNameToReturnVariableNames, contractNameAndFunctionNameToReturnTypes, contractToFunctionDefinitions, functionDefinitions, functionDefinitionToName, contractToContractName, functionDefinitionToName, variableDeclarationToName, variableDeclarationToName, contractToVariableDeclarations, eventDefinitionToName, contractToStructDefinitions, structDefinitionToName, contractToEnumDefinitions, enumDefinitionToName, contractToErrorDefinitions, errorDefinitionToName, printContractProfile, functionToBodyStatements)
import SolidityFeature
-- import Data.Aeson.Encode.Pretty
import Data.Text.Lazy.Encoding as TLE ()
import Data.Foldable (for_)
import CFG
import DefinitionUseAnalysis ( cFGEdgesToUnreadVariables, cFGEdgesToDefinitionUseTable )
import Debug.Trace

argsToOptsAndFilename :: [Text] -> ([Text], [Text])
argsToOptsAndFilename args = ([], args)

showFunctionNameAndUnreadVariables :: String -> String -> [Variable] -> String
showFunctionNameAndUnreadVariables text fn vars =
  let ftext = "In function " ++ fn ++ " the following variables are not read after an assignment"
      vtexts = map (showVariableWithLineNumber text) vars
    in Data.List.intercalate "\n" (ftext:vtexts)

charLocationAndTextToLineNumber :: Int -> String -> Int
charLocationAndTextToLineNumber charLoc text =
  let totalLineNumber = length $ lines text
      restLineNumber = length $ lines $ drop (charLoc+1) text
    in totalLineNumber - restLineNumber + 1

charLocationAndFileNameToLineNumber :: Int -> FilePath -> IO Int
charLocationAndFileNameToLineNumber charLoc fn =
  do text <- readFile fn
     return $ charLocationAndTextToLineNumber charLoc text

showVariableWithLineNumber :: String -> Variable -> String
showVariableWithLineNumber text (varName, varType, srcLocation) =
  let (charLocStr, _) = break (==':') srcLocation
      charLoc = read charLocStr :: Int
    in "\"" ++ varName ++ "\" declared at line " ++ show (charLocationAndTextToLineNumber charLoc text)

main :: IO ()
main = do args <- getArgs
          let (opts, files) = argsToOptsAndFilename $ fmap T.pack args
          case files of
            [] -> putStrLn "Error: No .sol file name given"
            _ -> do let filename = head files
                    t <- reader filename
                    putStrLn $ "File to analyze: " ++ fromJust (fileToAbsolutePath t) ++ "\n"
                    let cs = fileToContracts t
                    let functionsList = map contractToFunctionDefinitions cs
                    let functionNamesList = map (map functionDefinitionToName) functionsList
                    let cfgsList = map contractToCFGEdgesList cs
                    let simpleGraph = map (map cFGEdgesToSimpleGraph) cfgsList
                    let unreadsList = map (map cFGEdgesToUnreadVariables) cfgsList
                    let functionNameAndUnreadVarsList = zipWith zip functionNamesList unreadsList
                    let fnUnreads = map (filter (\(fn, unreads) -> not $ null unreads)) functionNameAndUnreadVarsList
                    text <- readFile $ unpack filename
                    let output = concatMap (map (uncurry (showFunctionNameAndUnreadVariables text))) fnUnreads
                    putStrLn $ Data.List.intercalate "\n\n" (map showContractProfile cs)
                    putStrLn ""
                    putStrLn $ Data.List.intercalate "\n\n" output
