module Main where
import Data.Maybe
import Data.Aeson (Value, decode)
import Data.Aeson.Key
import Data.Text as T ( Text, pack, unpack )
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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Text.Encoding as TSE
import Data.IntSet.Lens
import qualified Data.Aeson.Key as LB
-- import Profile (absolutePath, contractToEventDefinitions, functionDefinitions, contracts, contractNameAndFunctionNameToBody, contractNameToVariableDeclarations, contractNameToFunctionDefinitions, contractNameToFunctionNames, contractNameAndFunctionNameToArgumentVariableNames, contractNameAndFunctionNameToArgumentTypes, contractNameAndFunctionNameToReturnVariableNames, contractNameAndFunctionNameToReturnTypes, contractToFunctionDefinitions, functionDefinitions, functionDefinitionToName, contractToContractName, functionDefinitionToName, variableDeclarationToName, variableDeclarationToName, contractToVariableDeclarations, eventDefinitionToName, contractToStructDefinitions, structDefinitionToName, contractToEnumDefinitions, enumDefinitionToName, contractToErrorDefinitions, errorDefinitionToName, printContractProfile, functionToBodyStatements)
import Profile
-- import Data.Aeson.Encode.Pretty
import Data.Text.Lazy.Encoding as TLE
import Data.Foldable (for_)
import CFG
import LiveVariables

argsToOptsAndFilename :: [Text] -> ([Text], [Text])
argsToOptsAndFilename args = ([], args)

main :: IO ()
main = do args <- getArgs
          let (opts, files) = argsToOptsAndFilename $ fmap T.pack args
          case files of
            [] -> putStrLn "Error: No .sol file name given"
            _ -> do t <- reader (head files)
                    print $ absolutePath t
                    --let cs = contractDefinitions t
                    --let c = head cs
                    --print $ c ^.. key (fromString "name") . _String
                    --let fs = functionDefinitions c
                    --print $ fs . values ^? key (fromString "name")
                    --let a = do vadecs <- contractToVariableDeclarations c

                    --print $ contractTofunctionDefinitions c

                    --putStrLn $ encodePretty $ t ^.. _Value-- ^.. _Value
--                     print $ contractNameToVariableDeclarations t "Counter"
--                     print $ contractNameToFunctionDefinitions t "Counter"
--                     let functionNames = contractNameToFunctionNames t "Counter"
--                     print functionNames
--                     let functionRetTypes = contractNameAndFunctionNameToReturnTypes t "Counter" (unpack (head functionNames))
-- --                    print $ length functionRetTypes
--                     print functionRetTypes
--                     print $ contractNameAndFunctionNameToReturnVariableNames t "Counter" (unpack (head functionNames))
--                     let body = contractNameAndFunctionNameToBody t "Counter" (unpack (head functionNames))
--                     print $ blocksToCFGMatrices body
                    --print $ length $ blocksToCFGMatrices body
                    let cs = contracts t
                    let c = head cs
                    let vdecs = contractToStateVariableDeclarations c
                    print vdecs
                    -- print $ length vdecs
                    -- --let fdefs = contractToFunctionDefinitions c
                    -- let env = map variableDeclarationToVariable vdecs
                    -- print env
                    let fs = contractToFunctionDefinitions c
                    let f = head fs
                    let fn = functionDefinitionToName f
                    print fn
                    --print f
                    let fstats = functionDefinitionToBodyStatements f
                    print $ functionDefinitionToParameterVariables f
                    let cfgs = contractToCFGEdgesList c
                    print $ map simplifyCFGEdges cfgs
                    --print cfgs

--                     let cNames = map contractToContractName cs
--                     --print $ head cs
--                     print cNames
--                     let functionsList = map contractToFunctionDefinitions cs
--                     --print $ functionToBodyStatements (head $ head functionsList)
--                     --let cfg = functionToCFGNodes (head $ head functionsList)
--                     --print cfg
--                     let fstatsList = map functionToStatements (head functionsList)
--                     print $ map length fstatsList
--                     let cfgsList = map (\x -> statementsAndEnvironmentToCFGEdgesList x []) fstatsList
--                     print $ length cfgsList
--                     print $ map (map simplifyCFGEdges) cfgsList
--                     let functionNamesList = map (map functionDefinitionToName) functionsList
--                     print functionNamesList
--                     let vDecs = map contractToVariableDeclarations cs
--                     let vDecNames = map (map variableDeclarationToName) vDecs
--                     print vDecNames
--                     let events = contractToEventDefinitions $ head cs
--                     --print events
--                     let eventNames = map eventDefinitionToName events
--                     print eventNames
--                     let structs = contractToStructDefinitions $ head cs
--                     let structNames = map structDefinitionToName structs
--                     print structNames
--                     let enums = contractToEnumDefinitions $ head cs
--                     let enumNames = map enumDefinitionToName enums
--                     print enumNames

--                     let errs = contractToErrorDefinitions $ head cs
--                     let errNames = map errorDefinitionToName errs
--                     print errNames
--                     let fct = head $ contractToFunctionDefinitions (head cs)
--                     print fct
--                     print $ functionDefinitionToParameterVariables fct
--                     print $ functionDefinitionToReturnVariables fct
--                     --for_ cs printContractProfile
--                     let block = functionToStatements fct
--                     --for_ block (print . nodeType)
--                     --for_ block (print . statementToReadVariables)
--                     --for_ block (print . statementToUpdatedVariables)
--                     let s = block !! 2
--                     -- -- --print $ expressionStatementToExpression s
--                     -- -- print $ expressionInFunctionCallFormToArguments $ expressionStatementToExpression s
--                     -- print $ statementToReadVariables s
--                     print $ expressionInFunctionCallFormToArguments $ expressionStatementToExpression s
--                     -- print $ statementToUpdatedVariables s
--                     -- -- print $ variableDeclarationStatementToDeclaredVariable $ head block
--                     -- let initValue = variableDeclarationStatementToInitialValue $ head block
--                     -- print initValue
--                     -- -- print $ expressionInBinaryOperationFormToExpressions initValue
--                     -- -- print $ map expressionInIdentifierFormToVariable $ expressionInBinaryOperationFormToExpressions initValue
--                     -- -- print $ head block
--                     -- -- print $ expressionToFreeVariables initValue
--                     -- let lastExpr = expressionStatementToExpression $ last block
--                     -- print lastExpr
--                     -- print $ expressionInAssignmentFormToUpdatedVariables lastExpr
--                     -- print $ expressionInAssignmentFormToReadVariables lastExpr


--                     -- let headExpr = head block
--                     -- --print headExpr
--                     -- -- let lhs = expressionInAssignmentFormToLeftExpression headExpr
--                     -- -- print lhs
--                     -- -- print $ expressionInMemberAccessFormToVariable lhs
--                     -- -- print $ expressionInMemberAccessFormToMemberName lhs
--                     -- -- print $ expressionToFreeVariables lhs
--                     -- print $ variableDeclarationStatementToReadVariables headExpr
--                     -- print $ variableDeclarationStatementToUpdatedVariables headExpr
                    
--                     -- let rhs = expressionInAssignmentFormToRightExpression headExpr
--                     -- print rhs
--                     -- print $ expressionInMemberAccessFormToVariable rhs
--                     -- print $ expressionInMemberAccessFormToMemberName rhs
--                     -- print $ expressionInAssignmentFormToReadVariables headExpr
--                     -- print $ expressionInAssignmentFormToUpdatedVariables headExpr

--                     --print (expressionToFreeVariables $ variableDeclarationStatementToInitialValue $ head block)
--                     -- --print $ head block
--                     -- print fct

--                     -- let functionArgTypes = contractNameAndFunctionNameToArgumentTypes t "Counter" (unpack (head functionNames))
--                     -- print $ length functionArgTypes
--                     -- print functionArgTypes
--                     -- print $ contractNameAndFunctionNameToArgumentVariableNames t "Counter" (unpack (head functionNames))

--                     -- print $ t ^.. key (fromString "nodes")
--                     -- print $ t ^.. key (fromString "nodes") . values . key (fromString "nodes") . values . key (fromString "nodeType")
--                     -- print $ t ^.. key (fromString "nodes") . values . key (fromString "nodes") . values . key (fromString "name") . _String
--                     -- print $ t ^.. key (fromString "nodes") . values . key (fromString "nodes") . filtered (allOf (values.key (fromString "nodeType")) (has _String)) . values . key (fromString "name") ._String
--                     -- print $ t ^.. key (fromString "nodes") . values . key (fromString "nodes") . values . filtered (has (key (fromString "nodeType")._String.only (T.pack "FunctionDefinition"))) . key (fromString "name")._String
--                     -- print $ t ^.. key (fromString "nodes") . values . key (fromString "nodes") . values . filtered (has (key (fromString "nodeType")._String.only (T.pack "VariableDeclaration"))) . key (fromString "name")._String
--                     -- print $ t ^.. key (fromString "nodes") . values . filtered (has (key (fromString "nodeType")._String.only (T.pack "ContractDefinition"))) . key (fromString "name")._String

-- --contractDefinitions t = t ^.. key (fromString "nodes") . values . filtered (has (key (fromString "nodeType")._String.only (T.pack "ContractDefinition")))
--                     -- case mv of Nothing -> print "empty!"
--                     --            Just v -> print $ v ^? key (fromString "absolutePath") ._String

--           --                     --  Just v -> print $ do src <- v ^? key (fromString "src")
--           --                     --                       nodes <- v ^? key (fromString "nodes")
--           --                     --                       nodeType <- v ^? key (fromString "nodeType")
--           --                     --                       let srcs = v ^? values . key (fromString "src")
--           --                     --                       return src
--           --                                           --return $ show nodeType ++ " " ++ show src ++ ": " ++ show srcs

--           -- --           -- print mv
--           -- --           -- bs <- B.readFile $ (T.unpack (files !! 0))
--           -- --           -- let j = bs ^.. values.key (fromString "nodeType")._String
--           -- -- return
