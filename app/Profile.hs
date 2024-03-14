{-# LANGUAGE OverloadedStrings #-}
module Profile where

import Control.Lens
import Data.Aeson.Lens
--import Data.ByteString ()
import Data.Aeson.Key ( fromString )
import Data.Text ( Text, pack, unpack )
import Data.Aeson
import Data.Graph
import Data.Foldable (for_)
import Data.Maybe (maybeToList)
--import qualified Data.ByteString as Text
import Debug.Trace

type CFGMatrix = (Value, Int, [Int])
type CFGNode = (Value, String, [String])
-- to contract a Data.Graph via Data.Graph.graphFromEdges
type CFGEdges = [CFGNode]

absolutePath :: AsValue s => s -> Maybe Text
absolutePath t = t ^? key "absolutePath" . _String

contracts :: AsValue s => s -> [Value]
contracts t = t ^.. key "nodes" . values . filtered (has (key "nodeType" . _String . only "ContractDefinition"))

contractToContractName :: AsValue s => s -> Text
contractToContractName t = head $ t ^.. key "name" . _String

contractNameToVariableDeclarations :: AsValue s => s -> String -> [Value]
contractNameToVariableDeclarations t contractName = t ^.. key "nodes" . values . filtered (\x -> has (key "nodeType" . _String . only "ContractDefinition") x && has (key "name" . _String . only (Data.Text.pack contractName)) x) . key "nodes" . values . filtered (has (key "nodeType"._String.only "VariableDeclaration"))

contractToFunctionDefinitions :: AsValue s => s -> [Value]
contractToFunctionDefinitions t = t ^.. key "nodes" . values . filtered (has (key "nodeType"._String.only "FunctionDefinition"))

variableDeclarations :: AsValue s => s -> [Value]
variableDeclarations t = t ^.. key "nodes" . values . key "nodes" . values . filtered (has (key "nodeType"._String.only "VariableDeclaration"))

functionDefinitions :: AsValue s => s -> [Value]
functionDefinitions t = t ^.. key "nodes" . values . key "nodes" . values . filtered (has (key "nodeType"._String.only "FunctionDefinition"))

contractNameToFunctionDefinitions :: AsValue s => s -> String -> [Value]
contractNameToFunctionDefinitions t contractName = t ^.. key "nodes" . values . filtered (\x -> has (key "nodeType" . _String . only "ContractDefinition") x && has (key "name" . _String . only (Data.Text.pack contractName)) x) . key "nodes" . values . filtered (has (key "nodeType"._String.only "FunctionDefinition"))

contractNameToFunctionNames :: AsValue s => s -> String -> [Text]
contractNameToFunctionNames t contractName = t ^.. key "nodes" . values . filtered (\x -> has (key "nodeType" . _String . only "ContractDefinition") x && has (key "name" . _String . only (Data.Text.pack contractName)) x) . key "nodes" . values . filtered (has (key "nodeType"._String.only "FunctionDefinition")) . key "name" . _String

contractNameAndFunctionNameToArgumentTypes :: AsValue s => s -> String -> String -> [Text]
contractNameAndFunctionNameToArgumentTypes t contractName functionName =
    t ^.. key "nodes" . values . filtered (\x -> has (key "nodeType" . _String . only "ContractDefinition") x && has (key "name" . _String . only (Data.Text.pack contractName)) x) . key "nodes" . values . filtered (has (key "nodeType"._String.only "FunctionDefinition")) . filtered (has (key "name"._String.only (Data.Text.pack functionName))) . key "parameters" . key "parameters" . values.key "typeDescriptions" . key "typeString" . _String

contractNameAndFunctionNameToArgumentVariableNames :: AsValue s => s -> String -> String -> [Text]
contractNameAndFunctionNameToArgumentVariableNames t contractName functionName =
    t ^.. key "nodes" . values . filtered (\x -> has (key "nodeType" . _String . only "ContractDefinition") x && has (key "name" . _String . only (Data.Text.pack contractName)) x) . key "nodes" . values . filtered (has (key "nodeType"._String.only "FunctionDefinition")) . filtered (has (key "name"._String.only (Data.Text.pack functionName))) . key "parameters" . key "parameters" . values.key "name" . _String

contractNameAndFunctionNameToReturnTypes :: AsValue s => s -> String -> String -> [Text]
contractNameAndFunctionNameToReturnTypes t contractName functionName =
    t ^.. key "nodes" . values . filtered (\x -> has (key "nodeType" . _String . only "ContractDefinition") x && has (key "name" . _String . only (Data.Text.pack contractName)) x) . key "nodes" . values . filtered (has (key "nodeType"._String.only "FunctionDefinition")) . filtered (has (key "name"._String.only (Data.Text.pack functionName))) . key "returnParameters" . filtered (has (key "nodeType"._String.only (Data.Text.pack "ParameterList"))) . key (fromString "parameters") . values . key (fromString "typeDescriptions") . key (fromString "typeString") . _String

contractNameAndFunctionNameToReturnVariableNames :: AsValue s => s -> String -> String -> [Text]
contractNameAndFunctionNameToReturnVariableNames t contractName functionName =
    t ^.. key "nodes" . values . filtered (\x -> has (key "nodeType" . _String . only "ContractDefinition") x && has (key "name" . _String . only (Data.Text.pack contractName)) x) . key "nodes" . values . filtered (has (key "nodeType"._String.only "FunctionDefinition")) . filtered (has (key "name"._String.only (Data.Text.pack functionName))) . key "returnParameters" . filtered (has (key "nodeType"._String.only (Data.Text.pack "ParameterList"))) . key (fromString "parameters") . values . key "name" . _String

functionDefinitionToName :: AsValue s => s -> Text
functionDefinitionToName t = head $ t ^.. key "name" . _String

contractToVariableDeclarations :: AsValue s => s -> [Value]
--contractToVariableDeclarations t = t ^.. key "nodes" . values . filtered (has (key "nodeType"._String.only "VariableDeclaration"))
contractToVariableDeclarations = contractToDefinitionsOrDeclarations "VariableDeclaration"

variableDeclarationToName :: AsValue s => s -> Text
variableDeclarationToName = definitionOrDeclarationToName

-- struct-definition, event-definition, enum-definition, constant-variable-declaration, error-definition

contractToDefinitionsOrDeclarations :: AsValue s => Text -> s -> [Value]
contractToDefinitionsOrDeclarations s t = t ^.. key "nodes" . values . filtered (has (key "nodeType"._String.only s))

definitionOrDeclarationToName :: AsValue s => s -> Text
definitionOrDeclarationToName t = head $ t ^.. key "name" . _String

contractToEventDefinitions :: AsValue s => s -> [Value]
--contractToEventDefinitions t = t ^.. key "nodes" . values . filtered (has (key "nodeType"._String.only "EventDefinition"))
contractToEventDefinitions = contractToDefinitionsOrDeclarations "EventDefinition"

eventDefinitionToName :: AsValue s => s -> Text
eventDefinitionToName = definitionOrDeclarationToName

contractToStructDefinitions :: AsValue s => s -> [Value]
contractToStructDefinitions = contractToDefinitionsOrDeclarations "StructDefinition"
--contractToStructDefinitions t = t ^.. key "nodes" . values . filtered (has (key "nodeType"._String.only "StructDefinition"))

structDefinitionToName :: AsValue s => s -> Text
structDefinitionToName = definitionOrDeclarationToName

contractToEnumDefinitions :: AsValue s => s -> [Value]
--contractToEnumDefinitions t = t ^.. key "nodes" . values . filtered (has (key "nodeType"._String.only "EnumDefinition"))
contractToEnumDefinitions = contractToDefinitionsOrDeclarations "EnumDefinition"

enumDefinitionToName :: AsValue s => s -> Text
enumDefinitionToName = definitionOrDeclarationToName

contractToErrorDefinitions :: AsValue s => s -> [Value]
contractToErrorDefinitions = contractToDefinitionsOrDeclarations "ErrorDefinition"
--contractToErrorDefinitions t = t ^.. key "nodes" . values . filtered (has (key "nodeType"._String.only "ErrorDefinition"))

errorDefinitionToName :: AsValue s => s -> Text
errorDefinitionToName = definitionOrDeclarationToName

-- This function requires a consideration on control flows which come from if-then-else etc.
---------
-- contractNameAndFunctionNameToReturnValues :: AsValue s => s -> String -> String -> [Text]
-- contractNameAndFunctionNameToReturnValues t contractName functionName =
--     if null returnValues
--         then t ^.. key "nodes" . values . filtered (\x -> has (key "nodeType" . _String . only "ContractDefinition") x && has (key "name" . _String . only (Data.Text.pack contractName)) x) . key "nodes" . values . filtered (has (key "nodeType"._String.only "FunctionDefinition")) . filtered (has (key "name"._String.only (Data.Text.pack functionName))) . key "returnParameters" . filtered (has (key "nodeType"._String.only (Data.Text.pack "ParameterList"))) . key (fromString "parameters") . values . key "name" . _String
--         else varNames
--     where
--         returnValues = t ^.. key "nodes" . values . filtered (\x -> has (key "nodeType" . _String . only "ContractDefinition") x && has (key "name" . _String . only (Data.Text.pack contractName)) x) . key "nodes" . values . filtered (has (key "nodeType"._String.only "FunctionDefinition")) . filtered (has (key "name"._String.only (Data.Text.pack functionName)))
--         varNames = contractNameAndFunctionNameToReturnVariableNames t contractName functionName

contractNameAndFunctionNameToBody :: AsValue s => s -> String -> String -> [Value]
contractNameAndFunctionNameToBody t contractName functionName =
    t ^.. key "nodes" . values . filtered (\x -> has (key "nodeType" . _String . only "ContractDefinition") x && has (key "name" . _String . only (Data.Text.pack contractName)) x) . key "nodes" . values . filtered (has (key "nodeType"._String.only "FunctionDefinition")) . filtered (has (key "name"._String.only (Data.Text.pack functionName))) . key "body"

functionToCFGNodes :: AsValue s => s -> [CFGNode]
--functionToCFGNodes t = statementsToCFGNodes $ functionToBodyStatements t
functionToCFGNodes t = statementsToCFGNodes $ t ^.. key "body" . values

-- body should be kept in CFG, so that the notion of scope is explicit there.

-- functionToBodyStatements :: AsValue s => s -> [Value]
-- functionToBodyStatements t = t ^.. key "body" . key "statements" . values

blockToStatements :: AsValue s => s -> [Value]
blockToStatements t = t ^.. key "statements" . values

-- cFGMatricesToEntryId :: [CFGMatrix] -> Int
-- cFGMatricesToEntryId ((_, i, _):_) = i

cFGNodesToEntryNodeId :: [CFGNode] -> String
cFGNodesToEntryNodeId ((_, nodeId, _):_) = nodeId

concatCFGMatrices :: [CFGMatrix] -> [CFGMatrix] -> [CFGMatrix]
concatCFGMatrices cfg1 cfg2 = undefined
--- look for unfilled next id's in cfg1 and fill them by entry of cfg2

statementAndNextNodeIdToCFGNodes :: Value -> Maybe String -> [CFGNode]
statementAndNextNodeIdToCFGNodes stat mNextNodeId
    | nodeType == "Block" = blockAndNextNodeIdToCFGNodes stat mNextNodeId
    | nodeType == "VariableDeclarationStatement" = [(stat, id, maybeToList mNextNodeId)]
    | nodeType == "ExpressionStatement" = [(stat, id, maybeToList mNextNodeId)]
    | nodeType == "EmitStatement" = [(stat, id, maybeToList mNextNodeId)]
    | nodeType == "RevertStatement" = [(stat, id, [])]
    | nodeType == "Return" = [(stat, id, [])]
    | nodeType == "IfStatement" = ifStatementAndNextNodeIdToCFGNodes stat mNextNodeId
    | otherwise = [(stat, id, ["END"])]
    where
        nodeType = let ns = stat ^.. key "nodeType" . _String
                    in if null ns then "" else head ns
        id = unpack $ head $ stat ^.. key "src" . _String

blockAndNextNodeIdToCFGNodes :: AsValue s => s -> Maybe String -> [CFGNode]
blockAndNextNodeIdToCFGNodes stat mNextNodeId = undefined
    where
        innerStats = undefined

statementsAndNextNodeIdToCFGNodes :: [Value] -> Maybe String -> [CFGNode]
statementsAndNextNodeIdToCFGNodes [] _ = []
statementsAndNextNodeIdToCFGNodes (stat:stats) mNextNodeId =
    statementAndNextNodeIdToCFGNodes stat mNodeId ++ statementsAndNextNodeIdToCFGNodes stats mNextNodeId
    where
        mNodeId = if null stats then mNextNodeId
                        else Just $ src $ head stats

nodeType :: AsValue s => s -> String
nodeType t = unpack $ head $ t ^.. key "nodeType" . _String

src :: AsValue s => s -> String
src t = unpack $ head $ t ^.. key "src" . _String

-- statementToEntryNodeId :: Value -> String
-- statementToEntryNodeId t = src entryStat
--     where
--         entryStat = if nodeType t == "Block" then head $ blockToStatements t
--                        else t

statementsToCFGNodes :: [Value] -> [CFGNode]
statementsToCFGNodes stats = statementsAndNextNodeIdToCFGNodes stats Nothing

-- statementsToCFGEdgesList :: [Value] -> [CFGEdges]
-- statementsToCFGEdgesList = followingNodeIdAndStatementsToCFGEdgesList Nothing

-- followingNodeIdAndStatementsToCFGEdgesList :: Maybe String -> [Value] -> [CFGEdges]
-- followingNodeIdAndStatementsToCFGEdgesList _ [] = []
-- followingNodeIdAndStatementsToCFGEdgesList mFollowingNodeId (stat:stats)
--     | nt == "Block" = cFGEdges:restCFGEdgesList
--     | nt == "VariableDeclarationStatement" = [(stat, id, maybeToList mNextNodeId)]
--     | nt == "ExpressionStatement" = [(stat, id, maybeToList mNextNodeId)]
--     | nt == "EmitStatement" = [(stat, id, maybeToList mNextNodeId)]
--     | nt == "RevertStatement" = [(stat, id, [])]
--     | nt == "Return" = [(stat, id, [])]
--     | nt == "IfStatement" = ifStatementAndNextNodeIdToCFGNodes stat mNextNodeId
--     | otherwise = [(stat, id, ["END"])]
--     where
--         nt = nodeType stat
--         id = src stat
--         restCFGEdgesList = followingNodeIdAndStatementsToCFGEdgesList mFollowingNodeId stats
--         mNextNodeId = if null stats then mFollowingNodeId else Just $ src $ head stats
--         cFGEdges = followingNodeIdAndStatementToCFGEdges mNextNodeId stat
--         innerBlocks = (stat:stats) ^.. filtered (\x -> has (key "nodeType" . _String . only "Block") x || has (key "nodeType" . _String . only "IfStatement") x)

-- followingNodeIdAndStatementToCFGEdges :: Maybe String -> Value -> CFGEdges
-- followingNodeIdAndStatementToCFGEdges mFollowingNodeId stat = undefined

statementsToCFGEdgesList :: [Value] -> [CFGEdges]
statementsToCFGEdgesList stats = followingNodeIdAndStatementsToCFGEdgesListAux Nothing stats [] []

followingNodeIdAndStatementsToCFGEdgesListAux :: Maybe String -> [Value] -> CFGEdges -> [CFGEdges] -> [CFGEdges]
followingNodeIdAndStatementsToCFGEdgesListAux _ [] es ess = es:ess
followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId (stat:stats) es ess
    | nt == "Block" = let es' = es ++ [(stat, id, nextNodeIdList)]
                          innerStats = blockToStatements stat
                          ess' = ess ++ followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId innerStats [] []
                      in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess'
    | nt == "VariableDeclarationStatement" = let es' = es ++ [(stat, id, nextNodeIdList)]
                                             in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess
    | nt == "ExpressionStatement" = let es' = es ++ [(stat, id, nextNodeIdList)]
                                    in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess
    | nt == "EmitStatement" = let es' = es ++ [(stat, id, nextNodeIdList)]
                              in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess
    | nt == "RevertStatement" = let es' = es ++ [(stat, id, [])]
                                in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess
    | nt == "Return" = let es' = es ++ [(stat, id, [])]
                       in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess
    | nt == "IfStatement" = let es' = es ++ [(stat, id, nextNodeIdList)]
                                trueCaseStatements = stat ^.. key "trueBody" . key "statements" . values
                                falseCaseStatements = traceShowId $ stat ^.. key "falseBody" . key "statements" . values
                                trueCaseCFGEdges = followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId trueCaseStatements [] []
                                falseCaseCFGEdges = followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId falseCaseStatements [] []
                                ess' = if null $ stat ^.. filtered (has (key "falseBody")) then ess ++ trueCaseCFGEdges
                                        else ess ++ trueCaseCFGEdges ++ falseCaseCFGEdges
                            in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess'
    | otherwise = let es' = es ++ [(stat, id, ["END"])]
                  in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess
    where
        nt = nodeType stat
        id = src stat
        nextNodeIdList = if null stats then maybeToList mFollowingNodeId else [src $ head stats]

functionToStatements :: AsValue s => s -> [Value]
functionToStatements t = t ^.. key "body" . key "statements" . values

simplifyCFGEdges :: CFGEdges -> [(String, [String])]
simplifyCFGEdges [] = []
simplifyCFGEdges ((stat, id, ids):es) = (id, ids):(simplifyCFGEdges es)

-- statementsToCFGNodes [] = []
-- statementsToCFGNodes (stat:stats)
--     | nodeType == "VariableDeclarationStatement" = (stat, i, [restEntryNodeId]):restCFGNodes
--     | nodeType == "ExpressionStatement" = (stat, i, [restEntryNodeId]):restCFGNodes
--     | nodeType == "EmitStatement" = (stat, i, [restEntryNodeId]):restCFGNodes
--     | nodeType == "RevertStatement" = (stat, i, []):restCFGNodes
--     | nodeType == "Return" = (stat, i, []):restCFGNodes
--     | nodeType == "IfStatement" =
--         let trueStatements = stat ^.. key "trueBody" . key "statements"
--             falseStatements = stat ^.. key "falseBody" . key "statements"
--             statementAfterIfStatement = if null stats then [] else [head stats]
--             trueCFGNodes = init $ statementsToCFGNodes (trueStatements ++ statementAfterIfStatement)
--             trueEntryNodeId = cFGNodesToEntryNodeId trueCFGNodes
--             falseCFGNodes = if null (falseStatements ++ statementAfterIfStatement) then []
--                                 else init $ statementsToCFGNodes (falseStatements ++ statementAfterIfStatement)
--             nextNodeIds = case falseCFGNodes of [] -> [trueEntryNodeId]
--                                                 _ -> [trueEntryNodeId, cFGNodesToEntryNodeId falseCFGNodes]
--           in  (stat, i, nextNodeIds):trueCFGNodes++falseCFGNodes
--     | otherwise = [(stat, i, ["END"])]
--     where
--         nodeType = let ns = stat ^.. key "nodeType" . _String
--                     in if null ns then "" else head ns
--         i = unpack $ head $ stat ^.. key "src" . _String
-- --        js = []
--         restCFGNodes = statementsToCFGNodes stats
--         restEntryNodeId = cFGNodesToEntryNodeId restCFGNodes

-- statementToCFGNodes :: Value -> [CFGNode]
-- statementToCFGNodes stat
--     | nodeType == "VariableDeclarationStatement" = [(stat, i, js)]
--     | nodeType == "ExpressionStatement" = [(stat, i, js)]
--     | nodeType == "EmitStatement" = [(stat, i, js)]
--     | nodeType == "RevertStatement" = [(stat, i, [])]
--     | nodeType == "Return" = [(stat, i, [])]
--     | nodeType == "IfStatement" = ifStatementToCFGNodes stat
-- --    | otherwise = [(stat, i, [i+1])]
--     where
--         nodeType = head $ stat ^.. key "nodeType" . _String
--         i = unpack $ head $ stat ^.. key "src" . _String
--         js = []

ifStatementAndNextNodeIdToCFGNodes :: Value -> Maybe String -> [CFGNode]
ifStatementAndNextNodeIdToCFGNodes stat ms = undefined
    where
        trueCaseStatements = stat ^.. key "trueBody" . key "statements"


        falseCaseStatements = stat ^.. key "falseBody" . key "statements"

ifStatementToCFGNodes t = [(trueBody, trueNodeId, [])]
    where
        trueBody = head $ t ^.. key "trueBody"
        trueNodeId = unpack $ head $ trueBody ^.. key "statements" . values . key "src" . _String
        falseBody = head $ t ^.. key "falseBody"
        falseNodeId = head $ falseBody ^.. key "src" . _String

ifStatementToTrueBody t = t ^.. key "trueBody"

ifStatementToTrueStatements t = t ^.. key "trueBody" . key "statements"

statementsToIndexedCFGMatrices :: [Value] -> [CFGMatrix]
statementsToIndexedCFGMatrices = indexAndStatementsToCFGMatrices 0

-- the node id (currently Int) could be replaced by src data, the position in the source code and hence unique, in AST?
indexAndStatementToCFGMatrices :: Int -> Value -> [CFGMatrix]
indexAndStatementToCFGMatrices i stat
--    | nodeType == "Assignment" = [(stat, i, [i+1])]
    | nodeType == "VariableDeclarationStatement" = [(stat, i, [i+1])]
    | nodeType == "ExpressionStatement" = [(stat, i, [i+1])]
    | nodeType == "EmitStatement" = [(stat, i, [i+1])]
    | nodeType == "RevertStatement" = [(stat, i, [])]
    | nodeType == "Return" = [(stat, i, [])]
    | nodeType == "IfStatement" = indexAndIfStatementToCFGMatrices i stat
--    | otherwise = [(stat, i, [i+1])]
    where
        nodeType = head $ stat ^.. key "nodeType" . _String

indexAndIfStatementToCFGMatrices :: Int -> Value -> [CFGMatrix]
indexAndIfStatementToCFGMatrices i t = t ^.. undefined

indexAndStatementsToCFGMatrices :: Int -> [Value] -> [CFGMatrix]
indexAndStatementsToCFGMatrices i [] = []
indexAndStatementsToCFGMatrices i (stat:stats) = cFGMatrices ++ restCFGMatrices
    where
        cFGMatrices = indexAndStatementToCFGMatrices i stat
        j = i + length cFGMatrices
        restCFGMatrices = indexAndStatementsToCFGMatrices j stats



    --     f id = \stat ->
    --         if nodeType == "Assignment"
    --             then [stat, id, id+1]
    --         let nodeType = stat ^.. key "nodeType" . _String

    --     f (stat:stats) = 
    -- in if null statements
    --     then []
    --     else let statement = head statements
    --              rest = tail statements




--    t ^.. key "nodes" . values . filtered (\x -> has (key "nodeType" . _String . only "ContractDefinition") x && has (key "name" . _String . only (Data.Text.pack contractName)) x) . key "nodes" . values . filtered (has (key "nodeType"._String.only "FunctionDefinition")) . filtered (has (key "name"._String.only (Data.Text.pack functionName))) . key "body" . key "statements"
--    where body = undefined

-- sourceProfile :: ByteString -> IO ()
-- sourceProfile bs = let
--     srcPath = bs ^? key (fromString "absolutePath") ._String
--     contractName = bs ^? key (fromString "nodes") . values . key (fromString "name") ._String
--     nodes = bs ^.. key (fromString "nodes") . values . key (fromString "nodes") . values . key (fromString "kind")
--     --kinds = bs ^? key (fromString "nodes") . values . key (fromString "nodes") . key (fromString "nodeType")

--     in do
--         print srcPath
--         print contractName
--         print nodes
--         --print kinds

-- --     do
-- --     print $ bs ^? key (fromString "absolutePath") ._String
-- --     let nodes = bs ^? key (fromString "nodes") . values
-- --     print $ bs ^? key (fromString "nodes") . values . key (fromString "ContractDefinition")
-- --     print $ bs ^? key (fromString "nodes") . values
-- -- --    print bs ^? 

printProfile :: AsValue s => s -> IO ()
printProfile t = do for_ (contracts t) printContractProfile

printContractProfile :: AsValue s => s -> IO ()
printContractProfile t = do putStrLn $ "Contract name: " ++ unpack cName
                            putStrLn $ "Declared variable names: " ++ show vs
                            putStrLn $ "Defined function names: " ++ show fs
                            putStrLn $ "Defined enum names: " ++ show es
                            putStrLn $ "Defined struct names: " ++ show ss
                            putStrLn $ "Defined event names: " ++ show es
                            putStrLn $ "Defined error names: " ++ show errs
    where
        cName = contractToContractName t
        vs = map variableDeclarationToName $ contractToVariableDeclarations t
        fs = map functionDefinitionToName $ contractToFunctionDefinitions t
        ss = map structDefinitionToName $ contractToStructDefinitions t
        es = map enumDefinitionToName $ contractToEnumDefinitions t
        errs = map errorDefinitionToName $ contractToErrorDefinitions t
        evs = map eventDefinitionToName $ contractToEventDefinitions t
