{-# LANGUAGE OverloadedStrings #-}
module Profile where

import Control.Lens
import Data.Aeson.Lens
--import Data.ByteString ()
import Data.Aeson.Key ( fromString )
import Data.Text ( Text, pack, unpack )
import Data.List
import Data.Aeson
import Data.Graph
import Data.Foldable (for_)
import Data.Maybe (maybeToList, listToMaybe)
--import qualified Data.ByteString as Text
import Debug.Trace

type CFGMatrix = (Value, Int, [Int])
type VarName = String
type Variable = (VarName, String, String) -- name, type, and src (pointing the declaration statement, but no nameLocation!)
-- data Variable = VarDeclaration VarName String String | Pointer VarName 
-- type Pointer = (VarName, Variable)
-- data VarInfo = VarDef Int Variable | StoragePointer Variable Variable
--type Environment = [Variable] -- reference to available variables

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

contractToStateVariableDeclarations :: AsValue s => s -> [Value]
--contractToVariableDeclarations t = t ^.. key "nodes" . values . filtered (has (key "nodeType"._String.only "VariableDeclaration"))
contractToStateVariableDeclarations = contractToDefinitionsOrDeclarations "VariableDeclaration"

variableDeclarationToName :: AsValue s => s -> String
variableDeclarationToName = unpack . definitionOrDeclarationToName

variableDeclarationToType :: AsValue s => s -> String
variableDeclarationToType = unpack . definitionOrDeclarationToType

variableDeclarationToVariable :: AsValue s => s -> Variable
variableDeclarationToVariable v = (n, t, s)
    where
        n = variableDeclarationToName v
        t = variableDeclarationToType v
        s = src v

variableDeclarationToStorageLocation :: AsValue s => s -> String
variableDeclarationToStorageLocation t = unpack $ head $ t ^.. key "storageLocation" . _String

-- Note: declarations is an array, which is nullable according to the AST specification.
-- Currently it assumes the array is non-empty, and counts only the first element.
variableDeclarationStatementToVariableDeclaration :: AsValue s => s -> Value
variableDeclarationStatementToVariableDeclaration stat = head $ stat ^.. key "declarations" . values

-- struct-definition, event-definition, enum-definition, constant-variable-declaration, error-definition

contractToDefinitionsOrDeclarations :: AsValue s => Text -> s -> [Value]
contractToDefinitionsOrDeclarations s t = t ^.. key "nodes" . values . filtered (has (key "nodeType"._String.only s))

definitionOrDeclarationToName :: AsValue s => s -> Text
definitionOrDeclarationToName t = head $ t ^.. key "name" . _String

definitionOrDeclarationToType :: AsValue s => s -> Text
definitionOrDeclarationToType t = head $ t ^..  key "typeDescriptions" . key "typeString" . _String

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


-- body should be kept in CFG, so that the notion of scope is explicit there.

-- functionToBodyStatements :: AsValue s => s -> [Value]
-- functionToBodyStatements t = t ^.. key "body" . key "statements" . values

blockToStatements :: AsValue s => s -> [Value]
blockToStatements t = t ^.. key "statements" . values

-- cFGMatricesToEntryId :: [CFGMatrix] -> Int
-- cFGMatricesToEntryId ((_, i, _):_) = i

concatCFGMatrices :: [CFGMatrix] -> [CFGMatrix] -> [CFGMatrix]
concatCFGMatrices cfg1 cfg2 = undefined
--- look for unfilled next id's in cfg1 and fill them by entry of cfg2


nodeType :: AsValue s => s -> String
nodeType t = unpack $ head $ t ^.. key "nodeType" . _String

src :: AsValue s => s -> String
src t = unpack $ head $ t ^.. key "src" . _String

-- statementToEntryNodeId :: Value -> String
-- statementToEntryNodeId t = src entryStat
--     where
--         entryStat = if nodeType t == "Block" then head $ blockToStatements t
--                        else t


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


functionToStatements :: AsValue s => s -> [Value]
functionToStatements t = t ^.. key "body" . key "statements" . values


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

ifStatementToTrueBody :: AsValue s => s -> Value
ifStatementToTrueBody t = head $ t ^.. key "trueBody"

ifStatementToFalseBody :: AsValue s => s -> Value
ifStatementToFalseBody t = head $ t ^.. key "falseBody"

ifStatementToTrueStatements :: AsValue s => s -> [Value]
ifStatementToTrueStatements t = trueBody ^.. key "statements" . values
    where
        trueBody = ifStatementToTrueBody t

ifStatementToFalseStatements :: AsValue s => s -> [Value]
ifStatementToFalseStatements t = falseBody ^.. key "statements" . values
    where
        falseBody = ifStatementToFalseBody t

isIfStatementWithoutFalseBody :: AsValue s => s -> Bool
isIfStatementWithoutFalseBody t = null $ t ^.. filtered (has (key "falseBody"))

blockToNodeId :: AsValue s => s -> String
blockToNodeId t = unpack $ head $ t ^.. key "statements" . values . key "src" . _String

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
        vs = map variableDeclarationToName $ contractToStateVariableDeclarations t
        fs = map functionDefinitionToName $ contractToFunctionDefinitions t
        ss = map structDefinitionToName $ contractToStructDefinitions t
        es = map enumDefinitionToName $ contractToEnumDefinitions t
        errs = map errorDefinitionToName $ contractToErrorDefinitions t
        evs = map eventDefinitionToName $ contractToEventDefinitions t

variableDeclarationStatementToDeclaredVariable :: AsValue s => s -> Variable
variableDeclarationStatementToDeclaredVariable stat = (name, typeDescription, src)
    where
        name = unpack $ head $ stat ^.. key "declarations" . values . key "name" . _String
        typeDescription = unpack $ head $ stat ^.. key "declarations" . values . key "typeDescriptions" . key "typeString" . _String
        src = unpack $ head $ stat ^.. key "declarations" . values . key "src" . _String

variableDeclarationStatementToInitialValue :: AsValue s => s -> Maybe Value
variableDeclarationStatementToInitialValue stat = v
    where
        v = listToMaybe $ stat ^.. key "initialValue"

-- identifierExpressionToVariable :: AsValue s => s -> Variable
-- identifierExpressionToVariable stat = (name, typeDescription, src)
--     where
--         name = unpack $ head $ stat ^.. key "name" . _String
--         typeDescription = unpack $ head $ stat ^.. key "typeDescriptions" . _String
--         src = unpack $ head $ stat ^.. key "src" . _String

expressionInAssignmentFormToLeftExpression :: AsValue s => s -> Value
expressionInAssignmentFormToLeftExpression stat = head $ stat ^.. key "leftHandSide"

expressionInAssignmentFormToRightExpression :: AsValue s => s -> Value
expressionInAssignmentFormToRightExpression stat = head $ stat ^.. key "rightHandSide"


-- Duplicate elements should be removed
expressionToFreeVariables :: AsValue s => s -> [Variable]
expressionToFreeVariables stat
 | nt == "Identifier" = [expressionInIdentifierFormToVariable stat]
 | nt == "UnaryOperation" = expressionToFreeVariables (expressionInUnaryOperationFormToSubExpression stat)
 | nt == "BinaryOperation" = concatMap expressionToFreeVariables (expressionInBinaryOperationFormToExpressions stat)
 | nt == "IndexAccess" = let be = expressionInIndexAccessFormToBaseExpression stat
                             ie = expressionInIndexAccessFormToIndexExpression stat
                            in expressionToFreeVariables be ++ expressionToFreeVariables ie
 | nt == "MemberAccess" = maybeToList $ expressionInMemberAccessFormToVariable stat
 | nt == "Literal" = []
    where
        nt = nodeType stat

expressionToFreeVariableNames :: AsValue s => s -> [VarName]
expressionToFreeVariableNames t = nub $ expressionToFreeVariableNamesAux t

expressionToFreeVariableNamesAux :: AsValue s => s -> [VarName]
expressionToFreeVariableNamesAux stat
 | nt == "Identifier" = [expressionInIdentifierFormToVariableName stat]
 | nt == "UnaryOperation" = expressionToFreeVariableNamesAux (expressionInUnaryOperationFormToSubExpression stat)
 | nt == "BinaryOperation" = concatMap expressionToFreeVariableNamesAux (expressionInBinaryOperationFormToExpressions stat)
 | nt == "IndexAccess" = let be = expressionInIndexAccessFormToBaseExpression stat
                             ie = expressionInIndexAccessFormToIndexExpression stat
                            in expressionToFreeVariableNamesAux be ++ expressionToFreeVariableNamesAux ie
 | nt == "MemberAccess" = [expressionInMemberAccessFormToObjectName stat]
 | nt == "Literal" = []
 | nt == "FunctionCall" = concatMap expressionToFreeVariableNamesAux $ expressionInFunctionCallFormToArguments stat
    where
        nt = nodeType stat

expressionToPointableObjectName :: AsValue s => s -> Maybe String
expressionToPointableObjectName t
 | nt == "Identifier" = Just $ expressionInIdentifierFormToVariableName t
 | nt == "UnaryOperation" = Nothing
 | nt == "BinaryOperation" = Nothing
 | nt == "IndexAccess" = expressionToPointableObjectName $ expressionInIndexAccessFormToBaseExpression t
 | nt == "MemberAccess" = expressionToPointableObjectName $ expressionInMemberAccessFormToObjectName t
 | nt == "Literal" = Nothing
    where
        nt = nodeType t


expressionInMemberAccessFormToMemberName :: AsValue s => s -> String
expressionInMemberAccessFormToMemberName stat = unpack $ head $ stat ^.. key "memberName" . _String

expressionInMemberAccessFormToVariable :: AsValue p => p -> Maybe Variable
expressionInMemberAccessFormToVariable stat = if isEnum then Nothing else Just (name, typeDescription, src)
    where
        isEnum = False
        name = unpack $ head $ stat ^.. key "expression" . key "name" . _String
        typeDescription = unpack $ head $ stat ^.. key "expression" . key "typeDescriptions" . key "typeString" . _String
        src = unpack $ head $ stat ^.. key "expression" . key "src" . _String

expressionInMemberAccessFormToObjectName :: AsValue p => p -> String
expressionInMemberAccessFormToObjectName stat = unpack $ head $ stat ^.. key "expression" . key "name" . _String

expressionInIndexAccessFormToBaseExpression :: AsValue s => s -> Value
expressionInIndexAccessFormToBaseExpression stat = head $ stat ^.. key "baseExpression"

expressionInIndexAccessFormToIndexExpression :: AsValue s => s -> Value
expressionInIndexAccessFormToIndexExpression stat = head $ stat ^.. key "indexExpression"

expressionInIdentifierFormToVariable :: AsValue s => s -> Variable
expressionInIdentifierFormToVariable stat =
    let name = unpack $ head $ stat ^.. key "name" . _String
        typeDescription = unpack $ head $ stat ^.. key "typeDescriptions" . key "typeString" . _String
        src = unpack $ head $ stat ^.. key "src" . _String
     in (name, typeDescription, src)

expressionInIdentifierFormToVariableName :: AsValue s => s -> VarName
expressionInIdentifierFormToVariableName stat = unpack $ head $ stat ^.. key "name" . _String

expressionInBinaryOperationFormToExpressions :: AsValue s => s -> [Value]
expressionInBinaryOperationFormToExpressions stat = lhs ++ rhs
    where
        lhs = stat ^.. key "leftExpression"
        rhs = stat ^.. key "rightExpression"

expressionInUnaryOperationFormToSubExpression :: AsValue s => s -> Value
expressionInUnaryOperationFormToSubExpression stat = head $ stat ^.. key "subExpression"

expressionInUnaryOperationFormToOperator :: AsValue s => s -> String
expressionInUnaryOperationFormToOperator stat = unpack $ head $ stat ^.. key "operator" . _String

functionDefinitionToBodyStatements :: AsValue s => s -> [Value]
functionDefinitionToBodyStatements t = t ^.. key "body" . key "statements" . values

functionDefinitionToBody :: AsValue s => s -> [Value]
functionDefinitionToBody t = t ^.. key "body"

functionDefinitionToParameterVariables :: AsValue s => s -> [Variable]
functionDefinitionToParameterVariables stat = map (\v ->
    let name = unpack $ head $ v ^.. key "name" . _String
        typeDescription = unpack $ head $ v ^.. key "typeDescriptions" . key "typeString" . _String
        src = unpack $ head $ v ^.. key "src" . _String
     in (name, typeDescription, src))
    parameters
    where
        parameters = stat ^.. key "parameters" . key "parameters" . values

functionDefinitionToReturnVariables :: AsValue s => s -> [Variable]
functionDefinitionToReturnVariables stat = declaredVariablesWithName
    where
        parameters = stat ^.. key "returnParameters" . key "parameters" . values  . filtered (has (key "nodeType" . _String . only "VariableDeclaration"))
        declaredVariables = map (\v ->
            let name = unpack $ head $ v ^.. key "name" . _String
                typeDescription = unpack $ head $ v ^.. key "typeDescriptions" . key "typeString" . _String
                src = unpack $ head $ v ^.. key "src" . _String
             in (name, typeDescription, src))
            parameters
        declaredVariablesWithName = filter (\(n, _, _) -> n /= "") declaredVariables

functionDefinitionToReturnVariableTypes :: AsValue s => s -> [String]
functionDefinitionToReturnVariableTypes stat = declaredVariables
    where
        parameters = stat ^.. key "returnParameters" . key "parameters" . values  . filtered (has (key "nodeType" . _String . only "VariableDeclaration"))
        declaredVariables = map (\v ->
            unpack $ head $ v ^.. key "typeDescriptions" . key "typeString" . _String)
            parameters

-- NOTE: Variable declaration should be distinguished from variable use

expressionStatementToExpression :: AsValue s => s -> Value
expressionStatementToExpression stat = head $ stat ^.. key "expression"

statementInWhileFormToConditionalExpression :: AsValue s => s -> Value
statementInWhileFormToConditionalExpression stat = head $ stat ^.. key "condition"

statementInReturnFormToReturnExpression :: AsValue s => s -> Maybe Value
statementInReturnFormToReturnExpression stat = stat ^? key "expression"

statementInEmitFormToEventCall :: AsValue s => s -> Value
statementInEmitFormToEventCall stat = head $ stat ^.. key "eventCall"

expressionInAssignmentFormToOperator :: AsValue s => s -> String
expressionInAssignmentFormToOperator stat =unpack $ head $ stat ^.. key "operator" . _String

-- An arithmetic operator is an operator such as +=, -=, where the left hand side of the assignemnt
-- contributes to the assignment result by its own value.
-- In this function, the argument is assumed to be some assignment operator, hence everything except
-- "=" is an arithmetic assignment.
isArithmeticAssignmentOperator :: String -> Bool
isArithmeticAssignmentOperator = (/= "=")

isIncrementOrDecrementOperator :: String -> Bool
isIncrementOrDecrementOperator op = (op == "++") || (op == "--")

expressionInFunctionCallFormToArguments :: AsValue s => s -> [Value]
expressionInFunctionCallFormToArguments stat = stat ^.. key "arguments" . values

variableToVarName :: Variable -> VarName
variableToVarName (n, t, s) = n

isPointerTypeString :: String -> Bool
isPointerTypeString s = last (words s) == "pointer"
-- eg. ("typeString",String "struct Example.Order storage pointer")