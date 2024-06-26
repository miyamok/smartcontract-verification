{-# LANGUAGE OverloadedStrings #-}
module SolidityFeature where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Key ( fromString )
import Data.Text ( Text, pack, unpack )
import Data.List
import Data.Foldable (for_)
import Data.Maybe (maybeToList, listToMaybe)
import Debug.Trace

type CFGMatrix = (Value, Int, [Int])
type VarName = String
type Variable = (VarName, String, String) -- name, type, and src (pointing the declaration statement, but no nameLocation!)

type UnaryOperator = (Bool, String) -- isPrefix and operator in string

-- Functions processing a Solidity file

fileToAbsolutePath :: AsValue s => s -> Maybe String
fileToAbsolutePath t = fmap unpack $ t ^? key "absolutePath" . _String

fileToContracts :: AsValue s => s -> [Value]
fileToContracts t = t ^.. key "nodes" . values . filtered (has (key "nodeType" . _String . only "ContractDefinition"))

-- Functions processing a contract

contractToContractName :: AsValue s => s -> String
contractToContractName t = unpack $ head $ t ^.. key "name" . _String

contractToFunctionDefinitions :: AsValue s => s -> [Value]
contractToFunctionDefinitions t = t ^.. key "nodes" . values . filtered (has (key "nodeType"._String.only "FunctionDefinition"))

contractToStateVariableDeclarations :: AsValue s => s -> [Value]
contractToStateVariableDeclarations = contractToDefinitionsOrDeclarations "VariableDeclaration"

contractToDefinitionsOrDeclarations :: AsValue s => Text -> s -> [Value]
contractToDefinitionsOrDeclarations s t = t ^.. key "nodes" . values . filtered (has (key "nodeType"._String.only s))

contractToEventDefinitions :: AsValue s => s -> [Value]
contractToEventDefinitions = contractToDefinitionsOrDeclarations "EventDefinition"

contractToStructDefinitions :: AsValue s => s -> [Value]
contractToStructDefinitions = contractToDefinitionsOrDeclarations "StructDefinition"

contractToEnumDefinitions :: AsValue s => s -> [Value]
contractToEnumDefinitions = contractToDefinitionsOrDeclarations "EnumDefinition"

contractToErrorDefinitions :: AsValue s => s -> [Value]
contractToErrorDefinitions = contractToDefinitionsOrDeclarations "ErrorDefinition"

-- Functions processing a function definition

functionDefinitionToName :: AsValue s => s -> String
functionDefinitionToName t = unpack $ head $ t ^.. key "name" . _String

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

-- Functions processing a definition/declaration
-- struct-definition, event-definition, enum-definition, constant-variable-declaration, error-definition

definitionOrDeclarationToName :: AsValue s => s -> String
definitionOrDeclarationToName t = unpack . head $ t ^.. key "name" . _String

definitionOrDeclarationToType :: AsValue s => s -> String
definitionOrDeclarationToType t = unpack . head $ t ^..  key "typeDescriptions" . key "typeString" . _String

-- Functions processing an event definition

eventDefinitionToName :: AsValue s => s -> String
eventDefinitionToName = definitionOrDeclarationToName

-- Functions processing a struct definition

structDefinitionToName :: AsValue s => s -> String
structDefinitionToName = definitionOrDeclarationToName

-- Functions processing an enum definition

enumDefinitionToName :: AsValue s => s -> String
enumDefinitionToName = definitionOrDeclarationToName

-- Functions processing an error definition

errorDefinitionToName :: AsValue s => s -> String
errorDefinitionToName = definitionOrDeclarationToName

-- Functions processing a variable declaration

variableDeclarationToName :: AsValue s => s -> String
variableDeclarationToName = definitionOrDeclarationToName

variableDeclarationToType :: AsValue s => s -> String
variableDeclarationToType = definitionOrDeclarationToType

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

blockToStatements :: AsValue s => s -> [Value]
blockToStatements t = t ^.. key "statements" . values

concatCFGMatrices :: [CFGMatrix] -> [CFGMatrix] -> [CFGMatrix]
concatCFGMatrices cfg1 cfg2 = undefined
--- look for unfilled next id's in cfg1 and fill them by entry of cfg2


nodeType :: AsValue s => s -> String
nodeType t = unpack $ head $ t ^.. key "nodeType" . _String

src :: AsValue s => s -> String
src t = unpack $ head $ t ^.. key "src" . _String

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

ifStatementToConditionExpression :: AsValue s => s -> Value
ifStatementToConditionExpression t = head $ t ^.. key "condition"

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

printProfile :: AsValue s => s -> IO ()
printProfile t = do for_ (fileToContracts t) printContractProfile

printContractProfile :: AsValue s => s -> IO ()
printContractProfile t = do putStrLn $ "Contract name: " ++ cName
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

showVariable :: Variable -> String
showVariable (n, t, s) = "(" ++ intercalate "," [n, t, s] ++ ")"

showContractProfile :: AsValue s => s -> String
showContractProfile t =
    intercalate "\n" ["Contract name: " ++ cName, "Declared variable names: " ++ show vs, "Defined function names: " ++ show fs,
                        "Defined enum names: " ++ show es, "Defined struct names: " ++ show ss, "Defined event names: " ++ show es,
                        "Defined error names: " ++ show errs]
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
expressionToFreeVariableNamesAux t
 | nt == "Identifier" = [expressionInIdentifierFormToVariableName t]
 | nt == "UnaryOperation" = expressionToFreeVariableNamesAux (expressionInUnaryOperationFormToSubExpression t)
 | nt == "BinaryOperation" = concatMap expressionToFreeVariableNamesAux (expressionInBinaryOperationFormToExpressions t)
 | nt == "IndexAccess" = let be = expressionInIndexAccessFormToBaseExpression t
                             ie = expressionInIndexAccessFormToIndexExpression t
                            in expressionToFreeVariableNamesAux be ++ expressionToFreeVariableNamesAux ie
 | nt == "MemberAccess" = [expressionInMemberAccessFormToObjectName t]
 | nt == "Literal" = []
 | nt == "FunctionCall" = concatMap expressionToFreeVariableNamesAux $ expressionInFunctionCallFormToArguments t
 | nt == "TupleExpression" = concatMap expressionToFreeVariableNamesAux $ expressionInTupleFormToComponents t
 | nt == "Conditional" = nub $ concatMap expressionToFreeVariableNamesAux (expressionInConditionalFormToSubExpressions t)
    where
        nt = nodeType t

expressionInConditionalFormToCondition :: AsValue s => s -> Value
expressionInConditionalFormToCondition t = head $ t ^.. key "condition"

expressionInConditionalFormToTrueExpression :: AsValue s => s -> Value
expressionInConditionalFormToTrueExpression t = head $ t ^.. key "trueExpression"

expressionInConditionalFormToFalseExpression :: AsValue s => s -> Value
expressionInConditionalFormToFalseExpression t = head $ t ^.. key "falseExpression"

expressionInConditionalFormToSubExpressions :: AsValue s => s -> [Value]
expressionInConditionalFormToSubExpressions t = [cExpr, tExpr, fExpr]
    where
        cExpr = expressionInConditionalFormToCondition t
        tExpr = expressionInConditionalFormToTrueExpression t
        fExpr = expressionInConditionalFormToFalseExpression t

expressionInTupleFormToComponents :: AsValue s => s -> [Value]
expressionInTupleFormToComponents t = t ^.. key "components" . values

expressionToPointableStorageVariableName :: AsValue s => s -> Maybe String
expressionToPointableStorageVariableName t
 | nt == "Identifier" = Just $ expressionInIdentifierFormToVariableName t
 | nt == "UnaryOperation" = Nothing
 | nt == "BinaryOperation" = Nothing
 | nt == "IndexAccess" = expressionToPointableStorageVariableName $ expressionInIndexAccessFormToBaseExpression t
 | nt == "MemberAccess" = expressionToPointableStorageVariableName $ expressionInMemberAccessFormToObjectName t
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
expressionInIdentifierFormToVariableName t = unpack $ head $ t ^.. key "name" . _String

expressionInLiteralFormToValueString :: AsValue s => s -> String
expressionInLiteralFormToValueString t = unpack $ head $ t ^.. key "value" . _String

expressionInBinaryOperationFormToExpressions :: AsValue s => s -> [Value]
expressionInBinaryOperationFormToExpressions stat = lhs ++ rhs
    where
        lhs = stat ^.. key "leftExpression"
        rhs = stat ^.. key "rightExpression"

expressionInUnaryOperationFormToSubExpression :: AsValue s => s -> Value
expressionInUnaryOperationFormToSubExpression stat = head $ stat ^.. key "subExpression"

expressionInUnaryOperationFormToOperator :: AsValue s => s -> UnaryOperator
expressionInUnaryOperationFormToOperator stat = (isPrefix, op)
    where
        isPrefix = head $ stat ^.. key "prefix" . _Bool
        op = unpack $ head $ stat ^.. key "operator" . _String

-- NOTE: Variable declaration should be distinguished from variable use

expressionStatementToExpression :: AsValue s => s -> Value
expressionStatementToExpression stat = head $ stat ^.. key "expression"

statementInWhileFormToConditionalExpression :: AsValue s => s -> Value
statementInWhileFormToConditionalExpression stat = head $ stat ^.. key "condition"

statementInReturnFormToReturnExpression :: AsValue s => s -> Maybe Value
statementInReturnFormToReturnExpression stat = stat ^? key "expression"

statementInEmitFormToEventCall :: AsValue s => s -> Value
statementInEmitFormToEventCall stat = head $ stat ^.. key "eventCall"

statementInEmitFormToEmitExpressions :: AsValue s => s -> [Value]
statementInEmitFormToEmitExpressions stat = argExprs
    where
        functionCallExpr = statementInEmitFormToEventCall stat
        argExprs = expressionInFunctionCallFormToArguments functionCallExpr

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