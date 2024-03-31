module DefinitionUseAnalysis where

import Data.Aeson
import Data.Aeson.Lens
import SolidityFeature
import CFG
import Data.Maybe
import Data.List
import Debug.Trace


type LVTable = [(String, [Variable], [Variable])] -- A row consists of the src location, LVentry, and LVexit.
type DUTable = [(String, [String], [Variable], [Variable])]

--------------------------------------------
----  Read Variables (gen)

----- for statement

statementAndEnvironmentToReadVariables :: AsValue s => s -> Environment -> [Variable]
statementAndEnvironmentToReadVariables stat env
 | nt == "VariableDeclarationStatement" = variableDeclarationStatementAndEnvironmentToReadVariables stat env
 | nt == "ExpressionStatement" = expressionAndEnvironmentToReadVariables (expressionStatementToExpression stat) env
 | nt == "WhileStatement" = let varNames = expressionToFreeVariableNames $ statementInWhileFormToConditionalExpression stat
                                mvars = map (\vn -> varNameAndEnvironmentToVariable vn env) varNames
                              in catMaybes mvars
 | nt == "Return" = let varNames = concatMap expressionToFreeVariableNames $ maybeToList $ statementInReturnFormToReturnExpression stat
                        vars = catMaybes $ map (\vn -> varNameAndEnvironmentToVariable vn env) varNames
                     in vars
 | nt == "EmitStatement" = emitStatementAndEnvironmentToReadVariables stat env
    where
        nt = nodeType stat

variableDeclarationStatementAndEnvironmentToReadVariables :: AsValue s => s -> Environment -> [Variable]
variableDeclarationStatementAndEnvironmentToReadVariables stat env = vars
    where
        mInitValue = variableDeclarationStatementToInitialValue stat
        varNames = maybe [] expressionToFreeVariableNames mInitValue
        vars = catMaybes $ map (\vn -> varNameAndEnvironmentToVariable vn env) varNames

emitStatementAndEnvironmentToReadVariables :: AsValue s => s -> Environment -> [Variable]
emitStatementAndEnvironmentToReadVariables stat =
    expressionInFunctionCallFormAndEnvironmentToReadVariables (statementInEmitFormToEventCall stat)

----- for expression

expressionAndEnvironmentToReadVariables :: AsValue s => s -> Environment -> [Variable]
expressionAndEnvironmentToReadVariables stat env
 | nt == "Identifier" = maybeToList $ varNameAndEnvironmentToVariable (expressionInIdentifierFormToVariableName stat) env
 | nt == "UnaryOperation" = let varNames = expressionToFreeVariableNames $ expressionInUnaryOperationFormToSubExpression stat
                                in catMaybes $ map (\vn -> varNameAndEnvironmentToVariable vn env) varNames
 | nt == "BinaryOperation" = let varNames = concatMap expressionToFreeVariableNames (expressionInBinaryOperationFormToExpressions stat)
                                in catMaybes $ map (\vn -> varNameAndEnvironmentToVariable vn env) varNames
 | nt == "IndexAccess" = let be = expressionInIndexAccessFormToBaseExpression stat
                             ie = expressionInIndexAccessFormToIndexExpression stat
                             varNames = expressionToFreeVariableNames be ++ expressionToFreeVariableNames ie
                            in catMaybes $ map (\vn -> varNameAndEnvironmentToVariable vn env) varNames
 | nt == "MemberAccess" = let varName = expressionInMemberAccessFormToObjectName stat
                            in maybeToList $ varNameAndEnvironmentToVariable varName env
 | nt == "Literal" = []
 | nt == "Assignment" = expressionInAssignmentFormAndEnvironmentToReadVariables stat env
 | nt == "FunctionCall" = expressionInFunctionCallFormAndEnvironmentToReadVariables stat env
    where
        nt = nodeType stat

expressionInAssignmentFormAndEnvironmentToReadVariables :: AsValue s => s -> Environment -> [Variable]
expressionInAssignmentFormAndEnvironmentToReadVariables stat env
 | lftNodeType == "Identifier" = rhtFvars ++ lftReadVars
 | lftNodeType == "MemberAccess" =
    let mLftVar = expressionInMemberAccessFormToVariable lftExpr
      in case mLftVar of Nothing -> lftReadVars ++ rhtFvars ++ lftReadVars
                         Just (lftVarName, lftVarType, lftVarSrc) ->
                            let x = do lftVar <- varNameAndEnvironmentToVariable lftVarName env
                                       pointedVar <- pointerVariableAndEnvironmentToPointedVariable lftVar env
                                       return lftVar
                             in maybeToList x
        -- should exactly count parameters occurring in the left expression!
        -- should not contain non-variables! (currently, enum objects are included)
     --in lftFvars ++ rhtFvars ++ lftReadVars
 | lftNodeType == "IndexAccess" =
    let idxExpr = expressionInIndexAccessFormToIndexExpression lftExpr
        idxFvarNames = expressionToFreeVariableNames idxExpr
        idxFvars = catMaybes $ map (\vn -> varNameAndEnvironmentToVariable vn env) idxFvarNames
     in idxFvars ++ rhtFvars ++ lftReadVars
    where
        lftExpr = expressionInAssignmentFormToLeftExpression stat
        rhtExpr = expressionInAssignmentFormToRightExpression stat
        lftFvarNames = expressionToFreeVariableNames lftExpr
        lftFvars = catMaybes $ map (\vn -> varNameAndEnvironmentToVariable vn env) lftFvarNames
        rhtFvarNames = expressionToFreeVariableNames rhtExpr
        rhtFvars = catMaybes $ map (\vn -> varNameAndEnvironmentToVariable vn env) rhtFvarNames
        lftNodeType = nodeType lftExpr
        operator = expressionInAssignmentFormToOperator stat
        lftReadVars = if isArithmeticAssignmentOperator operator then lftFvars else []

-- member access
-- obj.member
-- objArr[i].member
-- objArr[i].members[j].belonging
-- objArr.member.belongings[i]

-- tuple
-- (bool s, ) = a.call{ value: 0.01 ether }("");

expressionInFunctionCallFormAndEnvironmentToReadVariables :: AsValue s => s -> Environment -> [Variable]
expressionInFunctionCallFormAndEnvironmentToReadVariables stat env = vars
    where
        argExprs = expressionInFunctionCallFormToArguments stat
        varNames = concatMap expressionToFreeVariableNames argExprs
        vars = catMaybes $ map (\vn -> varNameAndEnvironmentToVariable vn env) varNames


--------------------------------------------
----  Updated Variables (kill)

----- for statement

statementAndEnvironmentToUpdatedVariableNames :: AsValue s => s -> Environment -> [VarName]
statementAndEnvironmentToUpdatedVariableNames stat env
 | nt == "VariableDeclarationStatement" = variableDeclarationStatementAndEnvironmentToUpdatedVariableNames stat env
 | nt == "ExpressionStatement" = expressionAndEnvironmentToUpdatedVariableNames (expressionStatementToExpression stat) env
 | nt == "WhileStatement" = expressionAndEnvironmentToUpdatedVariableNames (statementInWhileFormToConditionalExpression stat) env
 | nt == "Return" = undefined
    where
        nt = nodeType stat

statementAndEnvironmentToUpdatedVariables :: AsValue s => s -> Environment -> [Variable]
statementAndEnvironmentToUpdatedVariables stat env
 | nt == "VariableDeclarationStatement" = variableDeclarationStatementAndEnvironmentToUpdatedVariables stat env
 | nt == "ExpressionStatement" = expressionAndEnvironmentToUpdatedVariables (expressionStatementToExpression stat) env
 | nt == "WhileStatement" = expressionAndEnvironmentToUpdatedVariables (statementInWhileFormToConditionalExpression stat) env
 | nt == "Return" = case statementInReturnFormToReturnExpression stat of Nothing -> []
                                                                         Just expr -> expressionAndEnvironmentToUpdatedVariables expr env
 | nt == "EmitStatement" = emitStatementAndEnvironmentToUpdatedVariables stat env
    where
        nt = nodeType stat

emitStatementAndEnvironmentToUpdatedVariables :: AsValue s => s -> Environment -> [Variable]
emitStatementAndEnvironmentToUpdatedVariables stat =
    expressionInFunctionCallFormAndEnvironmentToUpdatedVariables (statementInEmitFormToEventCall stat)

variableDeclarationStatementAndEnvironmentToUpdatedVariableNames :: AsValue s => s -> Environment -> [VarName]
variableDeclarationStatementAndEnvironmentToUpdatedVariableNames stat env =
    varName:varNames
    where
        expr = maybeToList $ variableDeclarationStatementToInitialValue stat
        (varName, _, _) = variableDeclarationStatementToDeclaredVariable stat
        varNames = concatMap (\e -> expressionAndEnvironmentToUpdatedVariableNames e env) expr

variableDeclarationStatementAndEnvironmentToUpdatedVariables :: AsValue s => s -> Environment -> [Variable]
variableDeclarationStatementAndEnvironmentToUpdatedVariables stat env =
    var:vars
    where
        expr = maybeToList $ variableDeclarationStatementToInitialValue stat
        var = variableDeclarationStatementToDeclaredVariable stat
        vars = concatMap (\e -> expressionAndEnvironmentToUpdatedVariables e env) expr

----- for expression

expressionAndEnvironmentToUpdatedVariableNames :: AsValue s => s -> Environment -> [VarName]
expressionAndEnvironmentToUpdatedVariableNames stat env
 | nt == "Identifier" = []
 | nt == "UnaryOperation" = let subExpr = expressionInUnaryOperationFormToSubExpression stat
                                op = expressionInUnaryOperationFormToOperator stat
                              in [expressionInIdentifierFormToVariableName subExpr | isIncrementOrDecrementOperator op]
 | nt == "BinaryOperation" = concatMap (\e -> expressionAndEnvironmentToUpdatedVariableNames e env) (expressionInBinaryOperationFormToExpressions stat)
 | nt == "IndexAccess" = let be = expressionInIndexAccessFormToBaseExpression stat
                             ie = expressionInIndexAccessFormToIndexExpression stat
                            in expressionAndEnvironmentToUpdatedVariableNames be env ++ expressionAndEnvironmentToUpdatedVariableNames ie env
 | nt == "MemberAccess" = [] --maybeToList $ expressionInMemberAccessFormToVariable stat
 | nt == "Literal" = []
 | nt == "Assignment" = expressionInAssignmentFormAndEnvironmentToUpdatedVariableNames stat env
 | nt == "FunctionCall" = expressionInFunctionCallFormAndEnvironmentToUpdatedVariableNames stat env
    where
        nt = nodeType stat

expressionAndEnvironmentToUpdatedVariables :: AsValue s => s -> Environment -> [Variable]
expressionAndEnvironmentToUpdatedVariables stat env
 | nt == "Identifier" = []
 | nt == "UnaryOperation" = let subExpr = expressionInUnaryOperationFormToSubExpression stat
                                op = expressionInUnaryOperationFormToOperator stat
                              in [expressionInIdentifierFormToVariable subExpr | isIncrementOrDecrementOperator op]
 | nt == "BinaryOperation" = concatMap (\e -> expressionAndEnvironmentToUpdatedVariables e env) (expressionInBinaryOperationFormToExpressions stat)
 | nt == "IndexAccess" = let be = expressionInIndexAccessFormToBaseExpression stat
                             ie = expressionInIndexAccessFormToIndexExpression stat
                            in expressionAndEnvironmentToUpdatedVariables be env ++ expressionAndEnvironmentToUpdatedVariables ie env
 | nt == "MemberAccess" = [] --maybeToList $ expressionInMemberAccessFormToVariable stat
 | nt == "Literal" = []
 | nt == "Assignment" = expressionInAssignmentFormAndEnvironmentToUpdatedVariables stat env
 | nt == "FunctionCall" = expressionInFunctionCallFormAndEnvironmentToUpdatedVariables stat env
    where
        nt = nodeType stat

expressionInFunctionCallFormAndEnvironmentToUpdatedVariableNames :: AsValue s => s -> Environment -> [VarName]
expressionInFunctionCallFormAndEnvironmentToUpdatedVariableNames stat env =
    concatMap (\e -> expressionAndEnvironmentToUpdatedVariableNames e env) argExprs
    where
        argExprs = expressionInFunctionCallFormToArguments stat

expressionInFunctionCallFormAndEnvironmentToUpdatedVariables :: AsValue s => s -> Environment -> [Variable]
expressionInFunctionCallFormAndEnvironmentToUpdatedVariables stat env =
    concatMap (\e -> expressionAndEnvironmentToUpdatedVariables e env) argExprs
    where
        argExprs = expressionInFunctionCallFormToArguments stat

-- generally several variables (eg. tuple unpacking)
expressionInAssignmentFormAndEnvironmentToUpdatedVariableNames :: AsValue s => s -> Environment -> [VarName]
expressionInAssignmentFormAndEnvironmentToUpdatedVariableNames stat env
 | nt == "Identifier" = [expressionInIdentifierFormToVariableName leftExpr]
 | nt == "MemberAccess" = [expressionInMemberAccessFormToObjectName leftExpr]
 | nt == "IndexAccess" =
    let baseExpr = expressionInIndexAccessFormToBaseExpression leftExpr
      in expressionToFreeVariableNames baseExpr
    where
        leftExpr = expressionInAssignmentFormToLeftExpression stat
        nt = nodeType leftExpr

expressionInAssignmentFormAndEnvironmentToUpdatedVariables :: AsValue s => s -> Environment -> [Variable]
expressionInAssignmentFormAndEnvironmentToUpdatedVariables stat env
 | nt == "Identifier" = [expressionInIdentifierFormToVariable leftExpr]
 | nt == "MemberAccess" = let objName = expressionInMemberAccessFormToObjectName leftExpr
                              mVar = varNameAndEnvironmentToVariable objName env
                              ptrs = environmentToStoragePointers env
                            in case mVar of Nothing -> []
                                            Just var -> case pointerVariableAndEnvironmentToPointedVariable var env
                                                of Nothing -> [var]
                                                   Just pointee -> [pointee]
 | nt == "IndexAccess" =
    let baseExpr = expressionInIndexAccessFormToBaseExpression leftExpr
      in expressionToFreeVariables baseExpr
    where
        leftExpr = expressionInAssignmentFormToLeftExpression stat
        nt = nodeType leftExpr

--------------------------------------------------
---- definition-use analysis

cFGEdgesToDefinitionUseTable :: CFGEdges -> DUTable
cFGEdgesToDefinitionUseTable [] = []
cFGEdgesToDefinitionUseTable (((stat, env), nodeId, nextNodeIds):cFGEdges)
 | length nextNodeIds > 1 = undefined -- no branching supported yet
 | otherwise = (nodeId, nextNodeIds, updatedVariables, readVariables):restTable
    where
        updatedVariables = statementAndEnvironmentToUpdatedVariables stat env
        readVariables = statementAndEnvironmentToReadVariables stat env
        restTable = cFGEdgesToDefinitionUseTable cFGEdges

cFGEdgesToUnreadVariables :: CFGEdges -> [Variable]
cFGEdgesToUnreadVariables [] = []
cFGEdgesToUnreadVariables cFGEdges = localUnreadVars
    where
        definitionUseTable = cFGEdgesToDefinitionUseTable cFGEdges
        ((stat, env), nodeId, nextNodeIds) = head cFGEdges
        parameterVars = [ v | VarDef 0 v <- env]
        initialTable = ("", [nodeId], parameterVars, []):definitionUseTable
        unreadVars = definitionUseTableToUnreadVariables initialTable
        nonLocalVars = [ v | VarDef i v <- env, i > 0 ]
        localUnreadVars = [ var | var <- unreadVars, var `notElem` nonLocalVars]

definitionUseTableToUnreadVariables :: DUTable -> [Variable]
definitionUseTableToUnreadVariables [] = []
definitionUseTableToUnreadVariables ((nodeId, nextNodeIds, updatedVars, readVars):definitionUseTable) =
    unreadVars ++ definitionUseTableToUnreadVariables definitionUseTable
    where
        readVars = concatMap (definitionUseTableAndNodeIdToReadVariables definitionUseTable) nextNodeIds
        unreadVars = [ var | var <- updatedVars, var `notElem` readVars ]

definitionUseTableAndNodeIdToReadVariables :: DUTable -> String -> [Variable]
definitionUseTableAndNodeIdToReadVariables definitionUseTable nodeId =
    case info of [] -> []
                 ((rvs, nextNodeIds):_) ->
                    rvs ++ concatMap (definitionUseTableAndNodeIdToReadVariables definitionUseTable) nextNodeIds
    where
        info = [ (rvs, nextNodeIds) | (nid, nextNodeIds, _, rvs) <- definitionUseTable, nid == nodeId ]



-- --------------------------------------------------
-- ----  Live variables analysis table

-- cFGEdgesToLiveVariablesAnalysisTable :: CFGEdges -> LVTable
-- cFGEdgesToLiveVariablesAnalysisTable cFGEdges = cFGEdgesToLiveVariablesAnalysisTableAux initTable cFGEdges
--     where
--         initTable = [ (nodeId, [], []) | (_, nodeId, _) <- cFGEdges ]

-- cFGEdgesToLiveVariablesAnalysisTableAux :: LVTable -> CFGEdges -> LVTable
-- cFGEdgesToLiveVariablesAnalysisTableAux oldTable cFGEdges =
--     if newTable == oldTable then newTable else cFGEdgesToLiveVariablesAnalysisTableAux newTable cFGEdges
--     where
--         newTable = [ (nodeId, cFGEdgesAndNodeIdToLVEntry cFGEdges nodeId, cFGEdgesAndNodeIdToLVExit cFGEdges nodeId) | (_, nodeId, _) <- cFGEdges ]

-- cFGEdgesToOneStepLiveVariablesAnalysisTable :: CFGEdges -> LVTable
-- cFGEdgesToOneStepLiveVariablesAnalysisTable cFGEdges =
--     [(nodeId, cFGEdgesAndNodeIdToLVEntry cFGEdges nodeId, cFGEdgesAndNodeIdToLVExit cFGEdges nodeId) | nodeId <- nodeIds]
--     -- [(nodeId, cFGEdgesAndNodeIdToLVEntry cFGEdges nodeId, cFGEdgesAndNodeIdToLVExit cFGEdges nodeId) | nodeId <- nodeIds]
--     where
--         nodeIds = cFGEdgesToNodeIds cFGEdges

-- cFGEdgesAndNodeIdToLVEntry :: CFGEdges -> String -> [Variable]
-- cFGEdgesAndNodeIdToLVEntry cFGEdges nodeId = nub $ [ v | v <- lVexit, v `notElem` updatedVariables] ++ readVariables
--     where
--         infos = [ (stat, env) | ((stat, env), nodeId', nextNodeIds) <- cFGEdges, nodeId == nodeId' ]
--         (stat, env) = head infos
--         updatedVariableNames = statementAndEnvironmentToUpdatedVariableNames stat env
--         updatedVariables = catMaybes $ map (\n -> varNameAndEnvironmentToVariable n env) updatedVariableNames
--         readVariables = statementAndEnvironmentToReadVariables stat env
--         -- readVariables = catMaybes $ map (\n -> varNameAndEnvironmentToVariable n env) readVariableNames
--         lVexit = cFGEdgesAndNodeIdToLVExit cFGEdges nodeId
--         -- node = head $ [ ((stat, env), nodeId, nextNodes) | ((stat, env), nodeId, nextNodes) <- cFGEdges]
--         isEntryNode =  [ nodeId' | (_, nodeId', _) <- cFGEdgesToEntryNodes cFGEdges, nodeId == nodeId']
--         -- parameterVariables = [ v | VarDef 0 v <- env ]
--         -- lVEntry = [ v | v <- lVexit, v `notElem` updatedVariables] ++ readVariables ++ if isEntryNode then

-- cFGEdgesAndNodeIdToLVExit :: CFGEdges -> String -> [Variable]
-- cFGEdgesAndNodeIdToLVExit cFGEdges nodeId
--  | isFinalNode = []
-- --  | isFinalNode = nub $ concatMap (cFGEdgesAndNodeIdToLVEntry cFGEdges) prevNodeIds
--  | otherwise = nub $ concatMap (cFGEdgesAndNodeIdToLVEntry cFGEdges) nextNodeIds
-- --  | otherwise = let ((v, env), nodeId, _) = head finNodes
-- --                    env' = removeLocalVariablesInEnvironment env
-- --                 in [ v' | VarDef i v' <- env']
--     where
--         -- finNodes = traceShowId $ cFGEdgesToFinalNodes cFGEdges
--         isFinalNode = not $ null [ nodeId' | (info, nodeId', nextNodeIds) <- cFGEdgesToFinalNodes cFGEdges, nodeId' == nodeId]
--         prevNodeIds = [ nodeId' | ( _, nodeId', nextNodes) <- cFGEdges, nodeId `elem` nextNodes ]
--         nextNodeIds = head [ nextNodeIds | ( _, nodeId', nextNodeIds) <- cFGEdges, nodeId == nodeId' ]

