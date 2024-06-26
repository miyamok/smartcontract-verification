module CFG where

import Data.Aeson
import SolidityFeature
import Data.Aeson.Lens
import Data.Maybe

import Debug.Trace
-- Naming isn't good.
type CFGNode = ((Value, Environment), String, [String])
-- to constract a Data.Graph via Data.Graph.graphFromEdges
type CFGEdges = [CFGNode]

data VarInfo = VarDef Int Variable | StoragePointer Variable Variable deriving (Show, Eq)

-- Renaming?
-- Currently, Environemnt is just a list of available variables and what points what,
-- but it doesn't contain a mapping from an ordinary variable to its value.
type Environment = [VarInfo]

variableToLocalVarDef :: Variable -> VarInfo
variableToLocalVarDef = VarDef 0

varDefToLevel :: VarInfo -> Int
varDefToLevel (VarDef i _) = i
varDefToLevel (StoragePointer _ _) = -1

isStoragePointer :: VarInfo -> Bool
isStoragePointer v = varDefToLevel v == -1

storagePointerToSourceVariable :: VarInfo -> Variable
storagePointerToSourceVariable (StoragePointer src dst) = src

varDefToVariable :: VarInfo -> Variable
varDefToVariable (VarDef _ v) = v

incrementVarDefLevel :: VarInfo -> VarInfo
incrementVarDefLevel (VarDef i v) = VarDef (i+1) v
incrementVarDefLevel (StoragePointer v u) = StoragePointer v u

incrementEnvironmentLevel :: Environment -> Environment
incrementEnvironmentLevel = map incrementVarDefLevel

decrementVarDefLevel :: VarInfo -> VarInfo
decrementVarDefLevel (VarDef i v) = VarDef (i-1) v
decrementVarDefLevel (StoragePointer v u) = StoragePointer v u

decrementEnvironmentLevel :: Environment -> Environment
decrementEnvironmentLevel env = newVarDefs ++ newStoragePointers
    where
        localVars = map varDefToVariable $ filter (\x -> varDefToLevel x == 0) env
        newStoragePointers = filter (\x -> isStoragePointer x && notElem (storagePointerToSourceVariable x) localVars) env
        newVarDefs = filter (\x -> varDefToLevel x > 0) env

updateVarInfoByStoragePointer :: VarInfo -> VarInfo -> VarInfo
updateVarInfoByStoragePointer (StoragePointer newSrc newDst) (StoragePointer src dst)
 | newSrc == src = StoragePointer src newDst
 | otherwise = StoragePointer src dst
updateVarInfoByStoragePointer (StoragePointer newSrc newDst) (VarDef i v) = VarDef i v

updateEnvironmentByStoragePointer :: VarInfo -> Environment -> Environment
updateEnvironmentByStoragePointer vi = map (updateVarInfoByStoragePointer vi)

environmentToVarDefs :: Environment -> Environment
environmentToVarDefs [] = []
environmentToVarDefs (VarDef i v:env) = VarDef i v:environmentToVarDefs env
environmentToVarDefs (_:env) = environmentToVarDefs env

environmentToStoragePointers :: Environment -> Environment
environmentToStoragePointers [] = []
environmentToStoragePointers (StoragePointer src dst:env) = StoragePointer src dst:environmentToStoragePointers env
environmentToStoragePointers (_:env) = environmentToStoragePointers env

lookupVariableInEnvironment :: Variable -> Environment -> [VarInfo]
lookupVariableInEnvironment _ [] = []
lookupVariableInEnvironment var (VarDef i v:env)
 | var == v = VarDef i v:lookupVariableInEnvironment var env
 | otherwise = lookupVariableInEnvironment var env
lookupVariableInEnvironment var (StoragePointer src dst:env)
 | var == src = StoragePointer src dst:lookupVariableInEnvironment var env
 | otherwise = lookupVariableInEnvironment var env

lookupVarNameInEnvironment :: VarName -> Environment -> [VarInfo]
lookupVarNameInEnvironment _ [] = []
lookupVarNameInEnvironment varName (VarDef i v:env)
 | varName == vn = VarDef i v:lookupVarNameInEnvironment varName env
 | otherwise = lookupVarNameInEnvironment varName env
    where (vn, vt, vsrc) = v
lookupVarNameInEnvironment varName (StoragePointer src dst:env)
 | varName == variableToVarName src = StoragePointer src dst:lookupVarNameInEnvironment varName env
 | otherwise = lookupVarNameInEnvironment varName env

varNameAndEnvironmentToVariable :: VarName -> Environment -> Maybe Variable
varNameAndEnvironmentToVariable n env = listToMaybe [ v | VarDef minLevel v <- varInfos ]
    where
        varInfos = lookupVarNameInEnvironment n env
        minLevel = minimum [ i | VarDef i _ <- varInfos ]

pointerVariableAndEnvironmentToPointedVariable :: Variable -> Environment -> Maybe Variable
pointerVariableAndEnvironmentToPointedVariable srcVar env = listToMaybe dstVars
    where
        dstVars = [ v2 | StoragePointer v1 v2 <- lookupVariableInEnvironment srcVar env, v1 == srcVar]

removeLocalVariablesInEnvironment :: Environment -> Environment
removeLocalVariablesInEnvironment env = nonLocalVarDefs ++ newStoragePointers
    where
        nonLocalVarDefs = [ VarDef i v | VarDef i v <- env, i>0 ]
        localVarDefs = [ VarDef 0 v | VarDef 0 v <- env ]
        newStoragePointers = [ StoragePointer src dst | StoragePointer src dst <- env, VarDef 0 src `notElem` localVarDefs ]

-- This takes a function definition and an environment which comes from the contract definitions
-- typically including state variable definitions.
functionAndEnvironmentToCFGNodes :: AsValue s => s -> Environment -> [CFGNode]
functionAndEnvironmentToCFGNodes t env = statementsAndEnvironmentToCFGNodes stats env'
    where
        -- need to find declared variable names due to parameters and returns
        stats = functionDefinitionToBodyStatements t
        paramVars = functionDefinitionToParameterVariables t
        returnVars = functionDefinitionToReturnVariables t
        env' = map variableToLocalVarDef (paramVars ++ returnVars) ++ env

statementsAndEnvironmentToCFGNodes :: [Value] -> Environment -> [CFGNode]
statementsAndEnvironmentToCFGNodes stats env =
    statementsAndEnvironmentAndNextNodeIdToCFGNodes stats env Nothing

-- cFGNodesToEntryNodeId :: [CFGNode] -> String
-- cFGNodesToEntryNodeId ((_, nodeId, _):_) = nodeId

statementsAndEnvironmentAndNextNodeIdToCFGNodes :: [Value] -> Environment -> Maybe String -> [CFGNode]
statementsAndEnvironmentAndNextNodeIdToCFGNodes [] env _ = []
statementsAndEnvironmentAndNextNodeIdToCFGNodes (stat:stats) env mNextNodeId = hdCFGNodes ++ tlCFGNodes
    where
        mNodeId = if null stats then mNextNodeId
                        else Just $ src $ head stats
        hdCFGNodes = statementAndEnvironmentAndNextNodeIdToCFGNodes stat env mNodeId
        env' = env
        tlCFGNodes = statementsAndEnvironmentAndNextNodeIdToCFGNodes stats env' mNextNodeId

statementAndEnvironmentAndNextNodeIdToCFGNodes :: Value -> Environment -> Maybe String -> [CFGNode]
statementAndEnvironmentAndNextNodeIdToCFGNodes stat env mNextNodeId
    | nt == "Block" = blockAndNextNodeIdToCFGNodes stat mNextNodeId
    | nt == "VariableDeclarationStatement" = [(node, id, maybeToList mNextNodeId)]
    | nt == "ExpressionStatement" = [(node, id, maybeToList mNextNodeId)]
    | nt == "EmitStatement" = [(node, id, maybeToList mNextNodeId)]
    | nt == "RevertStatement" = [(node, id, [])]
    | nt == "Return" = [(node, id, [])]
    | nt == "IfStatement" = ifStatementAndNextNodeIdToCFGNodes stat mNextNodeId
    | otherwise = [(node, id, ["END"])]
    where
        node = (stat, env)
        nt = nodeType stat
        id = src stat

blockAndNextNodeIdToCFGNodes :: AsValue s => s -> Maybe String -> [CFGNode]
blockAndNextNodeIdToCFGNodes stat mNextNodeId = undefined
    where
        innerStats = undefined

contractToCFGEdgesList :: AsValue s => s -> [CFGEdges]
contractToCFGEdgesList t = concat cFGEdgesList
    where
        functions = contractToFunctionDefinitions t
        env = map (variableToLocalVarDef . variableDeclarationToVariable) $ contractToStateVariableDeclarations t
        cFGEdgesList = map (`functionAndEnvironmentToCFGEdgesList` env) functions

-- commented out on 5 Apr.
functionAndEnvironmentToCFGEdgesList :: AsValue s => s -> Environment -> [CFGEdges]
functionAndEnvironmentToCFGEdgesList t env = statementsAndEnvironmentToCFGEdgesList stats env'
    where
        stats = functionDefinitionToBodyStatements t
        paramVars = functionDefinitionToParameterVariables t
        returnVars = functionDefinitionToReturnVariables t
        paramVarDefs = map variableToLocalVarDef paramVars
        --returnVarDefs = map variableToLocalVarDef returnVars
        -- This (VarDef 1) is a temporal solution.  To be improved.
        returnVarDefs = map (VarDef 1) returnVars
        env' = paramVarDefs ++ returnVarDefs ++ incrementEnvironmentLevel env

-- functionAndEnvironmentToCFGEdgesList :: AsValue s => s -> Environment -> [CFGEdges]
-- functionAndEnvironmentToCFGEdgesList t env = statementsAndEnvironmentToCFGEdgesListAux Nothing bodyBlock env' [] []
--     where
--         bodyBlock = functionDefinitionToBody t
--         paramVars = functionDefinitionToParameterVariables t
--         returnVars = functionDefinitionToReturnVariables t
--         paramVarDefs = map variableToLocalVarDef paramVars
--         -- This (VarDef 1) is a temporal solution.
--         -- To be improved (eg. Enriching Environment to carry return params separately).
--         returnVarDefs = map (VarDef 1) returnVars
--         env' = paramVarDefs ++ returnVarDefs ++ incrementEnvironmentLevel env

statementsAndEnvironmentToCFGEdgesList :: [Value] -> Environment -> [CFGEdges]
statementsAndEnvironmentToCFGEdgesList stats env = statementsAndEnvironmentToCFGEdgesListAux Nothing stats env [] []

statementsAndEnvironmentToCFGEdgesListAux :: Maybe String -> [Value] -> Environment -> CFGEdges -> [CFGEdges] -> [CFGEdges]
statementsAndEnvironmentToCFGEdgesListAux _ [] env [] ess = ess
statementsAndEnvironmentToCFGEdgesListAux mFollowingNodeId [] env es ess =
    if null lastNextNodeIds
        then let es' = initEs ++ [(lastNode, lastNodeId, maybeToList mFollowingNodeId)]
                in es':ess
        else es:ess
    where
        initEs = init es
        (lastNode, lastNodeId, lastNextNodeIds) = last es
statementsAndEnvironmentToCFGEdgesListAux mFollowingNodeId (stat:stats) env es ess
    | nt == "Block" = let es' = es ++ [(node, id, nextNodeIdList)]
                          innerStats = blockToStatements stat
                          env' = incrementEnvironmentLevel env
                          ess' = ess ++ statementsAndEnvironmentToCFGEdgesListAux mNextNodeId innerStats env' [] []
                      in statementsAndEnvironmentToCFGEdgesListAux mFollowingNodeId stats env es' ess'
    | nt == "VariableDeclarationStatement" = let es' = es ++ [(node, id, nextNodeIdList)]
                                             in statementsAndEnvironmentToCFGEdgesListAux mFollowingNodeId stats newEnv es' ess
    | nt == "ExpressionStatement" = let es' = es ++ [(node, id, nextNodeIdList)]
                                     in statementsAndEnvironmentToCFGEdgesListAux mFollowingNodeId stats newEnv es' ess
    | nt == "EmitStatement" = let es' = es ++ [(node, id, nextNodeIdList)]
                              in statementsAndEnvironmentToCFGEdgesListAux mFollowingNodeId stats env es' ess
    | nt == "RevertStatement" = let es' = es ++ [(node, id, nextNodeIdList)]
                                in statementsAndEnvironmentToCFGEdgesListAux mFollowingNodeId stats env es' ess
    | nt == "Return" = let es' = es ++ [(node, id, ["END"])]
                       in statementsAndEnvironmentToCFGEdgesListAux mFollowingNodeId stats env es' ess
    | nt == "IfStatement" = let trueCaseStatements = ifStatementToTrueStatements stat
                                trueCaseBody = ifStatementToTrueBody stat
                                falseCaseStatements = ifStatementToFalseStatements stat
                                falseCaseBody = ifStatementToFalseBody stat
                                trueCaseCFGEdges = statementsAndEnvironmentToCFGEdgesListAux mNextNodeId trueCaseStatements env [] []
                                falseCaseCFGEdges = statementsAndEnvironmentToCFGEdgesListAux mNextNodeId falseCaseStatements env [] []
                                withoutFalseBody = isIfStatementWithoutFalseBody stat
                                es' = if withoutFalseBody
                                      then es ++ [(node, id, src (head trueCaseStatements):nextNodeIdList)]
                                      else es ++ [(node, id, map (src . head) [trueCaseStatements, falseCaseStatements])]
                                ess' = if withoutFalseBody
                                       then ess ++ trueCaseCFGEdges
                                       else ess ++ trueCaseCFGEdges ++ falseCaseCFGEdges
                            in statementsAndEnvironmentToCFGEdgesListAux mFollowingNodeId stats env es' ess'
    -- | otherwise = let es' = es ++ [(node, id, ["END"])]
    --               in statementsAndEnvironmentToCFGEdgesListAux mFollowingNodeId stats env es' ess
    where
        nt = nodeType stat
        id = src stat
        mNextNodeId = if null stats then mFollowingNodeId else Just (src $ head stats)
        nextNodeIdList = maybeToList mNextNodeId
        node = (stat, env)
        varDefs = environmentToVarDefs env
        newEnv = statementAndEnvironmentToUpdatedEnvironment stat env

statementAndEnvironmentToUpdatedEnvironment :: Value -> Environment -> Environment
statementAndEnvironmentToUpdatedEnvironment stat env
    | nt == "Block" = incrementEnvironmentLevel env
    | nt == "VariableDeclarationStatement" = let var = variableDeclarationStatementToDeclaredVariable stat
                                                 varDef = variableToLocalVarDef var
                                                 decl = variableDeclarationStatementToVariableDeclaration stat
                                                 isStorageVariableDeclaration = "storage" == variableDeclarationToStorageLocation decl
                                                in if isStorageVariableDeclaration
                                                    then let mInitValueVariable =
                                                                    do initValue <- variableDeclarationStatementToInitialValue stat
                                                                       objName <- expressionToPointableStorageVariableName initValue
                                                                       VarDef i v <- listToMaybe $ lookupVarNameInEnvironment objName varDefs
                                                                       return v
                                                             mPointer = fmap (StoragePointer var) mInitValueVariable
                                                            in varDef:env ++ maybeToList mPointer
                                                    else varDef:env
    --- To clarify: in Expression Statement, no new variable is declared, but it can update an assignment concerning a storage pointer.
    --- question.  is there a use of "x=a" in another expression (eg. as an argument of a function call like "f(x=a, b)") which changes the pointer?
    | nt == "ExpressionStatement" = let expr = expressionStatementToExpression stat
                                        ntExpr = nodeType expr
                                        env' = if ntExpr == "Assignment" -- && variableDeclarationToStorageLocation stat == "storage"
                                                    then let lExpr = expressionInAssignmentFormToLeftExpression expr
                                                             var = expressionInIdentifierFormToVariable lExpr
                                                             isStorage = [ vsrc | StoragePointer vsrc vdst <- varDefs ]
                                                             --varDefs = lookupVariableInEnvironment var env 
                                                             rExpr = expressionInAssignmentFormToRightExpression expr
                                                             mPointedVar =
                                                                do objName <- expressionToPointableStorageVariableName rExpr
                                                                   VarDef i v <- listToMaybe $ lookupVarNameInEnvironment objName varDefs
                                                                   return v
                                                             mPointer = fmap (StoragePointer var) mPointedVar
                                                            in env ++ maybeToList mPointer
                                                    else env
                                      in env'
    | nt == "EmitStatement" = env
    | nt == "RevertStatement" = env
    | nt == "Return" = env
    | nt == "IfStatement" = env
    | otherwise = env
    where
        nt = nodeType stat
        id = src stat
        node = (stat, env)
        varDefs = environmentToVarDefs env

-- followingNodeIdAndStatementsToCFGEdgesListAux :: Maybe String -> [Value] -> CFGEdges -> [CFGEdges] -> [CFGEdges]
-- followingNodeIdAndStatementsToCFGEdgesListAux _ [] es ess = es:ess
-- followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId (stat:stats) es ess
--     | nt == "Block" = let es' = es ++ [(stat, id, nextNodeIdList)]
--                           innerStats = blockToStatements stat
--                           ess' = ess ++ followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId innerStats [] []
--                       in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess'
--     | nt == "VariableDeclarationStatement" = let es' = es ++ [(stat, id, nextNodeIdList)]
--                                              in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess
--     | nt == "ExpressionStatement" = let es' = es ++ [(stat, id, nextNodeIdList)]
--                                     in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess
--     | nt == "EmitStatement" = let es' = es ++ [(stat, id, nextNodeIdList)]
--                               in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess
--     | nt == "RevertStatement" = let es' = es ++ [(stat, id, [])]
--                                 in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess
--     | nt == "Return" = let es' = es ++ [(stat, id, [])]
--                        in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess
--     | nt == "IfStatement" = let trueCaseStatements = ifStatementToTrueStatements stat
--                                 falseCaseStatements = ifStatementToFalseStatements stat
--                                 trueCaseCFGEdges = followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId trueCaseStatements [] []
--                                 falseCaseCFGEdges = followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId falseCaseStatements [] []
--                                 withoutFalseBody = isIfStatementWithoutFalseBody stat
--                                 es' = if withoutFalseBody
--                                       then es ++ [(stat, id, src (head trueCaseStatements):nextNodeIdList)]
--                                       else es ++ [(stat, id, map (src . head) [trueCaseStatements, falseCaseStatements])]
--                                 ess' = if withoutFalseBody
--                                        then ess ++ trueCaseCFGEdges
--                                        else ess ++ trueCaseCFGEdges ++ falseCaseCFGEdges
--                                 -- (es', ess') = if withoutFalseBody
--                                 --     then (es ++ [(stat, id, src (head trueCaseStatements):nextNodeIdList)], ess ++ trueCaseCFGEdges)
--                                 --     else (es ++ [(stat, id, map (src . head) [trueCaseStatements, falseCaseStatements])], ess ++ trueCaseCFGEdges ++ falseCaseCFGEdges)
--                             in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess'
--     | otherwise = let es' = es ++ [(stat, id, ["END"])]
--                   in followingNodeIdAndStatementsToCFGEdgesListAux mFollowingNodeId stats es' ess
--     where
--         nt = nodeType stat
--         id = src stat
--         nextNodeIdList = if null stats then maybeToList mFollowingNodeId else [src $ head stats]

cFGEdgesToFinalNodes :: CFGEdges -> CFGEdges
cFGEdgesToFinalNodes cFGEdges = [ ((v, env), nodeId, []) | ((v, env), nodeId, []) <- cFGEdges ]

cFGEdgesToEntryNodes :: CFGEdges -> CFGEdges
cFGEdgesToEntryNodes cFGEdges = [ (info, nodeId, nextNodes) | (info, nodeId, nextNodes) <- cFGEdges, nodeId `notElem` referredNodeIds]
    where
        referredNodeIds = concat [ nextNodeIds | (_, _, nextNodeIds) <- cFGEdges ]

cFGEdgesToNodeIds :: CFGEdges -> [String]
cFGEdgesToNodeIds [] = []
cFGEdgesToNodeIds ((_, nodeId, _):es) = nodeId:cFGEdgesToNodeIds es

cFGEdgesToSimpleGraph :: CFGEdges -> [(String, [String])]
cFGEdgesToSimpleGraph [] = []
cFGEdgesToSimpleGraph (((stat, env), id, ids):es) = (id, ids):cFGEdgesToSimpleGraph es

simplifyCFGEdges :: CFGEdges -> [(String, Environment, [String])]
simplifyCFGEdges [] = []
simplifyCFGEdges (((stat, env), id, ids):es) = (id, env, ids):simplifyCFGEdges es

ifStatementAndNextNodeIdToCFGNodes :: Value -> Maybe String -> [CFGNode]
ifStatementAndNextNodeIdToCFGNodes stat ms = undefined
    -- where
    --     trueCaseStatements = stat ^.. key "trueBody" . key "statements"


    --     falseCaseStatements = stat ^.. key "falseBody" . key "statements"

-- ifStatementToCFGNodes :: AsValue p => p -> CFGEdges
-- ifStatementToCFGNodes t = [(trueBody, trueNodeId, [])]
--     where
--         trueBody = ifStatementToTrueBody t
--         trueNodeId = blockToNodeId trueBody
--         falseBody = ifStatementToFalseBody t
--         falseNodeId = blockToNodeId falseBody
