module LiveVariables where

import Data.Aeson
import Data.Aeson.Lens
import Profile
import CFG
import Data.Maybe
import Debug.Trace

-- nodeToUpdatedVariables :: CFGNode -> [Value]
-- nodeToUpdatedVariables node
-- | nodeType == "VariableDeclarationStatement" = 
-- | otherwise = 
--     where nodeType = nodeType node

-- generally several variables (eg. tuple unpacking)
expressionInAssignmentFormToUpdatedVariables :: AsValue s => s -> [Variable]
expressionInAssignmentFormToUpdatedVariables stat
 | nt == "Identifier" = [expressionInIdentifierFormToVariable leftExpr]
 | nt == "MemberAccess" = maybeToList $ expressionInMemberAccessFormToVariable leftExpr
 | nt == "IndexAccess" =
    let baseExpr = expressionInIndexAccessFormToBaseExpression leftExpr
      in expressionToFreeVariables baseExpr
    where
        leftExpr = expressionInAssignmentFormToLeftExpression stat
        nt = nodeType leftExpr

-- member access
-- obj.member
-- objArr[i].member
-- objArr[i].members[j].belonging
-- objArr.member.belongings[i]

-- tuple
-- (bool s, ) = a.call{ value: 0.01 ether }("");


expressionInAssignmentFormToReadVariables :: AsValue s => s -> [Variable]
expressionInAssignmentFormToReadVariables stat
 | lftNodeType == "Identifier" = rhtFvars ++ lftReadVars
 | lftNodeType == "MemberAccess" =
    let var = expressionInMemberAccessFormToVariable lftExpr
        -- should exactly count parameters occurring in the left expression!
        -- should not contain non-variables! (currently, enum objects are included)
     in lftFvars ++ rhtFvars ++ lftReadVars
 | lftNodeType == "IndexAccess" =
    let idxExpr = expressionInIndexAccessFormToIndexExpression lftExpr
     in expressionToFreeVariables idxExpr ++ rhtFvars ++ lftReadVars
    where
        lftExpr = expressionInAssignmentFormToLeftExpression stat
        rhtExpr = expressionInAssignmentFormToRightExpression stat
        lftFvars = expressionToFreeVariables lftExpr
        rhtFvars = expressionToFreeVariables rhtExpr
        lftNodeType = nodeType lftExpr
        operator = expressionInAssignmentFormToOperator stat
        lftReadVars = if isArithmeticAssignmentOperator operator then lftFvars else []

variableDeclarationStatementToReadVariables :: AsValue s => s -> [Variable]
variableDeclarationStatementToReadVariables = expressionToFreeVariables . variableDeclarationStatementToInitialValue

variableDeclarationStatementToUpdatedVariables :: AsValue s => s -> [Variable]
variableDeclarationStatementToUpdatedVariables stat =
    variableDeclarationStatementToDeclaredVariable stat:expressionToUpdatedVariables expr
    where
        expr = variableDeclarationStatementToInitialValue stat

statementToReadVariables :: AsValue s => s -> [Variable]
statementToReadVariables stat
 | nt == "VariableDeclarationStatement" = variableDeclarationStatementToReadVariables stat
 | nt == "ExpressionStatement" = expressionToReadVariables $ expressionStatementToExpression stat
 | nt == "WhileStatement" = expressionToFreeVariables $ statementInWhileFormToConditionalExpression stat
    where
        nt = nodeType stat

statementToUpdatedVariables :: AsValue s => s -> [Variable]
statementToUpdatedVariables stat
 | nt == "VariableDeclarationStatement" = variableDeclarationStatementToUpdatedVariables stat
 | nt == "ExpressionStatement" = expressionToUpdatedVariables $ expressionStatementToExpression stat
 | nt == "WhileStatement" = expressionToUpdatedVariables $ statementInWhileFormToConditionalExpression stat
    where
        nt = nodeType stat

expressionInFunctionCallFormToReadVariables :: AsValue s => s -> [Variable]
expressionInFunctionCallFormToReadVariables stat = concatMap expressionToFreeVariables argExprs
    where
        argExprs = expressionInFunctionCallFormToArguments stat

expressionInFunctionCallFormToUpdatedVariables :: AsValue s => s -> [Variable]
expressionInFunctionCallFormToUpdatedVariables stat = concatMap expressionToUpdatedVariables argExprs
    where
        argExprs = expressionInFunctionCallFormToArguments stat

expressionToReadVariables :: AsValue s => s -> [Variable]
expressionToReadVariables stat
 | nt == "Identifier" = [expressionInIdentifierFormToVariable stat]
 | nt == "UnaryOperation" = expressionToFreeVariables $ expressionInUnaryOperationFormToSubExpression stat
 | nt == "BinaryOperation" = concatMap expressionToFreeVariables (expressionInBinaryOperationFormToExpressions stat)
 | nt == "IndexAccess" = let be = expressionInIndexAccessFormToBaseExpression stat
                             ie = expressionInIndexAccessFormToIndexExpression stat
                            in expressionToFreeVariables be ++ expressionToFreeVariables ie
 | nt == "MemberAccess" = maybeToList $ expressionInMemberAccessFormToVariable stat
 | nt == "Literal" = []
 | nt == "Assignment" = expressionInAssignmentFormToReadVariables stat
 | nt == "FunctionCall" = expressionInFunctionCallFormToReadVariables stat
    where
        nt = nodeType stat

expressionToUpdatedVariables :: AsValue s => s -> [Variable]
expressionToUpdatedVariables stat
 | nt == "Identifier" = []
 | nt == "UnaryOperation" = let subExpr = expressionInUnaryOperationFormToSubExpression stat
                                op = expressionInUnaryOperationFormToOperator stat
                              in if isIncrementOrDecrementOperator op
                                then [expressionInIdentifierFormToVariable subExpr]
                                else []
 | nt == "BinaryOperation" = concatMap expressionToUpdatedVariables (expressionInBinaryOperationFormToExpressions stat)
 | nt == "IndexAccess" = let be = expressionInIndexAccessFormToBaseExpression stat
                             ie = expressionInIndexAccessFormToIndexExpression stat
                            in expressionToUpdatedVariables be ++ expressionToUpdatedVariables ie
 | nt == "MemberAccess" = [] --maybeToList $ expressionInMemberAccessFormToVariable stat
 | nt == "Literal" = []
 | nt == "Assignment" = expressionInAssignmentFormToUpdatedVariables stat
 | nt == "FunctionCall" = expressionInFunctionCallFormToUpdatedVariables stat
    where
        nt = nodeType stat
