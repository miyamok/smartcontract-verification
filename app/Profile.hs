{-# LANGUAGE OverloadedStrings #-}

module Profile where

import Control.Lens
import Data.Aeson.Lens
--import Data.ByteString ()
import Data.Aeson.Key ( fromString )
import Data.Text ( Text, pack )
import Data.Aeson
import Data.Graph
--import qualified Data.ByteString as Text

type CFGMatrix = (Value, Int, [Int])

absolutePath :: AsValue s => s -> Maybe Text
absolutePath t = t ^? key "absolutePath" . _String

contracts :: AsValue s => s -> [Value]
contracts t = t ^.. key "nodes" . values . filtered (has (key "nodeType" . _String . only "ContractDefinition"))

contractNameToVariableDeclarations :: AsValue s => s -> String -> [Value]
contractNameToVariableDeclarations t contractName = t ^.. key "nodes" . values . filtered (\x -> has (key "nodeType" . _String . only "ContractDefinition") x && has (key "name" . _String . only (Data.Text.pack contractName)) x) . key "nodes" . values . filtered (has (key "nodeType"._String.only "VariableDeclaration"))

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

blocksToCFGMatrices :: [Value] -> [CFGMatrix]
blocksToCFGMatrices = concatMap blockToCFGMatrices

blockToCFGMatrices :: Value -> [CFGMatrix]
blockToCFGMatrices v =
    statementsToCFGMatrices statements
        where statements = v ^.. key "statements" . values

cFGMatricesToEntryId :: [CFGMatrix] -> Int
cFGMatricesToEntryId ((_, i, _):_) = i

concatCFGMatrices :: [CFGMatrix] -> [CFGMatrix] -> [CFGMatrix]
concatCFGMatrices cfg1 cfg2 = undefined
--- look for unfilled next id's in cfg1 and fill them by entry of cfg2

statementsToCFGMatrices :: [Value] -> [CFGMatrix]
statementsToCFGMatrices = indexAndStatementsToCFGMatrices 0

indexAndStatementToCFGMatrices :: Int -> Value -> [CFGMatrix]
indexAndStatementToCFGMatrices i stat
--    | nodeType == "Assignment" = [(stat, i, [i+1])]
    | nodeType == "Return" = [(stat, i, [])]
    | nodeType == "ExpressionStatement" = [(stat, i, [i+1])]
    | nodeType == "VariableDeclarationStatement" = [(stat, i, [i+1])]
    | nodeType == "EmitStatement" = [(stat, i, [i+1])]
    | nodeType == "RevertStatement" = [(stat, i, [])]
--    | otherwise = [(stat, i, [i+1])]
    where
        nodeType = head $ stat ^.. key "nodeType" . _String

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
