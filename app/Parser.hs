{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Prelude.Compat ()
import Data.Aeson

import Control.Applicative (empty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text.Lazy.Encoding as TLE
import Data.Tree
import Data.Text as T
import Data.Text.Encoding as TSE
import Data.ByteString.Lazy
import Debug.Trace

--data SyntaxTree = SolDat {src :: String, nodeType :: String} deriving (Show)
data SyntaxTree = SolDat (Tree (String, String)) deriving (Show)

-- instance FromJSON SyntaxTree where
-- --   parseJSON (Object v) = SolDat <$>
-- --                          v .: "src" <*>
-- --                          v .: "nodeType"

--     parseJSON = withObject "SyntaxTree" $ \obj -> do
--         src <- obj .: "src"
--         nodeType <- obj .: "nodeType"
--         nodes <- obj .: "nodes"
--         children <- fmap parseJSON nodes
--         return $ SolDat (Node (src, nodeType) [])
--     -- parseJSON _          = Control.Applicative.empty

-- solcJsonToTree :: ByteString -> Maybe SyntaxTree
-- solcJsonToTree = decode

-- Data.ByteString.Lazy.fromStrict $ encodeUtf8 (Data.Text.pack "BOO")
-- decode :: FromJSON a = Data.ByteString.Lazy.Internal.ByteString -> Maybe a
parse :: Text -> Maybe Value
parse l = let t = Data.ByteString.Lazy.fromStrict $ TSE.encodeUtf8 l
              ret = decode t
            in ret