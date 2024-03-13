module Parser where

import Data.Text ( Text )
import Data.Text.Encoding ( encodeUtf8 )
import Data.Aeson ( decode, Value )
import Data.ByteString.Lazy ( fromStrict )

parse :: Text -> Maybe Value
parse = decode . fromStrict . encodeUtf8
