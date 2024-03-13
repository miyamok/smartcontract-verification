{-# LANGUAGE OverloadedStrings #-}

module Reader where
import Data.Text as T ( Text, lines, unlines )
import Control.Monad.IO.Class (MonadIO)
import Shelly ( run, shelly, silently, Sh )
import Data.ByteString (ByteString)
import qualified Data.ByteString as TL
import Data.Text.Lazy.Encoding as TLE
import Data.Text.Encoding as TSE

reader :: MonadIO m => Text -> m Text
reader filename = shelly $ silently $ do
    ret <- run "solc" ["--ast-compact-json", filename]
    return $ cleaningSolcASTOutput ret

--readerBS :: MonadIO m => Text -> m Text
readerBS :: Text -> Sh ByteString
readerBS filename = do
    ret <- run "solc" ["--ast-compact-json", filename]
    let a = cleaningSolcASTOutput ret
    let b = TSE.encodeUtf8 a
    return b

cleaningSolcASTOutput :: Text -> Text
cleaningSolcASTOutput =
    T.unlines . tail . tail . tail . tail . T.lines
