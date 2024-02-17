{-# LANGUAGE OverloadedStrings #-}

module Reader where
import Shelly ( run, shelly, silently )
import Data.Text as T ( Text, lines, unlines )
import Control.Monad.IO.Class (MonadIO)

reader :: MonadIO m => Text -> m Text
reader filename = shelly $ silently $ do
    ret <- run "solc" ["--ast-compact-json", filename]
    return $ cleaningSolcASTOutput ret

cleaningSolcASTOutput :: Text -> Text
cleaningSolcASTOutput =
    T.unlines . tail . tail . tail . tail . T.lines
