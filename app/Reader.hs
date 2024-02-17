{-# LANGUAGE OverloadedStrings #-}
module Reader where
import Shelly
import Data.Text as T

reader filename = shelly $ silently $ do
    ret <- run "solc" ["--ast-compact-json", filename]
    let cleanAST = cleaningSolcASTOutput ret
    return cleanAST

cleaningSolcASTOutput :: Text -> Text
cleaningSolcASTOutput t = T.unlines $ Prelude.tail $ Prelude.tail $ Prelude.tail $ Prelude.tail $ T.lines t
    



-- import System.Process

-- test = createProcess (proc "ls" [])
-- import Shelly
-- import Control.Applicative
-- import Control.Monad

-- convertEpub :: FilePath -> IO ()
-- convertEpub fname = undefined

-- test = shelly $ do
--     liftIO $ putStrLn "Hello, world!  I'm in Shelly"
--     --names <- Shelly.find (pure . Shelly.hasExt "hs") "."
--     fnames <- Shelly.find "*.hs"
--     liftIO $ forM_ fnames $ \fname -> do
--         putStrLn $ "Processing file " ++ show fname
--         convertEpub fname