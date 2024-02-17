{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import System.Directory.Internal.Prelude (getArgs)
import Reader ( reader )
import Parser ( parse )
import Data.Aeson ()
import Prelude.Compat ( print, head, IO, String, map, putStrLn )
import Text.JSON ()
import Data.Text ( Text, pack )

argsToOptsAndFilename :: [Text] -> ([Text], [Text])
argsToOptsAndFilename args = ([], args)

main :: IO ()
main = do args <- getArgs
          let (opts, files) = argsToOptsAndFilename (map pack args)
          case files of
            [] -> putStrLn "Error: No .sol file name given"
            _ -> do t <- reader (head files)
                    let mv = parse t
                    print mv
