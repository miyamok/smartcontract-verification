{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}


module Main where
import Reader
import Parser
import Data.Aeson
import Prelude.Compat

import Text.JSON
-- import qualified Data.Text.Lazy.IO as T
-- import qualified Data.Text.Lazy.Encoding as T

main :: IO ()
main = do r <- reader "./sample-solidity/first.sol"
          let req = parse r
          print req
