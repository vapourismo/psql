module Main (main) where

import           Control.Concurrent.CGroup (initRTSThreads)
import qualified PostgreSQL.Result.RowSpec
import qualified PostgreSQL.ResultSpec
import           Test.Hspec (describe, hspec)

main :: IO ()
main = do
  initRTSThreads
  hspec $ do
    describe "PostgreSQL.Result" PostgreSQL.ResultSpec.spec
    describe "PostgreSQL.Row" PostgreSQL.Result.RowSpec.spec
