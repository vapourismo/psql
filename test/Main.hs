module Main (main) where

import           Control.Concurrent.CGroup (initRTSThreads)
import qualified PostgreSQL.Result.RowSpec
import           Test.Hspec (describe, hspec)

main :: IO ()
main = do
  initRTSThreads
  hspec $
    describe "PostgreSQL.Row" PostgreSQL.Result.RowSpec.spec
