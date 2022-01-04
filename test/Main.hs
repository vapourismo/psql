module Main (main) where
import           Control.Concurrent.CGroup (initRTSThreads)
import qualified PostgreSQL.ResultSpec
import           Test.Hspec (describe, hspec)

main :: IO ()
main = do
  initRTSThreads
  hspec $
    describe "PostgreSQL.Result" PostgreSQL.ResultSpec.spec
