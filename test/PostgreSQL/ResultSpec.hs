{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module PostgreSQL.ResultSpec (spec) where

import           Control.Monad (when)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Except (runExceptT)
import qualified Control.Monad.Reader as Reader
import           Data.ByteString (ByteString)
import           Data.Functor.Alt (Alt (..))
import qualified Data.HashMap.Strict as HashMap
import           Data.SOP (HCollapse (hcollapse), K (..), NP (..), SListI)
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as PQ
import           PostgreSQL.Result (columnWith, namedColumnWith, runProcessor)
import qualified PostgreSQL.Result.Class as Class
import qualified PostgreSQL.Result.Column as Column
import           PostgreSQL.Types (ProcessorError (..), Value)
import           Test.Hspec (Spec, describe, it, shouldBe)

data StaticResult = StaticResult
  { staticResult_columns :: Vector.Vector (PQ.Oid, PQ.Format)
  , staticResult_namedColumns :: HashMap.HashMap ByteString PQ.Column
  , staticResult_rows :: Vector.Vector (Vector.Vector Value)
  }

newtype StaticResultT m a = StaticResultT
  { _unStaticResultT :: Reader.ReaderT StaticResult m a }
  deriving newtype (Functor, Applicative, Monad, MonadError e, MonadFail)

instance Alt m => Alt (StaticResultT m) where
  StaticResultT lhs <!> StaticResultT rhs = StaticResultT $ lhs <!> rhs

  {-# INLINE (<!>) #-}

runStaticResultT
  :: SListI xs
  => NP (K (PQ.Oid, PQ.Format, Maybe ByteString)) xs
  -> Vector.Vector (NP (K Value) xs)
  -> StaticResultT m a
  -> m a
runStaticResultT columns rows (StaticResultT (Reader.ReaderT run)) =
  run StaticResult
    { staticResult_columns = vectorColumns
    , staticResult_namedColumns = namedColumns
    , staticResult_rows = vectorRows
    }
  where
    listColumns = hcollapse columns

    vectorColumns = Vector.fromList $ map (\(typ, format, _) -> (typ, format)) listColumns

    namedColumns = HashMap.fromList
      [ (name, col)
      | (col, (_, _, Just name)) <- zip [0 ..] listColumns
      ]

    vectorRows = Vector.map (Vector.fromList . hcollapse) rows

columnInfo :: MonadFail m => PQ.Column -> StaticResultT m (PQ.Oid, PQ.Format)
columnInfo (PQ.Col col) = StaticResultT $ Reader.ReaderT $ \staticResult -> do
  let columnIndex = fromIntegral col

  when (columnIndex >= Vector.length (staticResult_columns staticResult)) $
    fail "Column index is out of bounds"

  pure $ staticResult_columns staticResult Vector.! columnIndex

instance MonadFail m => Class.HasResult (StaticResultT m) where
  numColumns = StaticResultT $ Reader.ReaderT $ \staticResult ->
    pure $ fromIntegral $ Vector.length $ staticResult_columns staticResult

  numRows = StaticResultT $ Reader.ReaderT $ \staticResult ->
    pure $ fromIntegral $ Vector.length $ staticResult_rows staticResult

  columnType = fmap fst . columnInfo

  columnFormat = fmap snd . columnInfo

  cellValue (PQ.Col col) (PQ.Row row) = StaticResultT $ Reader.ReaderT $ \staticResult -> do
    let columnIndex = fromIntegral col

    when (columnIndex >= Vector.length (staticResult_columns staticResult)) $
      fail "Column index is out of bounds"

    let rowIndex = fromIntegral row

    when (rowIndex >= Vector.length (staticResult_rows staticResult)) $
      fail "Row index is out of bounds"

    pure $ staticResult_rows staticResult Vector.! rowIndex Vector.! columnIndex

  columnFromName name = StaticResultT $ Reader.ReaderT $ \staticResult ->
    pure $ HashMap.lookup name $ staticResult_namedColumns staticResult

spec :: Spec
spec = do
  describe "runProcessor" $ do
    it "resolves column" $ do
      let columns = K (PQ.invalidOid, PQ.Text, Nothing) :* Nil

      result <- runExceptT $ runStaticResultT columns Vector.empty $
        runProcessor (columnWith Column.ignored) $ \_ _ ->
          pure ()

      result `shouldBe` Right ()

    it "fails on too few columns" $ do
      result <- runExceptT $ runStaticResultT Nil Vector.empty $
        runProcessor (columnWith Column.ignored) $ \_ _ ->
          pure ()

      let
        expectedError = NotEnoughColumns
          { processorError_wantedColumns = 1
          , processorError_haveColumns = 0
          }

      result `shouldBe` Left [expectedError]

    it "resolves named column" $ do
      let missingColumnName = "nonexistent"
      let columns = K (PQ.invalidOid, PQ.Text, Just missingColumnName) :* Nil

      result <- runExceptT $ runStaticResultT columns Vector.empty $
        runProcessor (namedColumnWith missingColumnName Column.ignored) $ \_ _ ->
          pure ()

      result `shouldBe` Right ()

    it "fails on missing named column" $ do
      let missingColumnName = "nonexistent"
      let columns = K (PQ.invalidOid, PQ.Text, Nothing) :* Nil

      result <- runExceptT $ runStaticResultT columns Vector.empty $
        runProcessor (namedColumnWith missingColumnName Column.ignored) $ \_ _ ->
          pure ()

      let expectedError = MissingNamedColumn {processorError_wantedColumnName = missingColumnName}
      result `shouldBe` Left [expectedError]
