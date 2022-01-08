{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module PostgreSQL.Result.RowSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import           Control.Monad (when)
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Massiv.Array as Massiv
import           Data.Traversable (for)
import qualified Data.Vector as Vector
import           Database.PostgreSQL.LibPQ (Format (Text), Oid, invalidOid)
import qualified PostgreSQL.Result.Cell as Cell
import qualified PostgreSQL.Result.Column as Column
import qualified PostgreSQL.Result.Row as Row
import           PostgreSQL.Types (ColumnNum, ProcessorError (..), ProcessorErrors, Value (..))

type Matrix = Massiv.Array Massiv.B Massiv.Ix2

runMatrixRow
  :: Vector.Vector (Oid, Format)
  -> HashMap.HashMap ByteString ColumnNum
  -> Matrix Value
  -> Row.Row a
  -> Either ProcessorErrors [a]
runMatrixRow columns names datas row = Except.runExcept $ do
  let
    Massiv.Sz2 numRows' numCols' = Massiv.size datas

    numRows = fromIntegral numRows'

    numCols = min (fromIntegral numCols') $ fromIntegral $ Vector.length columns

  runner <- Row.runRow row $ \req -> do
    col <-
      case Row.columnRequest_position req of
        Row.FixedColumn col -> pure col

        Row.NamedColumn name ->
          case HashMap.lookup name names of
            Just col -> pure col
            Nothing -> Except.throwError [MissingNamedColumn name]

    when (col >= numCols) $ Except.throwError [NotEnoughColumns col numCols]

    let (oid, format) = columns Vector.! fromIntegral col

    cell <-
      Except.liftEither $ first (fmap (ColumnParserError col oid format)) $
        Column.parseColumn (Row.columnRequest_parser req) oid format

    pure $ Reader.ReaderT $ \row -> do
      let value = Massiv.index' datas $ Massiv.Ix2 (fromIntegral row) $ fromIntegral col
      Except.liftEither $ first (fmap (CellParserError col oid format row value)) $
        Cell.parseCell cell value

  for [0 .. numRows - 1] (Reader.runReaderT runner)

spec :: Spec
spec =
  describe "Row" $ do
    it "works as an Applicative" $
      runMatrixRow [] [] [[], []] (pure ())
        `shouldBe` Right [(), ()]

    it "works on single columns" $
      runMatrixRow
        [(invalidOid, Text)]
        []
        [["Hello"], ["World"]]
        (Row.columnWith Column.raw)
        `shouldBe` Right ["Hello", "World"]

    it "works on multiple columns" $
      runMatrixRow
        [(invalidOid, Text), (invalidOid, Text)]
        []
        [["1", "Hello"], ["2", "World"]]
        (replicate <$> Row.column <*> Row.columnWith Column.raw)
        `shouldBe` Right [["Hello"], ["World", "World"]]

    it "resolves named columns" $
      runMatrixRow
        [(invalidOid, Text), (invalidOid, Text)]
        [("b", 1)]
        [["1", "Hello"], ["2", "World"]]
        (Row.namedColumnWith "b" Column.raw)
        `shouldBe` Right ["Hello", "World"]

    it "works with fixed columns" $
      runMatrixRow
        [(invalidOid, Text), (invalidOid, Text)]
        []
        [["1", "Hello"], ["2", "World"]]
        (Row.fixedColumnWith 1 Column.raw)
        `shouldBe` Right ["Hello", "World"]

    it "fails on too few columns" $
      runMatrixRow
        [(invalidOid, Text)]
        []
        [["Hello"]]
        (Row.fixedColumnWith 1 Column.raw)
        `shouldBe` Left [NotEnoughColumns 1 1]

    it "fails on unknown named column" $
      runMatrixRow
        [(invalidOid, Text)]
        []
        [["Hello"]]
        (Row.namedColumnWith "unknown" Column.raw)
        `shouldBe` Left [MissingNamedColumn "unknown"]
