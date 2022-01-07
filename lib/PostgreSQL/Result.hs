{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

module PostgreSQL.Result
  ( -- * Combinators
    Processor
  , column
  , columnWith
  , namedColumn
  , namedColumnWith

    -- * Evaluation
  , runProcessor
  , ProcessorError (..)
  , ProcessorErrors

    -- * Class
  , Class.HasResult (..)
  , ResultT (..)
  , runResultT

    -- * Validation
  , ResultError (..)
  , ResultErrors
  , checkForError
  )
where

import           Control.Applicative (liftA2)
import           Control.Monad (when)
import qualified Control.Monad.Except as Except
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State.Strict as State
import           Data.Bifunctor (first)
import qualified Data.ByteString as ByteString
import           Data.Foldable (for_)
import           Data.Functor.Alt (Alt (..))
import           Data.Maybe (fromMaybe)
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgreSQL.Result.Cell as Cell
import qualified PostgreSQL.Result.Class as Class
import qualified PostgreSQL.Result.Column as Column
import           PostgreSQL.Types (ProcessorError (..), ProcessorErrors, ResultError (..),
                                   ResultErrors, Value (..))

---

type ProcessorImpl m = (Except.MonadError ProcessorErrors m, Alt m, Class.HasResult m)

data ProcessorState = ProcessorState
  { processorState_nextColumn :: PQ.Column
  , processorState_numColumns :: PQ.Column
  }

-- | Result processor
--
-- Imagine you have an SQL query like @SELECT bar, baz FROM foo@. The query result is a table with
-- two columns: @bar@ and @baz@. Lets say @bar@ is of type @integer@ and @baz@ is of type @text@.
--
-- You can write the following 'Processor' to describe the result processor for the above query
-- result.
--
-- > data Foo = Foo Int Text
-- >
-- > result :: Processor Foo
-- > result = Foo <$> column <*> column
--
newtype Processor a = Processor
  { _unProcessor :: forall m. ProcessorImpl m => State.StateT ProcessorState m (m (PQ.Row -> m a)) }
  deriving stock Functor

instance Applicative Processor where
  pure x = Processor $ pure $ pure $ pure $ pure x

  {-# INLINE pure #-}

  Processor lhs <*> Processor rhs = Processor $
    liftA2 (liftA2 (liftA2 (<*>))) lhs rhs

  {-# INLINE (<*>) #-}

instance Alt Processor where
  Processor lhs <!> Processor rhs = Processor $
    liftA2 (<!>) lhs rhs

  {-# INLINE (<!>) #-}

-- | Validate the query result as much as possible and then hand off the return type assembly to the
-- given function.
runProcessor
  :: (Except.MonadError ProcessorErrors m, Alt m, Class.HasResult m)
  => Processor a
  -- ^ Result processor
  -> (PQ.Row -> (PQ.Row -> m a) -> m b)
  -- ^ This function is given the number of rows and an action to retrieve each row.
  -> m b
runProcessor (Processor parseColumn) return = do
  (parseRow, ProcessorState _ wantColumns) <- State.runStateT parseColumn (ProcessorState 0 0)

  columns <- Class.numColumns
  when (columns < wantColumns) $
    Except.throwError $ pure NotEnoughColumns
      { processorError_haveColumns = columns
      , processorError_wantedColumns = wantColumns
      }

  -- This part is purposely run after we've verified that enough columns are available.
  -- Otherwise we might run into errors with methods from 'Class.HasResult' that expect a certain
  -- number of columns to be there.
  parseRow <- parseRow

  rows <- Class.numRows
  return rows parseRow

{-# INLINE runProcessor #-}

-- | Create a row processor for a column.
makeRowProcessor
  :: ( Class.HasResult m
     , Except.MonadError ProcessorErrors m
     )
  => Column.Parser a
  -- ^ Column parser
  -> PQ.Column
  -- ^ Column to process
  -> m (PQ.Row -> m a)
makeRowProcessor (Column.Parser withColumn) column = do
  typ <- Class.columnType column
  format <- Class.columnFormat column

  Cell.Cell parseCell <-
    Except.liftEither $ first (fmap (ColumnParserError column typ format)) $ withColumn typ format

  pure $ \row -> do
    value <- Class.cellValue column row

    let toFieldParserErrors = fmap (CellParserError column typ format row value)
    Except.liftEither $ first toFieldParserErrors $ parseCell value

{-# INLINE makeRowProcessor #-}

-- | Process a column with the given parser.
columnWith
  :: Column.Parser a
  -- ^ Column parser
  -> Processor a
columnWith parser = Processor $ do
  column <- State.state $ \column ->
    let
      selectedColumn = processorState_nextColumn column
    in
      ( selectedColumn
      , ProcessorState
        { processorState_nextColumn = selectedColumn + 1
        , processorState_numColumns = max (selectedColumn + 1) (processorState_numColumns column)
        }
      )

  pure $ makeRowProcessor parser column

{-# INLINE columnWith #-}

-- | Process a column. This is equivalent to 'columnWith' in conjunction with 'Column.columnParser'.
column
  :: Column.ColumnResult a
  => Processor a
column =
  columnWith Column.columnParser

{-# INLINE column #-}

-- | Process a named column with the given parser. Named columns do not advance the current column
-- cursor - they don't interfere with 'column' and 'columnWith'.
namedColumnWith
  :: ByteString.ByteString
  -- ^ Column name
  -> Column.Parser a
  -- ^ Column parser
  -> Processor a
namedColumnWith columnName parser = Processor $ do
  column <- do
    mbColumn <- Class.columnFromName columnName
    case mbColumn of
      Just column -> pure column
      Nothing -> Except.throwError [MissingNamedColumn columnName]

  State.modify $ \procState -> procState
    { processorState_numColumns = max (column + 1) (processorState_numColumns procState) }

  pure $ makeRowProcessor parser column

{-# INLINE namedColumnWith #-}

-- | Process a column. Named columns do not advance the current column cursor - they don't interfere
-- with 'column' and 'columnWith'.
namedColumn
  :: Column.ColumnResult a
  => ByteString.ByteString
  -- ^ Column name
  -> Processor a
namedColumn columnName =
  namedColumnWith columnName Column.columnParser

{-# INLINE namedColumn #-}

---

-- | Implementation for 'Result.HasResult' that delegates to @postgresql-libpq@
newtype ResultT m a = ResultT
  { _unResultT :: Reader.ReaderT PQ.Result m a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , Except.MonadError e
    )

instance Alt m => Alt (ResultT m) where
  ResultT lhs <!> ResultT rhs = ResultT $ lhs <!> rhs

  {-# INLINE (<!>) #-}

instance MonadIO m => Class.HasResult (ResultT m) where
  numColumns = ResultT $ Reader.ReaderT $ \result ->
    liftIO $ PQ.nfields result

  {-# INLINE numColumns #-}

  numRows = ResultT $ Reader.ReaderT $ \result ->
    liftIO $ PQ.ntuples result

  {-# INLINE numRows #-}

  columnType col = ResultT $ Reader.ReaderT $ \result ->
    liftIO $ PQ.ftype result col

  {-# INLINE columnType #-}

  columnFormat col = ResultT $ Reader.ReaderT $ \result ->
    liftIO $ PQ.fformat result col

  {-# INLINE columnFormat #-}

  cellValue col row = ResultT $ Reader.ReaderT $ \result ->
    liftIO $ maybe Null Value <$> PQ.getvalue' result row col

  {-# INLINE cellValue #-}

  columnFromName name = ResultT $ Reader.ReaderT $ \result ->
    liftIO $ PQ.fnumber result name

  {-# INLINE columnFromName #-}

runResultT :: PQ.Result -> ResultT m a -> m a
runResultT result (ResultT action) = Reader.runReaderT action result

{-# INLINE runResultT #-}

---

-- | Check the result, if any, and the connection for errors.
checkForError
  :: (MonadIO m, Except.MonadError ResultErrors m)
  => PQ.Connection
  -> Maybe PQ.Result
  -> m PQ.Result
checkForError conn mbResult = do
  result <-
    case mbResult of
      Nothing -> do
        connError <- liftIO $ PQ.errorMessage conn
        Except.throwError [ResultError (fromMaybe mempty connError)]

      Just result -> pure result

  resultError <- liftIO $ PQ.resultErrorMessage result
  for_ resultError $ \error ->
    when (ByteString.length error > 0) $
      Except.throwError [ResultError error]

  pure result

{-# INLINE checkForError #-}
