{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

module PostgreSQL.Result
  ( Result
  , runResultPq

    -- * Combinators
  , ignored
  , single
  , first
  , many
  , affectedRows

    -- * Validation
  , checkForError
  )
where

import           Control.Monad (when)
import qualified Control.Monad.Except as Except
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import           Data.Foldable (for_)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8')
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgreSQL.Result.Row as Row
import qualified PostgreSQL.Types as Types
import           Text.Read (readEither)

data ResultF a where
  IgnoreResult :: ResultF ()

  SingleRow :: Row.Row a -> ResultF a

  FirstRow :: Row.Row a -> ResultF a

  ManyRows :: Row.Row a -> ResultF (Vector.Vector a)

  AffectedRows :: ResultF Integer

-- | Query result
--
-- @since 0.0.0
data Result a where
  Result :: (b -> a) -> ResultF b -> Result a

-- | @since 0.0.0
instance Functor Result where
  fmap f (Result g inner) = Result (f . g) inner

  {-# INLINE fmap #-}

-- | Process libpq's 'PQ.Result'.
--
-- @since 0.0.0
runResultPq :: MonadIO m => PQ.Result -> Result a -> m (Either Types.Errors a)
runResultPq result (Result f basis) = Except.runExceptT $ fmap f $
  case basis of
    IgnoreResult ->
      pure ()

    SingleRow row -> do
      rows <- liftIO (PQ.ntuples result)
      when (rows /= 1) $
        Except.throwError [Types.ErrorDuringValidation $ Types.MultipleRows $ Types.RowNum rows]
      runRow <- importErrors (Row.runRowPq result row)
      importErrors (runRow 0)

    FirstRow row -> do
      rows <- liftIO (PQ.ntuples result)
      when (rows < 1) $ Except.throwError [Types.ErrorDuringValidation Types.NoRows]
      runRow <- importErrors (Row.runRowPq result row)
      importErrors (runRow 0)

    ManyRows row -> do
      runRow <- importErrors (Row.runRowPq result row)
      PQ.Row rows <- liftIO (PQ.ntuples result)
      Vector.generateM (fromIntegral rows) (importErrors . runRow . fromIntegral)

    AffectedRows -> do
      tuples <- liftIO (PQ.cmdTuples result)
      case tuples of
        Nothing -> pure 0
        Just tuples ->
          Except.liftEither
          $ Bifunctor.first
            (pure . Types.ErrorDuringValidation . Types.FailedToParseAffectedRows . Text.pack)
          $ do
            tuples <- Bifunctor.first show (decodeUtf8' tuples)
            readEither (Text.unpack tuples)
  where
    importErrors = Except.withExceptT (fmap Types.ErrorDuringProcessing)


-- | Ignore the result set.
--
-- @since 0.0.0
ignored :: Result ()
ignored = Result id IgnoreResult

-- | Process exactly 1 row.
--
-- @since 0.0.0
single :: Row.Row a -> Result a
single row = Result id (SingleRow row)

-- | Process only the first row. There may be more rows in the result set, but they won't be
-- touched.
--
-- @since 0.0.0
first :: Row.Row a -> Result a
first row = Result id (FirstRow row)

-- | Process 0 or more rows.
--
-- @since 0.0.0
many :: Row.Row a -> Result (Vector.Vector a)
many row = Result id (ManyRows row)


-- | Get the number of affected rows.
--
-- @since 0.0.0
affectedRows :: Result Integer
affectedRows = Result id AffectedRows

---

-- | Check the result, if any, and the connection for errors.
--
-- @since 0.0.0
checkForError
  :: (MonadIO m, Except.MonadError Types.ResultErrors m)
  => Types.Connection
  -> Maybe PQ.Result
  -> m PQ.Result
checkForError conn mbResult = do
  result <-
    case mbResult of
      Nothing -> do
        connError <- liftIO $ PQ.errorMessage conn
        Except.throwError [Types.BadResultStatus (fromMaybe mempty connError)]

      Just result -> pure result

  resultError <- liftIO $ PQ.resultErrorMessage result
  for_ resultError $ \error ->
    when (ByteString.length error > 0) $
      Except.throwError [Types.BadResultStatus error]

  pure result

{-# INLINE checkForError #-}
