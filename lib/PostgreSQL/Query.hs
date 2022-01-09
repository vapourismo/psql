{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module PostgreSQL.Query
  ( -- * Query execution
    Class.execute
  , execute_
  , query
  , queryWith

    -- * Statement preparation
  , Class.withPreparedStatement

    -- * Classes
  , Class.Query

    -- * Interpreters
  , QueryT
  , runQueryT
  , runQueryTThrow
  )
where

import           Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow (throwM), bracket)
import qualified Control.Monad.Except as Except
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Reader as Reader
import           Control.Monad.State.Class (MonadState)
import           Control.Monad.Trans (MonadTrans (lift))
import           Control.Monad.Writer.Class (MonadWriter)
import           Data.Coerce (coerce)
import           Data.Functor (void)
import           Data.Functor.Alt (Alt (..))
import           Data.Functor.Apply (Apply)
import           Data.Functor.Bind (Bind (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgreSQL.Param as Param
import qualified PostgreSQL.Query.Class as Class
import qualified PostgreSQL.Result as Result
import qualified PostgreSQL.Result.Row as Row
import qualified PostgreSQL.Statement as Statement
import           PostgreSQL.Types (Error (..), Errors)

---

-- | Like 'execute' but does not concern itself with the result handle.
--
-- @since 0.0.0
execute_
  :: (Class.Executable statement, Class.Query query)
  => statement param
  -- ^ Statement
  -> param
  -- ^ Statement input
  -> query ()
execute_ statement param =
  void (Class.execute statement param)

{-# INLINE execute_ #-}

-- | Perform a parameterized query.
--
-- @since 0.0.0
query
  :: (Class.Executable statement, Class.Query query, Row.AutoRow row)
  => statement param
  -- ^ Query statement
  -> param
  -- ^ Query parameter
  -> query (Vector.Vector row)
query statement input =
  queryWith statement input (Result.many Row.autoRow)

{-# INLINE query #-}

-- | Perform a parameterized query. This also lets you specify the result processor explicitly.
--
-- @since 0.0.0
queryWith
  :: (Class.Executable statement, Class.Query query)
  => statement param
  -- ^ Query statement
  -> param
  -- ^ Query parameter
  -> Result.Result row
  -- ^ Result row processor
  -> query row
queryWith statement input resultProcessor = do
  result <- Class.execute statement input
  Class.processResult result resultProcessor

{-# INLINE queryWith #-}

---

-- | Interpreter for 'Class.Query'
--
-- @since 0.0.0
newtype QueryT m a = QueryT
  { unQueryT :: Reader.ReaderT PQ.Connection (Except.ExceptT Errors m) a }
  deriving newtype
    ( Functor -- ^ @since 0.0.0
    , Apply -- ^ @since 0.0.0
    , Applicative -- ^ @since 0.0.0
    , Monad -- ^ @since 0.0.0
    , MonadIO -- ^ @since 0.0.0
    , MonadState s -- ^ @since 0.0.0
    , MonadWriter s -- ^ @since 0.0.0
    , Except.MonadError Errors -- ^ @since 0.0.0
    , MonadThrow -- ^ @since 0.0.0
    , MonadCatch -- ^ @since 0.0.0
    , MonadMask -- ^ @since 0.0.0
    )

-- | @since 0.0.0
instance Monad m => Alt (QueryT m) where
  QueryT lhs <!> QueryT rhs = QueryT $ lhs <!> rhs

  {-# INLINE (<!>) #-}

-- | @since 0.0.0
instance Monad m => Bind (QueryT m) where
  QueryT x >>- f = QueryT (x >>- unQueryT . f)

  {-# INLINE (>>-) #-}

-- | @since 0.0.0
instance MonadTrans QueryT where
  lift = QueryT . lift . lift

  {-# INLINE lift #-}

-- | @since 0.0.0
instance Reader.MonadReader r m => Reader.MonadReader r (QueryT m) where
  ask = QueryT $ lift $ lift Reader.ask

  {-# INLINE ask #-}

  local f (QueryT inner) = QueryT $
    Reader.mapReaderT (Except.mapExceptT (Reader.local f)) inner

  {-# INLINE local #-}

prepareStatement
  :: MonadIO m
  => Statement.Statement a
  -> QueryT m (Statement.PreparedStatement a)
prepareStatement statement = QueryT $ Reader.ReaderT $ \conn -> do
  let name = Statement.statement_name statement

  mbResult <- liftIO $
    PQ.prepare
      conn
      name
      (Statement.statement_code statement)
      (Just (Statement.statement_types statement))

  _result <- Except.withExceptT (fmap ErrorDuringValidation) $
    Result.checkForError conn mbResult

  pure Statement.PreparedStatement
    { Statement.preparedStatement_name = name
    , Statement.preparedStatement_mkParams =
      map Param.toPrepared . Statement.statement_mkParams statement
    }

{-# INLINE prepareStatement #-}

deallocatePreparedStatement
  :: (MonadIO m, MonadMask m)
  => Statement.PreparedStatement a
  -> QueryT m ()
deallocatePreparedStatement statement =
  execute_ [Statement.stmt| DEALLOCATE $(quotedName) |] ()
  where
    quotedName = Statement.identifier $ decodeUtf8 $ Statement.preparedStatement_name statement

instance (MonadIO m, MonadMask m) => Class.Query (QueryT m) where
  type NativeResult (QueryT m) = PQ.Result

  executeStatement statement input = QueryT $ Reader.ReaderT $ \conn -> do
    mbResult <- liftIO $ do
      let code = Statement.statement_code statement
      case Statement.statement_mkParams statement input of
        []     -> PQ.exec conn code
        params -> PQ.execParams conn code (coerce params) PQ.Text

    Except.withExceptT (fmap ErrorDuringValidation) $ Result.checkForError conn mbResult

  {-# INLINE executeStatement #-}

  withPreparedStatement statement =
    bracket (prepareStatement statement) deallocatePreparedStatement

  {-# INLINE withPreparedStatement #-}

  executePreparedStatement statement input = QueryT $ Reader.ReaderT $ \conn -> do
    mbResult <- liftIO $
      PQ.execPrepared
        conn
        (Statement.preparedStatement_name statement)
        (coerce (Statement.preparedStatement_mkParams statement input))
        PQ.Text

    Except.withExceptT (fmap ErrorDuringValidation) $ Result.checkForError conn mbResult

  {-# INLINE executePreparedStatement #-}

  processResult result processor = QueryT
    $ Reader.lift
    $ Except.ExceptT
    $ Result.runResultPq result processor

  {-# INLINE processResult #-}

-- | Run an interaction with a PostgreSQL database.
--
-- @since 0.0.0
runQueryT
  :: PQ.Connection
  -> QueryT m a
  -> m (Either Errors a)
runQueryT conn (QueryT action) =
  Except.runExceptT $ Reader.runReaderT action conn

{-# INLINE runQueryT #-}

-- | Like 'runQueryT' but throw on error instead.
--
-- @since 0.0.0
runQueryTThrow
  :: MonadThrow m
  => PQ.Connection
  -> QueryT m a
  -> m a
runQueryTThrow conn query = do
  result <- runQueryT conn query
  either (throwM . NonEmpty.head) pure result

{-# INLINE runQueryTThrow #-}
