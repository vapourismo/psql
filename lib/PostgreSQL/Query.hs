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
  , queryCustom
  , queryWith
  , queryCustomWith

    -- * Statement preparation
  , Class.withPreparedStatement

    -- * Results
  , QueryResult (..)
  , genericQueryProcessor

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
import           Data.Functor.Identity (Identity (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as Vector
import           Data.Void (Void)
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified GHC.Generics as Generics
import qualified PostgreSQL.Param as Param
import qualified PostgreSQL.Query.Class as Class
import qualified PostgreSQL.Result as Result
import qualified PostgreSQL.Result.Column as Column
import qualified PostgreSQL.Statement as Statement
import           PostgreSQL.Types (Assembler, Error (..), Errors, RowNum)

---

-- | Like 'execute' but does not concern itself with the result handle.
execute_
  :: (Class.Executable statement, Class.Query query)
  => statement a
  -- ^ Statement
  -> a
  -- ^ Statement input
  -> query ()
execute_ statement param =
  void (Class.execute statement param)

{-# INLINE execute_ #-}

buildVector
  :: Monad m
  => RowNum
  -> (RowNum -> m a)
  -> m (Vector.Vector a)
buildVector row mkRow =
  Vector.generateM (fromIntegral row) (mkRow . fromIntegral)

{-# INLINE buildVector #-}

-- | Perform a parameterized query.
query
  :: (Class.Executable statement, Class.Query query, QueryResult row)
  => statement param
  -- ^ Query statement
  -> param
  -- ^ Query parameter
  -> query (Vector.Vector row)
query statement input =
  queryWith statement input queryProcessor

{-# INLINE query #-}

-- | Perform a parameterized query. This also lets you specify the row processor explicitly.
queryWith
  :: (Class.Executable statement, Class.Query query)
  => statement param
  -- ^ Query statement
  -> param
  -- ^ Query parameter
  -> Result.Processor row
  -- ^ Result row processor
  -> query (Vector.Vector row)
queryWith statement input resultProcessor =
  queryCustomWith statement input resultProcessor buildVector

{-# INLINE queryWith #-}

-- | Perform a parameterized query with a custom function to build the result type.
queryCustom
  :: (Class.Executable statement, Class.Query query, QueryResult row)
  => statement param
  -- ^ Query statement
  -> param
  -- ^ Query parameter
  -> Assembler row result
  -- ^ Given a number of rows and a way to fetch each row, assemble the result.
  -> query result
queryCustom statement input =
  queryCustomWith statement input queryProcessor

{-# INLINE queryCustom #-}

-- | Perform a parameterized query with a custom function to build the result type. This also lets
-- you specify the row processor explicitly.
queryCustomWith
  :: (Class.Executable statement, Class.Query query)
  => statement param
  -- ^ Query statement
  -> param
  -- ^ Query parameter
  -> Result.Processor row
  -- ^ Result row processor
  -> Assembler row result
  -- ^ Given a number of rows and a way to fetch each row, assemble the result.
  -> query result
queryCustomWith statement input resultProcessor f = do
  result <- Class.execute statement input
  Class.processResult result resultProcessor f

{-# INLINE queryCustomWith #-}

---

-- | @a@ is the result of a query
class QueryResult a where
  queryProcessor :: Result.Processor a

  default queryProcessor
    :: (Generics.Generic a, QueryResult (Generics.Rep a Void))
    => Result.Processor a
  queryProcessor =
    genericQueryProcessor

  {-# INLINE queryProcessor #-}

-- | Does process anything.
instance QueryResult () where
  queryProcessor = pure ()

instance Column.AutoColumn a => QueryResult (Identity a) where
  queryProcessor = fmap Identity Result.column

  {-# INLINE queryProcessor #-}

instance
  ( Column.AutoColumn a
  , Column.AutoColumn b
  )
  => QueryResult (a, b)

instance
  ( Column.AutoColumn a
  , Column.AutoColumn b
  , Column.AutoColumn c
  )
  => QueryResult (a, b, c)

instance
  ( Column.AutoColumn a
  , Column.AutoColumn b
  , Column.AutoColumn c
  , Column.AutoColumn d
  )
  => QueryResult (a, b, c, d)

instance
  ( Column.AutoColumn a
  , Column.AutoColumn b
  , Column.AutoColumn c
  , Column.AutoColumn d
  , Column.AutoColumn e
  )
  => QueryResult (a, b, c, d, e)

instance
  ( Column.AutoColumn a
  , Column.AutoColumn b
  , Column.AutoColumn c
  , Column.AutoColumn d
  , Column.AutoColumn e
  , Column.AutoColumn f
  )
  => QueryResult (a, b, c, d, e, f)

instance
  ( Column.AutoColumn a
  , Column.AutoColumn b
  , Column.AutoColumn c
  , Column.AutoColumn d
  , Column.AutoColumn e
  , Column.AutoColumn f
  , Column.AutoColumn g
  )
  => QueryResult (a, b, c, d, e, f, g)

---

instance Column.AutoColumn a => QueryResult (Generics.K1 tag a x) where
  queryProcessor = Generics.K1 <$> Result.column

  {-# INLINE queryProcessor #-}

instance QueryResult (f x) => QueryResult (Generics.M1 tag meta f x) where
  queryProcessor = Generics.M1 <$> queryProcessor

  {-# INLINE queryProcessor #-}

instance (QueryResult (lhs x), QueryResult (rhs x)) => QueryResult ((Generics.:*:) lhs rhs x) where
  queryProcessor = (Generics.:*:) <$> queryProcessor <*> queryProcessor

  {-# INLINE queryProcessor #-}

-- | 'QueryResult' implementation for @a@ using GHC generics
--
-- Note, this only supports product types. Fields of the product type will effectively be combined
-- with 'Result.column' from left to right.
--
-- Consider the following type.
--
-- > data MyFoo = MyFoo Int String
--
-- The 'Result.Processor' generated from 'genericQueryProcessor' is identical to this:
--
-- > MyFoo <$> column <*> column
--
--
genericQueryProcessor
  :: (Generics.Generic a, QueryResult (Generics.Rep a Void))
  => Result.Processor a
genericQueryProcessor =
  Generics.to @_ @Void <$> queryProcessor

{-# INLINE genericQueryProcessor #-}

---

-- | Interpreter for 'Class.Query'
newtype QueryT m a = QueryT
  { unQueryT :: Reader.ReaderT PQ.Connection (Except.ExceptT Errors m) a }
  deriving newtype
    ( Functor
    , Apply
    , Applicative
    , Monad
    , MonadIO
    , MonadState s
    , MonadWriter s
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

instance Monad m => Alt (QueryT m) where
  QueryT lhs <!> QueryT rhs = QueryT $ lhs <!> rhs

  {-# INLINE (<!>) #-}

instance Monad m => Bind (QueryT m) where
  QueryT x >>- f = QueryT (x >>- unQueryT . f)

  {-# INLINE (>>-) #-}

instance MonadTrans QueryT where
  lift = QueryT . lift . lift

  {-# INLINE lift #-}

instance Except.MonadError e m => Except.MonadError e (QueryT m) where
  throwError = QueryT . lift . lift . Except.throwError

  {-# INLINE throwError #-}

  catchError action handle = QueryT
    $ Reader.ReaderT
    $ \conn -> Except.ExceptT
    $ Except.catchError (runQueryT conn action)
    $ runQueryT conn .  handle

  {-# INLINE catchError #-}

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
  type Result (QueryT m) = PQ.Result

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

  processResult result processor f = QueryT
    $ Reader.lift
    $ Except.withExceptT (fmap ErrorDuringProcessing)
    $ Result.runResultT result
    $ Result.runProcessor processor f

  {-# INLINE processResult #-}

-- | Run an interaction with a PostgreSQL database.
runQueryT
  :: PQ.Connection
  -> QueryT m a
  -> m (Either Errors a)
runQueryT conn (QueryT action) =
  Except.runExceptT $ Reader.runReaderT action conn

{-# INLINE runQueryT #-}

-- | Like 'runQueryT' but throw on error instead.
runQueryTThrow
  :: MonadThrow m
  => PQ.Connection
  -> QueryT m a
  -> m a
runQueryTThrow conn query = do
  result <- runQueryT conn query
  either (throwM . NonEmpty.head) pure result

{-# INLINE runQueryTThrow #-}
