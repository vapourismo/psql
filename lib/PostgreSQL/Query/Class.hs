{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module PostgreSQL.Query.Class
  ( Query (..)
  , Executable (..)
  )
where

import           Data.Kind (Type)
import qualified PostgreSQL.Result as Result
import qualified PostgreSQL.Statement as Statement

-- | PostgreSQL query
--
-- @since 0.0.0
class Monad query => Query query where
  -- | Query result
  --
  -- @since 0.0.0
  type Result query :: Type

  -- | Execute a statement.
  --
  -- @since 0.0.0
  executeStatement
    :: Statement.Statement a
    -- ^ Statement
    -> a
    -- ^ Statement input
    -> query (Result query)

  -- | Execute a previously prepared statement.
  --
  -- @since 0.0.0
  executePreparedStatement
    :: Statement.PreparedStatement a
    -- ^ Prepared statement
    -> a
    -- ^ Statement input
    -> query (Result query)

  -- | Prepare a statement. The prepared statement is only valid within the provided continuation.
  --
  -- @since 0.0.0
  withPreparedStatement
    :: Statement.Statement a
    -- ^ Statement to prepare
    -> (Statement.PreparedStatement a -> query r)
    -- ^ Scope within which the statement may be used
    -> query r

  -- | Process the result object.
  --
  -- @since 0.0.0
  processResult
    :: Result query
    -- ^ Result
    -> Result.Result a
    -- ^ Result processor
    -> query a

-- | @statement@ is an executable statement.
--
-- @since 0.0.0
class Executable statement where
  -- | Execute a statement.
  --
  -- @since 0.0.0
  execute
    :: Query query
    => statement param
    -- ^ Statement
    -> param
    -- ^ Statement input
    -> query (Result query)

-- | @since 0.0.0
instance Executable Statement.Statement where
  execute = executeStatement

  {-# INLINE execute #-}

-- | @since 0.0.0
instance Executable Statement.PreparedStatement where
  execute = executePreparedStatement

  {-# INLINE execute #-}
