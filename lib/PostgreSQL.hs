{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | PostgreSQL client
--
-- Have a look at the individual sub-sections to find out more about the respective concepts.
--
module PostgreSQL
  ( -- * Templates and statements
    -- $templatesAndStatements

    -- ** Templates
    -- $templatesAndStatements_templates
    Statement.Template
  , Statement.renderTemplate

    -- *** Quasi-quotation
  , Statement.tpl

    -- *** Combinators
    -- $templatesAndStatements_templates_combinators
  , Statement.code
  , Statement.identifier
  , Statement.string
  , Statement.param
  , Statement.paramWith
  , Statement.constant

    -- ** Statements
    -- $templatesAndStatements_statements
  , Statement.Statement
  , Statement.stmt

    -- ** Prepared statements
    -- $templatesAndStatements_prepadeStatements
  , Statement.PreparedStatement

    -- * Query execution
    -- $queryExecution

    -- ** Combinators
  , Query.execute
  , Query.execute_
  , Query.query
  , Query.queryWith
  , Query.withPreparedStatement

    -- ** Evaluation
  , Query.Query
  , Class.RunQuery (..)
  , Class.runQueryThrow

    -- ** Interpreters
  , ConnectionPool.ConnectionPoolT
  , ConnectionPool.runConnectionPoolT
  , ConnectionPool.defaultConnectionPoolSettings

    -- * Result processing
    -- ** Top level
  , Result.Result
  , Result.ignored
  , Result.single
  , Result.first
  , Result.many
  , Result.affectedRows

    -- ** Row level
  , Row.Row
  , Row.column
  , Row.columnWith
  , Row.fixedColumn
  , Row.fixedColumnWith
  , Row.namedColumn
  , Row.namedColumnWith
  , Row.AutoRow (..)
  , Row.AutoColumnDelegate
  , Row.genericRow
  , Row.Named (..)
  , Row.Fixed (..)

    -- ** Column level
  , Column.Column
  , Column.AutoColumn (..)
  , Column.Readable (..)

    -- * Errors
  , Types.Error (..)
  , Types.Errors
  , Types.ProcessorError (..)
  , Types.ProcessorErrors
  , Types.ResultError (..)
  , Types.ResultErrors
  , Types.ParserError (..)
  , Types.ParserErrors

    -- * Common types
  , Types.Format (..)
  , Types.Oid
  , Types.ColumnNum
  , Types.RowNum
  , Types.Value (..)
  , Column.RawValue (..)
  )
where

import qualified PostgreSQL.Class as Class
import qualified PostgreSQL.ConnectionPool as ConnectionPool
import qualified PostgreSQL.Query as Query
import qualified PostgreSQL.Result as Result
import qualified PostgreSQL.Result.Column as Column
import qualified PostgreSQL.Result.Row as Row
import qualified PostgreSQL.Statement as Statement
import qualified PostgreSQL.Types as Types

-- $templatesAndStatements
--
-- Writing a statement usually involves writing it as a 'Statement.Template' and then rendering it
-- as a 'Statement.Statement'. The latter can optionally be prepared to become a
-- 'Statement.PreparedStatement'.
--
-- Templates and statements can take an input. The type of that input is determined by the type
-- parameter that 'Statement.Template', 'Statement.Statement' and 'Statement.PreparedStatement
-- expose.
--

-- $templatesAndStatements_templates
--
-- Templates can be constructed using the 'Statement.tpl' quasi-quoter or manually using the
-- provided combinators.
--

-- $templatesAndStatements_templates_combinators
--
-- 'Statement.Template' implements 'Semigroup' and 'Monoid'. These can be used to compose the
-- following combinators.
--
-- It also supports 'IsString' which helps to create templates from string literals.
--
-- You may use overloaded labels to refer to parameters.
--
-- prop> #my_param === param my_param
--
-- @my_param@ shall be a record field of data type.
--

-- $templatesAndStatements_statements
--
-- 'Statement.Statement's are created using the 'Statement.stmt' quasi-quoter or by rendering a
-- 'Statement.Template' via 'Statement.renderTemplate'.
--

-- $templatesAndStatements_prepadeStatements
--
-- 'Statement.PreparedStatement's can be obtained using 'Query.withPreparedStatement'.
--
-- This can be useful when executing a statement repeatedly and you want to save some time on
-- parsing and type-checking the statement on the database server.
--

-- $queryExecution
--
-- Queries are built using any type @query@ that satisfies @'Query.Query' query@. The combinators
-- below are used to run or prepare statements.
--
-- In most cases you will use 'Class.runQuery' or 'Class.runQueryThrow' to then actually run the
-- query.
--
-- 'ConnectionPool.ConnectionPoolT' is a useful interpreter for the 'Class.RunQuery' effect.
--
