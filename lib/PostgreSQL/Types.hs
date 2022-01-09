{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module PostgreSQL.Types
  ( Value (..)
  , RegType (..)
  , PackedParam (..)
  , PackedParamPrepared (..)
  , ParserError (..)
  , ParserErrors
  , ProcessorError (..)
  , ProcessorErrors
  , ResultError (..)
  , ResultErrors
  , Error (..)
  , Errors
  , ColumnNum (..)
  , RowNum (..)

    -- * Re-exports
  , PQ.Format (..)
  , PQ.Oid (..)
  , PQ.Connection
  )
where

import           Control.Exception (Exception)
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty)
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Database.PostgreSQL.LibPQ as PQ
import           Foreign.C.Types (CInt)

-- | Value
--
-- @since 0.0.0
data Value
  = Null
  | Value ByteString
  deriving stock
    ( Show -- ^ @since 0.0.0
    , Eq -- ^ @since 0.0.0
    , Ord -- ^ @since 0.0.0
    )

-- | @since 0.0.0
instance IsString Value where
  fromString = Value . fromString

-- | Postgre's regtype
--
-- @since 0.0.0
newtype RegType = RegType
  { unRegType :: Text }
  deriving newtype
    ( Show -- ^ @since 0.0.0
    , Read -- ^ @since 0.0.0
    , Eq -- ^ @since 0.0.0
    , Ord -- ^ @since 0.0.0
    , IsString -- ^ @since 0.0.0
    )

-- | Packed parameter
--
-- @since 0.0.0
newtype PackedParam = PackedParam (Maybe (PQ.Oid, ByteString, PQ.Format))
  deriving newtype Show -- ^ @since 0.0.0

-- | Packed parameter for a prepared query
--
-- @since 0.0.0
newtype PackedParamPrepared = PackedParamPrepared (Maybe (ByteString, PQ.Format))
  deriving newtype Show -- ^ @since 0.0.0

-- | Error that occurs when parsing a column
--
-- @since 0.0.0
data ParserError
  = UnsupportedFormat PQ.Format
  | UnsupportedOid PQ.Oid
  deriving stock
    ( Show -- ^ @since 0.0.0
    , Eq -- ^ @since 0.0.0
    , Ord -- ^ @since 0.0.0
    )

type ParserErrors = NonEmpty ParserError

-- | Error that may occur during processing
--
-- @since 0.0.0
data ProcessorError
  = ColumnParserError
    { processorError_column :: ColumnNum
    , processorError_type :: PQ.Oid
    , processorError_format :: PQ.Format
    , processorError_columnError :: ParserError
    }
  | CellParserError
    { processorError_column :: ColumnNum
    , processorError_type :: PQ.Oid
    , processorError_format :: PQ.Format
    , processorError_row :: RowNum
    , processorError_value :: Value
    , processorError_cellError :: Text
    }
  | NotEnoughColumns
    { processorError_wantedColumns :: ColumnNum
    , processorError_haveColumns :: ColumnNum
    }
  | MissingNamedColumn
    { processorError_wantedColumnName :: ByteString
    }
  deriving stock
    ( Show -- ^ @since 0.0.0
    , Eq -- ^ @since 0.0.0
    , Ord -- ^ @since 0.0.0
    )

type ProcessorErrors = NonEmpty ProcessorError

-- | Error that occurs when validating the result
--
-- @since 0.0.0
data ResultError
  = BadResultStatus
    { resultError_status :: ByteString }
  | NoRows
  | MultipleRows
    { resultError_numRows :: RowNum }
  | FailedToParseAffectedRows
    { resultError_message :: Text }
  deriving stock
    ( Show -- ^ @since 0.0.0
    , Eq -- ^ @since 0.0.0
    , Ord -- ^ @since 0.0.0
    )


type ResultErrors = NonEmpty ResultError

-- | @since 0.0.0
data Error
  = ErrorDuringProcessing ProcessorError
  -- ^ Occurs when processing the result table
  | ErrorDuringValidation ResultError
  -- ^ Occurs when validating the result object
  deriving stock
    ( Show -- ^ @since 0.0.0
    , Eq -- ^ @since 0.0.0
    , Ord -- ^ @since 0.0.0
    )
  deriving anyclass Exception -- ^ @since 0.0.0

type Errors = NonEmpty Error

-- | Numberic column identifier
--
-- @since 0.0.0
newtype ColumnNum = ColumnNum
  { fromColumnNum :: PQ.Column }
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Num, Integral, Real) via CInt

-- | Numberic row identifier
--
-- @since 0.0.0
newtype RowNum = RowNum
  { fromRowNum :: PQ.Row }
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Num, Integral, Real) via CInt
