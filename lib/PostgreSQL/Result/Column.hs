{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Exports of this module are concerned with columns in a Postgres query result. This includes
-- validation of type and format. Parsing of the actual cell values in a column is delegated to
-- "PostgreSQL.Result.Cell".
module PostgreSQL.Result.Column
  ( -- * Column
    Column (..)

    -- ** Basics
  , ignored
  , raw
  , text
  , readable

    -- ** Helpful combinators
  , unchecked
  , validate
  , onlyTextual
  , onlyBinary

    -- * Class
  , AutoColumn (..)

    -- * Errors
  , ParserError (..)
  , ParserErrors

    -- * Helpers
  , Readable (..)
  , RawValue (..)
  )
where

import           Data.ByteString (ByteString)
import           Data.Coerce (coerce)
import           Data.Functor.Alt (Alt (..))
import           Data.Text (Text)
import qualified Database.PostgreSQL.LibPQ as PQ
import           Numeric.Natural (Natural)
import qualified PostgreSQL.Result.Cell as Cell
import           PostgreSQL.Types (ParserError (..), ParserErrors, Value)

-- | Result column parser
--
-- @since 0.0.0
newtype Column a = Column
  { parseColumn
      :: PQ.Oid -- OID of the column type
      -> PQ.Format -- Format in which the cells of this column will appear
      -> Either ParserErrors (Cell.Cell a)
  }
  deriving stock Functor -- ^ @since 0.0.0

-- | @since 0.0.0
instance Alt Column where
  Column lhs <!> Column rhs = Column $ \typ format ->
    case (lhs typ format, rhs typ format) of
      (Right lhsParser, Right rhsParser) ->
        -- Both parsers at the column level succeeded. This means we must pass the alternation down
        -- to the cell-level parser.
        Right (lhsParser <!> rhsParser)

      (Left lhsErrors, Left rhsErrors) ->
        -- Both have failed, therefore we must combine the errors.
        Left (lhsErrors <> rhsErrors)

      (lhs, rhs) ->
        -- At this point we know that exactly one parser at the column level has failed.
        lhs <!> rhs

  {-# INLINE (<!>) #-}

-- | Lift a cell parser. This does perform any validation on column type or format.
--
-- @since 0.0.0
unchecked :: Cell.Cell a -> Column a
unchecked parser = Column $ \_ _ -> Right parser

{-# INLINE unchecked #-}

-- | Only allow textual format.
--
-- @since 0.0.0
onlyTextual :: Column a -> Column a
onlyTextual (Column run) = Column $ \oid format ->
  case format of
    PQ.Binary -> Left [UnsupportedFormat format]
    PQ.Text -> run oid format

{-# INLINE onlyTextual #-}

-- | Only allow binary format.
--
-- @since 0.0.0
onlyBinary :: Column a -> Column a
onlyBinary (Column run) = Column $ \oid format ->
  case format of
    PQ.Text -> Left [UnsupportedFormat format]
    PQ.Binary -> run oid format

{-# INLINE onlyBinary #-}

-- | Validate the result of a cell parser.
--
-- @since 0.0.0
validate :: Column a -> (a -> Either Text b) -> Column b
validate (Column run) f = Column $ \oid fmt -> do
  parser <- run oid fmt
  pure (Cell.validate parser f)

{-# INLINE validate #-}

-- | Don't parse the column.
--
-- @since 0.0.0
ignored :: Column ()
ignored = unchecked Cell.ignored

{-# INLINE ignored #-}

-- | Raw value. Rejects @NULL@.
--
-- @since 0.0.0
raw :: Column ByteString
raw = unchecked Cell.raw

{-# INLINE raw #-}

-- | Parse as UTF-8 'Text'. See 'Cell.text'.
--
-- @since 0.0.0
text :: Column Text
text = onlyTextual (unchecked Cell.text)

{-# INLINE text #-}

-- | Parse something using its 'Read' instance. Only supports textual format. See 'Cell.readable'.
--
-- @since 0.0.0
readable :: Read a => Column a
readable = onlyTextual (unchecked Cell.readable)

{-# INLINE readable #-}

-- | Default column parser for a type
--
-- @since 0.0.0
class AutoColumn a where
  autoColumn :: Column a

-- | @since 0.0.0
instance AutoColumn () where
  autoColumn = ignored

  {-# INLINE autoColumn #-}

-- | @since 0.0.0
instance AutoColumn Int where
  autoColumn = readable

  {-# INLINE autoColumn #-}

-- | @since 0.0.0
instance AutoColumn Word where
  autoColumn = readable

  {-# INLINE autoColumn #-}

-- | @since 0.0.0
instance AutoColumn Integer where
  autoColumn = readable

  {-# INLINE autoColumn #-}

-- | @since 0.0.0
instance AutoColumn Natural where
  autoColumn = readable

  {-# INLINE autoColumn #-}

-- | @since 0.0.0
instance AutoColumn Float where
  autoColumn = readable

  {-# INLINE autoColumn #-}

-- | @since 0.0.0
instance AutoColumn Double where
  autoColumn = readable

  {-# INLINE autoColumn #-}

-- | @since 0.0.0
instance AutoColumn PQ.Oid where
  autoColumn = PQ.Oid <$> readable

  {-# INLINE autoColumn #-}

-- | @since 0.0.0
instance AutoColumn Text where
  autoColumn = text

  {-# INLINE autoColumn #-}

-- | @since 0.0.0
instance (AutoColumn a, AutoColumn b) => AutoColumn (Either a b) where
  autoColumn = fmap Left autoColumn <!> fmap Right autoColumn

  {-# INLINE autoColumn #-}

-- | Provides a 'AutoColumn' instance using the 'Read' for @a@
--
-- @since 0.0.0
newtype Readable a = Readable a

-- | @since 0.0.0
instance Read a => AutoColumn (Readable a) where
  autoColumn = coerce $ readable @a

  {-# INLINE autoColumn #-}

-- | The raw cell value
--
-- @since 0.0.0
data RawValue = RawValue
  { rawValue_type :: PQ.Oid
  , rawValue_format :: PQ.Format
  , rawValue_value :: Value
  }
  deriving stock (Show, Eq, Ord)

-- | @since 0.0.0
instance AutoColumn RawValue where
  autoColumn = Column $ \oid format ->
    Right $ Cell.Cell $ Right . RawValue oid format

  {-# INLINE autoColumn #-}
