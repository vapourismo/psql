{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PostgreSQL.Column
  ( -- * Parser
    Parser (..)
  , ParserError (..)
  , ParserErrors

    -- ** Basic parsers
  , readableParser
  , ignoringParser
  , textParser

    -- ** Parser validators
  , onlyTextualParser
  , validateParser

    -- * Class
  , ColumnResult (..)

    -- * Helpers
  , ParseViaRead (..)
  , RawValue (..)
  )
where

import           Data.Coerce (coerce)
import           Data.Functor.Alt (Alt (..))
import           Data.Text (Text)
import qualified Database.PostgreSQL.LibPQ as PQ
import           Numeric.Natural (Natural)
import qualified PostgreSQL.Result.Cell as Cell
import           PostgreSQL.Types (ParserError (..), ParserErrors, Value)

-- | Parser for a result column
newtype Parser a = Parser
  { runParser
    :: PQ.Oid -- OID of the column type
    -> PQ.Format -- Format in which the cells of this column will appear
    -> Either ParserErrors (Cell.Parser a)
  }
  deriving stock Functor

instance Alt Parser where
  Parser lhs <!> Parser rhs = Parser $ \typ format ->
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

-- | Pass through cell parser that does nothing to validate the column beforehand.
unchecked :: Cell.Parser a -> Parser a
unchecked parser = Parser $ \_ _ -> Right parser

{-# INLINE unchecked #-}

-- | Only allow 'PQ.Text' format.
onlyTextualParser :: Parser a -> Parser a
onlyTextualParser (Parser run) = Parser $ \oid format ->
  case format of
    PQ.Binary -> Left [UnsupportedFormat format]
    PQ.Text -> run oid format

{-# INLINE onlyTextualParser #-}

-- | Validate the result of a parser.
validateParser :: Parser a -> (a -> Either Text b) -> Parser b
validateParser (Parser run) f = Parser $ \oid fmt -> do
  parser <- run oid fmt
  pure (Cell.validateParser parser f)

-- | Parse anything using its 'Read' instance. Only supports textual format and rejects @NULL@
-- values.
readableParser :: Read a => Parser a
readableParser = onlyTextualParser $ unchecked Cell.readableParser

{-# INLINE readableParser #-}

-- | Don't parse the column.
ignoringParser :: Parser ()
ignoringParser = unchecked Cell.ignoringParser

-- | Parse as UTF-8 'Text'.
textParser :: Parser Text
textParser = onlyTextualParser $ unchecked Cell.textParser

-- | Can parse a column in the 'PQ.Result'
class ColumnResult a where
  columnParser :: Parser a

instance ColumnResult () where
  columnParser = ignoringParser

instance ColumnResult Int where
  columnParser = readableParser

instance ColumnResult Word where
  columnParser = readableParser

instance ColumnResult Integer where
  columnParser = readableParser

instance ColumnResult Natural where
  columnParser = readableParser

instance ColumnResult Float where
  columnParser = readableParser

instance ColumnResult Double where
  columnParser = readableParser

instance ColumnResult PQ.Oid where
  columnParser = PQ.Oid <$> readableParser

instance ColumnResult Text where
  columnParser = textParser

instance (ColumnResult a, ColumnResult b) => ColumnResult (Either a b) where
  columnParser = (Left <$> columnParser) <!> (Right <$> columnParser)

-- | Provides a 'ColumnResult' instance using the 'Read' for @a@
newtype ParseViaRead a = ParseViaRead a

instance Read a => ColumnResult (ParseViaRead a) where
  columnParser = coerce $ readableParser @a

-- | The raw cell value
data RawValue = RawValue
  { rawValue_type :: PQ.Oid
  , rawValue_format :: PQ.Format
  , rawValue_value :: Value
  }
  deriving stock (Show, Eq, Ord)

instance ColumnResult RawValue where
  columnParser = Parser $ \oid format ->
    Right $ Cell.Parser $ Right . RawValue oid format
