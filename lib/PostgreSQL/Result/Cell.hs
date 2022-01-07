{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- The derived Alt instance for Cell causes this.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Things in this module are dedicated to parsing Postgres values independent of their format or
-- type.
module PostgreSQL.Result.Cell
  ( Cell (..)
  , ignored
  , raw
  , text
  , readable
  , validate
  )
where

import           Control.Monad ((>=>))
import           Control.Monad.Except (Except, ExceptT (..))
import           Control.Monad.Reader (ReaderT (..))
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import           Data.Functor.Alt (Alt (..))
import           Data.Functor.Identity (Identity (..))
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8')
import           PostgreSQL.Types (Value (..))
import           Text.Read (readEither)

-- | Cell value parser
--
-- @since 0.0.0
newtype Cell a = Cell
  { parseCell :: Value -> Either (NonEmpty Text) a }
  deriving stock Functor -- ^ @since 0.0.0

-- | @since 0.0.0
deriving via ReaderT Value (Except (NonEmpty Text)) instance Alt Cell

-- | Do not parse the cell at all.
--
-- @since 0.0.0
ignored :: Cell ()
ignored = Cell $ \_ -> Right ()

{-# INLINE ignored #-}

-- | Get the raw cell value. Does not allow @NULL@.
--
-- @since 0.0.0
raw :: Cell ByteString
raw = Cell $ \case
  Null -> Left ["Can't be NULL"]
  Value encoded -> Right encoded

{-# INLINE raw #-}

-- | Parse as UTF-8 'Text'. Does not allow @NULL@.
--
-- @since 0.0.0
text :: Cell Text
text = validate raw (first (Text.pack . show) . decodeUtf8')

{-# INLINE text #-}

-- | Parse something using its 'Read' instance via 'text'. Rejects @NULL@ values.
--
-- @since 0.0.0
readable :: Read a => Cell a
readable = validate text (first Text.pack . readEither . Text.unpack)

{-# INLINE readable #-}

-- | Validate the given cell parser.
--
-- @since 0.0.0
validate :: Cell a -> (a -> Either Text b) -> Cell b
validate (Cell run) f = Cell (run >=> first pure . f)

{-# INLINE validate #-}
