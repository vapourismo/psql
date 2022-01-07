{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- The derived Alt instance for Cell causes this.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module PostgreSQL.Result.Cell
  ( Cell (..)
  , readable
  , ignored
  , text
  , validate
  )
where

import           Control.Monad ((>=>))
import           Control.Monad.Except (Except, ExceptT (..))
import           Control.Monad.Reader (ReaderT (..))
import           Data.Bifunctor (first)
import           Data.Functor.Alt (Alt (..))
import           Data.Functor.Identity (Identity (..))
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
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

-- | Parse anything using its 'Read' instance. Rejects @NULL@ values.
--
-- @since 0.0.0
readable :: Read a => Cell a
readable = Cell $ \case
  Null -> Left ["Can't be NULL"]
  Value text -> first (pure . Text.pack) $ readEither $ Text.unpack $ decodeUtf8 text

{-# INLINE readable #-}

-- | Do not parse the cell at all.
--
-- @since 0.0.0
ignored :: Cell ()
ignored = Cell $ \_ -> Right ()

{-# INLINE ignored #-}

-- | Parse as UTF-8 'Text'
--
-- @since 0.0.0
text :: Cell Text
text = Cell $ \case
  Null -> Left ["Can't be NULL"]
  Value encoded -> Right (decodeUtf8 encoded)

{-# INLINE text #-}

-- | Validate the given cell parser.
--
-- @since 0.0.0
validate :: Cell a -> (a -> Either Text b) -> Cell b
validate (Cell run) f = Cell (run >=> first pure . f)

{-# INLINE validate #-}
