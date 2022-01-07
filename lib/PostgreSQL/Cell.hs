{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- The derived Alt instance for Parser causes this.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module PostgreSQL.Cell
  ( Parser (..)
  , readableParser
  , ignoringParser
  , textParser
  , validateParser
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

-- | Parser of a cell value
newtype Parser a = Parser
  { runParser :: Value -> Either (NonEmpty Text) a }
  deriving stock Functor

deriving via ReaderT Value (Except (NonEmpty Text)) instance Alt Parser

-- | Parse anything using its 'Read' instance. Rejects @NULL@ values.
readableParser :: Read a => Parser a
readableParser = Parser $ \case
  Null -> Left ["Can't be NULL"]
  Value text -> first (pure . Text.pack) $ readEither $ Text.unpack $ decodeUtf8 text

{-# INLINE readableParser #-}

-- | Do not parse the cell at all.
ignoringParser :: Parser ()
ignoringParser = Parser $ \_ -> Right ()

{-# INLINE ignoringParser #-}

-- | Parse as UTF-8 'Text'
textParser :: Parser Text
textParser = Parser $ \case
  Null -> Left ["Can't be NULL"]
  Value encoded -> Right (decodeUtf8 encoded)

{-# INLINE textParser #-}

-- | Validate the given cell parser.
validateParser :: Parser a -> (a -> Either Text b) -> Parser b
validateParser (Parser run) f = Parser (run >=> first pure . f)

{-# INLINE validateParser #-}
