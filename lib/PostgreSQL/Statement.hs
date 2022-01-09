{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Tools to deal with templates and statements are defined here.
module PostgreSQL.Statement
  ( Template
  , code
  , identifier
  , string
  , param
  , paramWith
  , constant

  , Statement (..)
  , renderTemplate

  , PreparedStatement (..)

  , tpl
  , stmt
  )
where

import           Control.Applicative ((<|>))
import           Control.Monad (join)
import qualified Control.Monad.State.Strict as State
import qualified Crypto.Hash as Hash
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import           Data.Char (isAlphaNum)
import           Data.Foldable (asum, fold)
import           Data.Functor.Contravariant (Contravariant (..))
import qualified Data.Sequence as Sequence
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           Data.Traversable (for)
import           Data.Void (Void)
import           GHC.OverloadedLabels (IsLabel (..))
import           GHC.Records (HasField (..))
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as Quote
import           Numeric.Natural (Natural)
import qualified PostgreSQL.Param as Param
import           PostgreSQL.Types (Oid)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char

data Segment a
  = Parameter (Param.Info (a -> Param.Value))
  | Code Text

instance Contravariant Segment where
  contramap f = \case
    Parameter g -> Parameter $ fmap (. f) g
    Code text -> Code text

  {-# INLINE contramap #-}

-- | SQL statement template
--
-- @since 0.0.0
newtype Template a = Template
  { _unStatement :: Sequence.Seq (Segment a) }
  deriving newtype
    ( Semigroup -- ^ @since 0.0.0
    , Monoid -- ^ @since 0.0.0
    )

-- | @since 0.0.0
instance Contravariant Template where
  contramap f (Template seqs) = Template (fmap (contramap f) seqs)

  {-# INLINE contramap #-}

-- | @OverloadedStrings@ helper for 'code'
--
-- > "my code" === code "my code"
--
-- @since 0.0.0
instance IsString (Template a) where
  fromString = code . Text.pack

  {-# INLINE fromString #-}

-- | @OverloadedLabels@ helper for 'param'
--
-- > #myParam === param (getField @"myParam")
--
-- Use this with a database:
--
-- > data MyFoo = MyFoo { bar :: Int, baz :: String }
-- >
-- > myStatementTpl :: Template MyFoo
-- > myStatementTpl = "SELECT * FROM my_foo WHERE bar = " <> #bar <> " AND baz = " <> #baz
--
--
-- @since 0.0.0
instance (HasField n r a, Param.Param a) => IsLabel n (Template r) where
  fromLabel = param (getField @n @r @a)

  {-# INLINE fromLabel #-}

-- | Create a code-only statement.
--
-- @since 0.0.0
code :: Text -> Template a
code = Template . Sequence.singleton . Code

{-# INLINE code #-}

-- | Create a code segment that mentions the given identifier (e.g. table or column name).
--
-- @since 0.0.0
identifier :: Text -> Template a
identifier name =
  code $ Text.concat ["\"", safeName, "\""]
  where
    safeName = Text.intercalate "\"\"" $ Text.split (== '"') name

{-# INLINE identifier #-}

-- | Encase the given string literal in single quotes. Single quotes in the literal are
-- automatically escaped.
--
-- @since 0.0.0
string :: Text -> Template a
string str = "'" <> code (Text.replace "'" "''" str) <> "'"

{-# INLINE string #-}

-- | Annotate the given statement with a type signature.
annotateParamType :: Maybe Text -> Template a -> Template a
annotateParamType typeAnnotation stmt =
  case typeAnnotation of
    Just paramType -> "(" <> stmt <> code (" :: " <> paramType <> ")")
    Nothing -> stmt

{-# INLINE annotateParamType #-}

-- | Reference a parameter.
--
-- @since 0.0.0
param :: forall b a. Param.Param b => (a -> b) -> Template a
param f = paramWith $ fmap (. f) $ Param.paramInfo @b

{-# INLINE param #-}

-- | Reference a parameter.
--
-- @since 0.0.0
paramWith :: Param.Info (a -> Param.Value) -> Template a
paramWith info =
  annotateParamType (Param.info_typeName info) $ Template $ Sequence.singleton $ Parameter info

{-# INLINE paramWith #-}

-- | Constant part of a query.
--
-- @since 0.0.0
constant :: forall b a. Param.Param b => b -> Template a
constant x = paramWith $ fmap (. const x) $ Param.paramInfo @b

{-# INLINE constant #-}

-- | Rendered SQL statement
--
-- @since 0.0.0
data Statement a = Statement
  { statement_code :: ByteString
  , statement_mkParams :: a -> [Param.PackedParam]
  , statement_types :: [Oid]
  , statement_name :: ByteString
  }

-- | @since 0.0.0
instance Contravariant Statement where
  contramap f statement = Statement
    { statement_code = statement_code statement
    , statement_mkParams = statement_mkParams statement . f
    , statement_types = statement_types statement
    , statement_name = statement_name statement
    }

  {-# INLINE contramap #-}

-- | Render the SQL statement.
--
-- @since 0.0.0
renderTemplate :: Template a -> Statement a
renderTemplate (Template segments :: Template a) = Statement
  { statement_code = codeBytes
  , statement_mkParams = mkParams
  , statement_types = types
  , statement_name = ByteString.Char8.pack (show hash)
  }
  where
    code :: Text
    code = fold $ flip State.evalState (1 :: Natural) $ for segments $ \case
      Parameter _ -> do
        index <- State.state $ \i -> (i, i + 1)
        pure $ Text.pack $ '$' : show index

      Code text ->
        pure text

    codeBytes :: ByteString
    codeBytes = encodeUtf8 code

    mkParams :: a -> [Param.PackedParam]
    mkParams input =
      foldr
        (\case
          Parameter info -> (Param.packParam (fmap ($ input) info) :)
          Code{} -> id
        )
        []
        segments

    types :: [Oid]
    types =
      foldr
        (\case
          Parameter info -> (Param.typeOid (Param.info_type info) :)
          Code{} -> id
        )
        []
        segments

    hash :: Hash.Digest Hash.SHA224
    hash =
      Hash.hashFinalize $ Hash.hashUpdates Hash.hashInit $
        codeBytes : map (ByteString.Char8.pack . show) types

{-# INLINE renderTemplate #-}

---

-- | Prepared statement
--
-- @since 0.0.0
data PreparedStatement a = PreparedStatement
  { preparedStatement_name :: ByteString
  , preparedStatement_mkParams :: a -> [Param.PackedParamPrepared]
  }

-- | @since 0.0.0
instance Contravariant PreparedStatement where
  contramap f statement = PreparedStatement
    { preparedStatement_name = preparedStatement_name statement
    , preparedStatement_mkParams = preparedStatement_mkParams statement . f
    }

---

parseName :: Megaparsec.Parsec Void String String
parseName =
  Megaparsec.takeWhile1P Nothing $ \c ->
    isAlphaNum c || elem @[] c "_'"

data QuoteSegment
  = QuoteCode String
  | QuoteParam String
  | QuoteSubst String

parseQuote :: Megaparsec.Parsec Void String (TH.Q TH.Exp)
parseQuote =
  combine <$> Megaparsec.many (asum [nonSegment, dollar, interactive])
  where
    nonSegment = QuoteCode <$> Megaparsec.takeWhile1P Nothing (/= '$')

    dollar = QuoteCode "$" <$ Megaparsec.Char.string "$$"

    between lhs inner rhs =
      Megaparsec.between (Megaparsec.Char.char lhs) (Megaparsec.Char.char rhs) inner

    interactive = do
      _ <- Megaparsec.Char.char '$'
      asum
        [ QuoteSubst <$> between '(' parseName ')'
        , QuoteParam <$> (between '{' parseName '}' <|> parseName)
        ]

    combine segments = do
      segments <- for segments $ pure . \case
        QuoteCode code ->
          [e| fromString $(TH.stringE code) |]

        QuoteParam paramCode ->
          integrateAsParam paramCode

        QuoteSubst paramCode ->
          integrateAsSubst paramCode

      [e| mconcat $(TH.listE segments) |]

integrateAsParam :: String -> TH.ExpQ
integrateAsParam paramCode =
  [e| PostgreSQL.Statement.param $(TH.varE (TH.mkName paramCode)) |]

integrateAsSubst :: String -> TH.ExpQ
integrateAsSubst paramCode =
  TH.varE (TH.mkName paramCode)

tplQuoteExp :: String -> TH.Q TH.Exp
tplQuoteExp contents = do
  join $ either (fail . Megaparsec.errorBundlePretty) pure $
    Megaparsec.parse
      (parseQuote <* Megaparsec.eof)
      "(PostgreSQL.Statement.tpl quasi-quotation)"
      contents

-- | Produces a 'Template' expression.
--
-- Supports the same features as 'stmt'.
--
-- @since 0.0.0
tpl :: Quote.QuasiQuoter
tpl = Quote.QuasiQuoter
  { Quote.quoteExp = tplQuoteExp
  , Quote.quotePat = error "'tpl' cannot be used in a pattern"
  , Quote.quoteType = error "'tpl' cannot be used in a type"
  , Quote.quoteDec = error "'tpl' cannot be used in a declaration"
  }

stmtQuoteExp :: String -> TH.Q TH.Exp
stmtQuoteExp contents = do
  stmt <- either (fail . Megaparsec.errorBundlePretty) pure $
    Megaparsec.parse
      (parseQuote <* Megaparsec.eof)
      "(PostgreSQL.Statement.stmt quasi-quotation)"
      contents
  [e| renderTemplate $stmt |]

-- | Produces a 'Statement' expression.
--
-- > [stmt| SELECT $param * 2 |]
--
-- Use @$$@ to render a single @$@.
--
-- == Parameters
--
-- Use @$param@ or @${param}@ to reference a query parameter.
--
-- @[stmt| ${x} |]@ is equivalent to @'param' x@.
--
-- == Substitutions
--
-- Use @$(substr)@ to embed another 'Template' where @substr :: 'Template' a@.
--
-- @[stmt| $(x) |]@ is equivalent to @x@.
--
-- == Examples
--
-- > data MyParams = MyParams { foo :: Int, bar :: Text }
-- >
-- > myStatement :: Statement MyParams
-- > myStatement = [stmt| SELECT baz FROM my_table WHERE foo > ${foo} AND bar = ${bar} |]
--
-- @since 0.0.0
stmt :: Quote.QuasiQuoter
stmt = Quote.QuasiQuoter
  { Quote.quoteExp = stmtQuoteExp
  , Quote.quotePat = error "'stmt' cannot be used in a pattern"
  , Quote.quoteType = error "'stmt' cannot be used in a type"
  , Quote.quoteDec = error "'stmt' cannot be used in a declaration"
  }
