{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Parsel.ParseError
  ( -- * Evaluation Exceptions
    ParseError (ParseError, exn'kind, exn'begin, exn'end, exn'source),

    -- ** Construction
    makeExnEndOfFile,
    makeExnBottom,
    makeExnChar,
    makeExnString,

    -- * Exception Info
    ParseErrorInfo (ExnEoF, ExnBot, ExnChr, ExnStr),
  )
where

import Control.DeepSeq (NFData)

import Data.SrcLoc (SrcLoc)
import Data.SrcLoc qualified as SrcLoc
import Data.Text (Text)
import qualified Data.Text as Text

import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- Parse Errors ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseError = ParseError
  { exn'kind :: ParseErrorInfo
  , exn'begin :: {-# UNPACK #-} !SrcLoc
  , exn'end :: {-# UNPACK #-} !SrcLoc
  , exn'source :: {-# UNPACK #-} !Text
  }
  deriving (Eq, Generic, Ord, NFData, Show)

-- Parse Errors - Construction -------------------------------------------------

-- | TODO
--
-- @since 1.0.0
makeExnEndOfFile :: SrcLoc -> Text -> ParseError
makeExnEndOfFile loc src = ParseError ExnEoF loc loc src

-- | TODO
--
-- @since 1.0.0
makeExnBottom :: SrcLoc -> Text -> ParseError
makeExnBottom loc src = ParseError ExnBot loc loc src

-- | TODO
--
-- @since 1.0.0
makeExnChar :: SrcLoc -> Char -> Text -> ParseError
makeExnChar loc chr = ParseError (ExnChr chr) loc (SrcLoc.feed loc chr)

-- | TODO
--
-- @since 1.0.0
makeExnString :: SrcLoc -> Text -> Text -> ParseError
makeExnString loc text = ParseError (ExnStr text) loc (Text.foldl' SrcLoc.feed loc text)

-- Exception Info --------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseErrorInfo
  = ExnEoF
  | ExnBot
  | ExnChr {-# UNPACK #-} !Char
  | ExnStr Text
  deriving (Eq, Generic, Ord, NFData, Show)
