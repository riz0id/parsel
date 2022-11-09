{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Parsel.ParseError
  ( -- * Parse Errors
    ParseError (..),

    -- ** Construction
    makeExnEndOfFile,
    makeExnBottom,
    makeExnChar,
    makeExnString,

    -- ** Docs
    docParseError,
    docParseErrorInfo,
    docParseErrorSpan,
    docParseErrorLoc,
    docParseErrorSource,

    -- * Parse Error Info
    ParseErrorInfo (ExnEoF, ExnBot, ExnChr, ExnStr),

    -- ** Docs
  )
where

import Control.DeepSeq (NFData)

import Data.SrcLoc (SrcLoc)
import Data.SrcLoc qualified as SrcLoc
import Data.SrcSpan (SrcSpan)
import Data.SrcSpan qualified as SrcSpan
import Data.Text (Text)
import Data.Text qualified as Text

import GHC.Generics (Generic)

import Prelude hiding (lines, span)

import Text.Emit (Doc, emit, (<+>), (<!>))
import Text.Emit qualified as Emit

--------------------------------------------------------------------------------

-- Parse Errors ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseError = ParseError
  { exn'label :: {-# UNPACK #-} !Text
  , exn'kind :: ParseErrorInfo
  , exn'span :: {-# UNPACK #-} !SrcSpan
  , exn'source :: {-# UNPACK #-} !Text
  }
  deriving (Eq, Generic, Ord, NFData, Show)

-- Parse Errors - Construction -------------------------------------------------

-- | TODO
--
-- @since 1.0.0
makeExnEndOfFile :: Text -> SrcSpan -> Text -> ParseError
makeExnEndOfFile label = ParseError label ExnEoF
{-# INLINE CONLIKE makeExnEndOfFile #-}

-- | TODO
--
-- @since 1.0.0
makeExnBottom :: Text -> SrcSpan -> Text -> ParseError
makeExnBottom label = ParseError label ExnBot 
{-# INLINE CONLIKE makeExnBottom #-}

-- | TODO
--
-- @since 1.0.0
makeExnChar :: Text -> SrcSpan -> Char -> Text -> ParseError
makeExnChar label sp chr = ParseError label (ExnChr chr) sp
{-# INLINE CONLIKE makeExnChar #-}

-- | TODO
--
-- @since 1.0.0
makeExnString :: Text -> SrcLoc -> Text -> Text -> ParseError
makeExnString label loc text =
  let span :: SrcSpan
      span = SrcSpan.SrcSpan loc (Text.foldl' SrcLoc.feed loc text)
   in ParseError label (ExnStr text) span
{-# INLINE CONLIKE makeExnString #-}

-- Parse Errors - Show ---------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
docParseError :: ParseError -> Doc a
docParseError exn =
  "\ESC[1;31m"
    <> "Error:"
    <> "\ESC[0m"
    <+> docParseErrorInfo (SrcSpan.begin (exn'span exn)) (exn'label exn) (exn'source exn) (exn'kind exn)
    <!> docParseErrorSource (exn'span exn) (exn'source exn)

-- | TODO
--
-- @since 1.0.0
docParseErrorInfo :: SrcLoc -> Text -> Text -> ParseErrorInfo -> Doc a
docParseErrorInfo loc label source info = case info of
  ExnBot ->
    "Encountered the empty pattern while parsing"
      <+> Emit.text label
  ExnEoF ->
    "Encountered end of file while parsing"
      <+> Emit.text label
  ExnChr chr ->
    "Unexpected character"
      <> "\ESC[1;31m"
      <+> (let actual'chr :: Text
               actual'chr = Text.take 1 (Text.drop (SrcLoc.posn loc) source)
            in if actual'chr == "\n"
                 then "newline"
                 else "'" <> Emit.text actual'chr <> "'")
      <+> "\ESC[0m"
      <> "while parsing"
      <+> Emit.text label
      <> ", expected the character"
      <+> "\ESC[1;34m"
      <> "'"
      <> Emit.text (Text.singleton chr)
      <> "'"
      <> "\ESC[0m"
  ExnStr str ->
    "Unexpected string '"
      <> "\ESC[1;31m"
      <> Emit.text (Text.take (Text.length str) (Text.drop (SrcLoc.posn loc) source))
      <> "\ESC[0m"
      <> "' while parsing"
      <+> Emit.text label
      <> ", expected the string"
      <+> "\ESC[1;34m"
      <> "'"
      <> Emit.text str
      <> "'"
      <> "\ESC[0m"

-- | TODO
--
-- @since 1.0.0
docParseErrorSpan :: SrcSpan -> Doc a
docParseErrorSpan span =
  let doc'loc1 = docParseErrorLoc (SrcSpan.begin span)
      doc'loc2 = docParseErrorLoc (SrcSpan.end span)
   in doc'loc1 <+> "to" <+> doc'loc2

-- | TODO
--
-- @since 1.0.0
docParseErrorLoc :: SrcLoc -> Doc a
docParseErrorLoc loc =
  let doc'posn = emit (SrcLoc.posn loc)
      doc'line = emit (SrcLoc.line loc)
      doc'coln = emit (SrcLoc.coln loc)
   in doc'posn <> ":" <> doc'line <> ":" <> doc'coln

-- | TODO
--
-- @since 1.0.0
docParseErrorSource :: SrcSpan -> Text -> Doc a
docParseErrorSource span source =
  let lineno = " " <> Text.pack (show (SrcLoc.line (SrcSpan.begin span)))
      line'margin = Emit.text (Text.replicate (Text.length lineno) " ")
      padding = Emit.text (Text.replicate (SrcLoc.coln (SrcSpan.begin span) - 1) " ")
      underline = padding <> "\ESC[1;31m" <> Emit.text (Text.replicate (SrcSpan.diff span) "─") <> "\ESC[0m"
      sourceline = Emit.text source
   in Emit.vsep
        [ Emit.hsep [line'margin, "╭─" <+> docParseErrorSpan span]
        , Emit.hsep [line'margin, "│"]
        , Emit.hsep [Emit.text lineno, "│", sourceline]
        , Emit.hsep [line'margin, "┊" <+> underline]
        ]

-- Parse Error Info ------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseErrorInfo
  = ExnBot
  | ExnEoF
  | ExnChr {-# UNPACK #-} !Char
  | ExnStr {-# UNPACK #-} !Text
  deriving (Eq, Generic, Ord, NFData, Show)

-- Parse Error Info - Show -----------------------------------------------------

-- | TODO
--
-- @since 1.0.0