module Text.Parsel.ParseError
  ( -- * Evaluation Exceptions
    ParseError (ParseError, exn'kind, exn'begin, exn'end, exn'source),

    -- * Exception Info
    ParseErrorInfo
      ( ExnEndOfFile,
        ExnEvalBottom,
        ExnChrMismatch,
        ExnStrMismatch
      ),
  )
where

import Data.SrcLoc (SrcLoc)

--------------------------------------------------------------------------------

-- Evaluation Exceptions -------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseError = ParseError
  { exn'kind :: ParseErrorInfo
  , exn'begin :: {-# UNPACK #-} !SrcLoc
  , exn'end :: {-# UNPACK #-} !SrcLoc
  , exn'source :: String
  }
  deriving (Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0

-- Exception Info --------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseErrorInfo
  = ExnEndOfFile
  | ExnEvalBottom
  | ExnChrMismatch {-# UNPACK #-} !Char
  | ExnStrMismatch String
  deriving (Eq, Ord, Show)