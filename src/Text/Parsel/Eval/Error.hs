module Text.Parsel.Eval.Error
  ( -- * Evaluation Exceptions
    EvalExn (EvalExn, exn'kind, exn'begin, exn'end, exn'source),

    -- * Exception Info 
    EvalExnInfo (ExnEndOfFile, ExnChrMismatch, ExnStrMismatch),
  )
where

import Data.SrcLoc (SrcLoc)

--------------------------------------------------------------------------------

-- Evaluation Exceptions -------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
data EvalExn = EvalExn
  { exn'kind :: EvalExnInfo
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
data EvalExnInfo
  = ExnEndOfFile
  | ExnChrMismatch {-# UNPACK #-} !Char
  | ExnStrMismatch String
  deriving (Eq, Ord, Show)
