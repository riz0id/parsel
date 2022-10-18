{-# LANGUAGE BangPatterns #-}

module Text.Parsel.Parse.ParseState (
  -- * Parse State
  ParseState (ParseState, state'location, state'offset),

  -- ** Construction
  newParseState,

  -- ** Modification
  feedParseState,
  feedsParseState,
) where

import Data.SrcLoc (SrcLoc)
import Data.SrcLoc qualified as SrcLoc
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Internal.Encoding.Utf8 qualified as Text.Utf8

--------------------------------------------------------------------------------

-- Parse State -----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseState = ParseState
  { state'location :: {-# UNPACK #-} !SrcLoc
  -- ^ TODO
  , state'offset :: {-# UNPACK #-} !Int
  -- ^ TODO
  }

-- Parse State - Construction --------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newParseState :: ParseState
newParseState = ParseState SrcLoc.empty 0
{-# INLINE CONLIKE newParseState #-}

-- Parse State - Modification --------------------------------------------------

-- | TODO
--
-- @since 1.0.0
feedParseState :: Char -> ParseState -> ParseState
feedParseState chr (ParseState loc off) =
  let loc' = SrcLoc.feed loc chr
      off' = Text.Utf8.utf8Length chr + off
   in ParseState loc' off'

-- | TODO
--
-- @since 1.0.0
feedsParseState :: Text -> ParseState -> ParseState
feedsParseState text (ParseState loc off) =
  let loc' = Text.foldl' SrcLoc.feed loc text
      off' = Text.foldr' ((+) . Text.Utf8.utf8Length) off text
   in ParseState loc' off'
