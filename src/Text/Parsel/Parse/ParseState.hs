{-# LANGUAGE BangPatterns #-}

module Text.Parsel.Parse.ParseState (
  -- * Parse State
  ParseState (..),

  -- ** Construction
  newParseState,

  -- ** Lenses
  stwBegin,
  stwEnd,

  -- ** Basic Operations 
  diffBytes,

  -- ** Modification
  feedParseState,
  feedsParseState,
) where

import Control.Lens (Lens', lens)

import Data.Text (Text)

--------------------------------------------------------------------------------

import Text.Parsel.Parse.ParseLoc ( ParseLoc )
import Text.Parsel.Parse.ParseLoc qualified as ParseLoc 

-- Parse State -----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseState = ParseState
  { state'begin :: {-# UNPACK #-} !ParseLoc
  -- ^ TODO
  , state'end :: {-# UNPACK #-} !ParseLoc
  -- ^ TODO
  }
  deriving (Eq, Ord, Show)

-- Parse State - Construction --------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newParseState :: ParseState
newParseState = ParseState ParseLoc.empty ParseLoc.empty 
{-# INLINE CONLIKE newParseState #-}

-- Lenses ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
stwBegin :: Lens' ParseState ParseLoc 
stwBegin = lens state'begin \st x -> st{state'begin = x}

-- | TODO
--
-- @since 1.0.0
stwEnd :: Lens' ParseState ParseLoc 
stwEnd = lens state'end \st x -> st{state'end = x}

-- Basic Operations ------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
diffBytes :: ParseState -> Int
diffBytes stw = 
  let i0 = ParseLoc.offset (state'begin stw)
      i1 = ParseLoc.offset (state'end stw)
   in i1 - i0
{-# INLINE diffBytes #-}

-- Modification ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
feedParseState :: Char -> ParseState -> ParseState
feedParseState chr st = st{state'end = ParseLoc.feed (state'end st) chr}
  -- let end' = SrcLoc.feed (state'end st) chr
  --     off' = Text.Utf8.utf8Length chr + state'offset st
  --  in st {state'end = end', state'offset = off'}

-- | TODO
--
-- @since 1.0.0
feedsParseState :: Text -> ParseState -> ParseState
feedsParseState text st = st{state'end = ParseLoc.feeds (state'end st) text}
