module Text.Parsel.Parse.ParseLoc (
  ParseLoc (..),

  -- * Construction
  empty, 

  -- * Modification
  nextColn,
  nextColn1,
  nextLine,

  -- * Feed
  feed,
  feeds,
) where

import Data.SrcLoc (SrcLoc)
import Data.SrcLoc qualified as SrcLoc
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Internal.Encoding.Utf8 qualified as Text.Utf8

--------------------------------------------------------------------------------

-- ParseLoc --------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
data ParseLoc = ParseLoc 
  { srcloc :: {-# UNPACK #-} !SrcLoc
    -- ^ The lexical location of this 'ParseLoc' relative to the beginning of 
    -- the source file it originated from.
  , offset :: {-# UNPACK #-} !Int
    -- ^ The absolute number of bytes 
  }
  deriving (Eq, Ord, Show)

-- Construction ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
empty :: ParseLoc 
empty = ParseLoc SrcLoc.empty 0
{-# INLINE CONLIKE empty #-}

-- Basic Operations ------------------------------------------------------------

-- Modification ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
nextColn :: ParseLoc -> Char -> ParseLoc 
nextColn loc chr = 
  let srcloc' = SrcLoc.nextColn (srcloc loc)
      offset' = Text.Utf8.utf8Length chr + offset loc
   in ParseLoc srcloc' offset'

-- | TODO
--
-- @since 1.0.0
nextColn1 :: ParseLoc -> ParseLoc 
nextColn1 loc = 
  let srcloc' = SrcLoc.nextColn (srcloc loc)
      offset' = 1 + offset loc
   in ParseLoc srcloc' offset'

-- | TODO
--
-- @since 1.0.0
nextLine :: ParseLoc -> Char -> ParseLoc 
nextLine loc chr = 
  let srcloc' = SrcLoc.nextLine (srcloc loc)
      offset' = Text.Utf8.utf8Length chr + offset loc
   in ParseLoc srcloc' offset'

-- Feed ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
feed :: ParseLoc -> Char -> ParseLoc
feed loc chr = 
  let srcloc' = SrcLoc.feed (srcloc loc) chr
      offset' = Text.Utf8.utf8Length chr + offset loc
   in ParseLoc srcloc' offset'
{-# INLINE feed #-}

-- | TODO
--
-- @since 1.0.0
feeds :: ParseLoc -> Text -> ParseLoc 
feeds loc text = 
  let srcloc' = Text.foldl' SrcLoc.feed (srcloc loc) text
      offset' = Text.foldr' ((+) . Text.Utf8.utf8Length) (offset loc) text
   in ParseLoc srcloc' offset'

-- Modification  ---------------------------------------------------------------


