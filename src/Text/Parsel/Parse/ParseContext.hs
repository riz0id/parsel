module Text.Parsel.Parse.ParseContext (
  ParseContext (
    ParseContext,
    context'label,
    context'buffer,
    context'length
  ),

  -- * Construction
  newParseContext,

  -- * Modification
  relabelParseContext,
) where

import Data.Text.Internal (Text (Text))
import Data.Text.Array (Array)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseContext = ParseContext
  { context'label :: {-# UNPACK #-} !Text
  -- ^ TODO
  , context'buffer :: {-# UNPACK #-} !Array
  -- ^ TODO
  , context'length :: {-# UNPACK #-} !Int
  -- ^ TODO
  }

-- Parse Context - Construction ------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newParseContext :: Text -> Text -> ParseContext
newParseContext label (Text source _ len) = ParseContext label source len
{-# INLINE CONLIKE newParseContext #-}

-- Parse Context - Modification ------------------------------------------------

-- | TODO
--
-- @since 1.0.0
relabelParseContext :: Text -> ParseContext -> ParseContext
relabelParseContext label (ParseContext _ src len) = ParseContext label src len
{-# INLINE CONLIKE relabelParseContext #-}