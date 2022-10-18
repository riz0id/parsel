
module Text.Parsel.Parse.ParseContext
  ( ParseContext (ParseContext, context'label, context'source, context'length),

    -- * Construction
    newParseContext,

    -- * Modification
    relabelParseContext,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseContext = ParseContext
  { context'label :: {-# UNPACK #-} !Text
  -- ^ TODO
  , context'source :: {-# UNPACK #-} !Text
  -- ^ TODO
  , context'length :: {-# UNPACK #-} !Int
  -- ^ TODO
  }

-- Parse Context - Construction ------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newParseContext :: Text -> Text -> ParseContext
newParseContext label source = ParseContext label source (Text.length source)
{-# INLINE CONLIKE newParseContext #-}

-- Parse Context - Modification ------------------------------------------------

-- | TODO
--
-- @since 1.0.0
relabelParseContext :: Text -> ParseContext -> ParseContext
relabelParseContext label (ParseContext _ src len) = ParseContext label src len
{-# INLINE CONLIKE relabelParseContext #-}