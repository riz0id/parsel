

module Text.Parsel.Parse.Context
  ( -- * Parser Contexts
    ParseCtx (ParseCtx, ctx'source),
  )
where

--------------------------------------------------------------------------------

-- Parser Contexts -------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseCtx = ParseCtx
  { ctx'source :: String
  -- ^ TODO
  }
  deriving (Eq, Ord, Show)
