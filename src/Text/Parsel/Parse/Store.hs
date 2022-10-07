module Text.Parsel.Parse.Store
  ( -- * Evaluation Store
    ParseStore (ParseStore, store'location, store'branches),
  )
where

import Data.SrcLoc (SrcLoc)

--------------------------------------------------------------------------------

import Text.Parsel.ParseError (ParseError)

-- Parser Environments ---------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseStore = ParseStore
  { store'location :: {-# UNPACK #-} !SrcLoc
  -- ^ TODO
  , store'branches :: [ParseError]
  -- ^ TODO
  }
  deriving (Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
