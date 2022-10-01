
module Text.Parsel.Eval.Store
  ( -- * Evaluation Store
    EvalStore (EvalStore, store'location),
  )
where

import Data.SrcLoc (SrcLoc)

--------------------------------------------------------------------------------

-- Evaluation Environments -----------------------------------------------------

data EvalStore = EvalStore
  { store'location :: {-# UNPACK #-} !SrcLoc
  }
  deriving (Eq, Ord, Show)