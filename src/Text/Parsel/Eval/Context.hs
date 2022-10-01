{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Text.Parsel.Eval.Context
  ( -- * Evaluation Contexts
    EvalCtx (EvalCtx, ctx'source),
  )
where

--------------------------------------------------------------------------------

-- Evaluation Contexts ---------------------------------------------------------

data EvalCtx = EvalCtx
  { ctx'source :: String
  }
  deriving (Eq, Ord, Show)
