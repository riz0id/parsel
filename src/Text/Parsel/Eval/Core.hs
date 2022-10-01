{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Text.Parsel.Eval.Core
  ( -- * Evaluation Monad
    runEvalIO,
    EvalIO,
    Eval (Eval, unEval),
  )
where

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad, PrimState, RealWorld, primitive)
import Control.Monad.Reader (MonadReader, ask, liftIO, local)
import Control.Monad.State (MonadState, get, put, state)

import Data.Kind (Type)

import GHC.Exts (State#)
import GHC.IO (IO (IO))

import Text.Parsel.Eval.Context (EvalCtx)
import Text.Parsel.Eval.Error (EvalExn)
import Text.Parsel.Eval.Store (EvalStore)

--------------------------------------------------------------------------------

-- Eval Monad ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runEvalIO ::
  EvalCtx ->
  EvalStore ->
  EvalIO a ->
  IO (EvalStore, Either EvalExn a)
runEvalIO ctx env0 (Eval k) =
  primitive \rw0# -> case k ctx env0 rw0# of
    (# rw1#, env1, (# e | #) #) -> (# rw1#, (env1, Left e) #)
    (# rw1#, env1, (# | x #) #) -> (# rw1#, (env1, Right x) #)
{-# INLINE runEvalIO #-}

-- | TODO
--
-- @since 1.0.0
type EvalIO :: Type -> Type
type EvalIO = Eval RealWorld

-- | TODO
--
-- @since 1.0.0
newtype Eval s a = Eval
  { unEval ::
      EvalCtx ->
      EvalStore ->
      State# s ->
      (# State# s, EvalStore, (# EvalExn| a #) #)
  }

-- | @since 1.0.0
instance Functor (Eval s) where
  fmap f (Eval k) =
    Eval \ctx env0 st0# -> case k ctx env0 st0# of
      (# st1#, env1, (# e | #) #) -> (# st1#, env1, (# e | #) #)
      (# st1#, env1, (# | x #) #) -> (# st1#, env1, (# | f x #) #)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative (Eval s) where
  pure x = Eval \_ env st# -> (# st#, env, (# | x #) #)
  {-# INLINE pure #-}

  Eval f <*> Eval g =
    Eval \ctx env0 st0# -> case f ctx env0 st0# of
      (# st1#, env1, (# e | #) #) -> (# st1#, env1, (# e | #) #)
      (# st1#, env1, (# | k #) #) -> case g ctx env1 st1# of
        (# st2#, env2, (# e | #) #) -> (# st2#, env2, (# e | #) #)
        (# st2#, env2, (# | x #) #) -> (# st2#, env2, (# | k x #) #)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad (Eval s) where
  Eval k >>= f =
    Eval \ctx env0 st0# -> case k ctx env0 st0# of
      (# st1#, env1, (# e | #) #) -> (# st1#, env1, (# e | #) #)
      (# st1#, env1, (# | x #) #) -> unEval (f x) ctx env1 st1#
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance PrimMonad (Eval s) where
  type PrimState (Eval s) = s

  primitive k =
    Eval \_ env st0# -> case k st0# of
      (# st1#, x #) -> (# st1#, env, (# | x #) #)
  {-# INLINE primitive #-}

-- | @since 1.0.0
instance MonadIO (Eval RealWorld) where
  liftIO (IO k) =
    Eval \_ env rw0# -> case k rw0# of
      (# rw1#, x #) -> (# rw1#, env, (# | x #) #)
  {-# INLINE liftIO #-}

-- | @since 1.0.0
instance MonadError EvalExn (Eval s) where
  throwError e = Eval \_ env st# -> (# st#, env, (# e | #) #)
  {-# INLINE throwError #-}

  catchError (Eval k) f =
    Eval \ctx env0 st0# -> case k ctx env0 st0# of
      (# st1#, env1, (# e | #) #) -> unEval (f e) ctx env1 st1#
      (# st1#, env1, (# | x #) #) -> (# st1#, env1, (# | x #) #)
  {-# INLINE catchError #-}

-- | @since 1.0.0
instance MonadReader EvalCtx (Eval s) where
  ask = Eval \ctx env st0# -> (# st0#, env, (# | ctx #) #)
  {-# INLINE ask #-}

  local f (Eval k) = Eval \ctx env -> k (f ctx) env
  {-# INLINE local #-}

-- | @since 1.0.0
instance MonadState EvalStore (Eval s) where
  get = Eval \_ env st# -> (# st#, env, (# | env #) #)
  {-# INLINE get #-}

  put env = Eval \_ _ st# -> (# st#, env, (# | () #) #)
  {-# INLINE put #-}

  state k =
    Eval \_ env0 st# -> case k env0 of
      (x, env1) -> (# st#, env1, (# | x #) #)
  {-# INLINE state #-}
