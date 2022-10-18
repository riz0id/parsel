{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Text.Parsel.Parse.Core
  ( -- * Parse Monad
    ParseIO,
    Parse (Parse, unParse),
    runPrimParse,
  )
where

import Control.Applicative (Alternative, empty, (<|>))

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad, PrimState, RealWorld, primitive)
import Control.Monad.Reader (MonadReader, ask, local)
import Control.Monad.State (MonadState, get, put, state)

import Data.Kind (Type)

import GHC.Exts (State#)
import GHC.IO (IO (IO))

--------------------------------------------------------------------------------

import Text.Parsel.Parse.ParseContext (ParseContext (context'label))
import Text.Parsel.Parse.ParseState (ParseState (state'location))
import Text.Parsel.ParseError (ParseError, makeExnBottom)
import Text.Parsel.Parse.ParseContext (context'source)

-- Parse Monad ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runPrimParse ::
  PrimMonad m =>
  ParseContext ->
  ParseState ->
  Parse (PrimState m) a ->
  m (ParseState, Either ParseError a)
runPrimParse ctx env0 (Parse k) =
  primitive \rw0# -> case k ctx env0 rw0# of
    (# rw1#, env1, (# e | #) #) -> (# rw1#, (env1, Left e) #)
    (# rw1#, env1, (# | x #) #) -> (# rw1#, (env1, Right x) #)
{-# INLINE runPrimParse #-}

-- | TODO
--
-- @since 1.0.0
type ParseIO :: Type -> Type
type ParseIO = Parse RealWorld

-- | TODO
--
-- @since 1.0.0
newtype Parse s a = Parse
  { unParse ::
      ParseContext ->
      ParseState ->
      State# s ->
      (# State# s, ParseState, (# ParseError| a #) #)
  }

-- | @since 1.0.0
instance Functor (Parse s) where
  fmap f (Parse k) =
    Parse \ctx env0 st0# -> case k ctx env0 st0# of
      (# st1#, env1, (# e | #) #) -> (# st1#, env1, (# e | #) #)
      (# st1#, env1, (# | x #) #) -> (# st1#, env1, (# | f x #) #)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative (Parse s) where
  pure x = Parse \_ env st# -> (# st#, env, (# | x #) #)
  {-# INLINE pure #-}

  Parse f <*> Parse g =
    Parse \ctx env0 st0# -> case f ctx  env0 st0# of
      (# st1#, env1, (# e | #) #) -> (# st1#, env1, (# e | #) #)
      (# st1#, env1, (# | k #) #) -> case g ctx env1 st1# of
        (# st2#, env2, (# e | #) #) -> (# st2#, env2, (# e | #) #)
        (# st2#, env2, (# | x #) #) -> (# st2#, env2, (# | k x #) #)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Alternative (Parse s) where
  empty =
    Parse \ctx env st# ->
      let !lab = context'label ctx
          !src = context'source ctx
          !loc = state'location env
       in (# st#, env, (# makeExnBottom lab loc src | #) #)
  {-# INLINE empty #-}

  Parse f <|> Parse g =
    Parse \ctx env0 st0# -> case f ctx env0 st0# of
      (# st1#, _, (# _ | #) #) -> g ctx env0 st1#
      (# st1#, env1, (# | x #) #) -> (# st1#, env1, (# | x #) #)
  {-# INLINE (<|>) #-}

-- | @since 1.0.0
instance Monad (Parse s) where
  Parse k >>= f =
    Parse \ctx env0 st0# -> case k ctx env0 st0# of
      (# st1#, env1, (# e | #) #) -> (# st1#, env1, (# e | #) #)
      (# st1#, env1, (# | x #) #) -> unParse (f x) ctx env1 st1#
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance PrimMonad (Parse s) where
  type PrimState (Parse s) = s

  primitive k =
    Parse \_ env st0# -> case k st0# of
      (# st1#, x #) -> (# st1#, env, (# | x #) #)
  {-# INLINE primitive #-}

-- | @since 1.0.0
instance MonadIO (Parse RealWorld) where
  liftIO (IO k) =
    Parse \_ env rw0# -> case k rw0# of
      (# rw1#, x #) -> (# rw1#, env, (# | x #) #)
  {-# INLINE liftIO #-}

-- | @since 1.0.0
instance MonadError ParseError (Parse s) where
  throwError e = Parse \_ env st# -> (# st#, env, (# e | #) #)
  {-# INLINE throwError #-}

  catchError (Parse k) f =
    Parse \ctx env0 st0# -> case k ctx env0 st0# of
      (# st1#, env1, (# e | #) #) -> unParse (f e) ctx env1 st1#
      (# st1#, env1, (# | x #) #) -> (# st1#, env1, (# | x #) #)
  {-# INLINE catchError #-}

-- | @since 1.0.0
instance MonadState ParseState (Parse s) where
  get = Parse \_ env st# -> (# st#, env, (# | env #) #)
  {-# INLINE get #-}

  put env = Parse \_ _ st# -> (# st#, env, (# | () #) #)
  {-# INLINE put #-}

  state k =
    Parse \_ env0 st# -> case k env0 of
      (x, env1) -> (# st#, env1, (# | x #) #)
  {-# INLINE state #-}

-- | @since 1.0.0
instance MonadReader ParseContext (Parse s) where
  ask = Parse \ctx env st# -> (# st#, env, (# | ctx #) #)
  {-# INLINE ask #-}

  local f (Parse k) = Parse \ctx env st# -> k (f ctx) env st#
  {-# INLINE local #-}