{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Text.Parsel.Parse.Core
  ( -- * Parse State
    ParseState (ParseState, state'source, state'length, state'location),

    -- ** Construction
    newParseState,

    -- ** Modification
    feedParseState,
    feedsParseState,

    -- * Parse Monad
    ParseIO,
    Parse (Parse, unParse),
    runPrimParse,
  )
where

import Control.Applicative (Alternative, empty, (<|>))

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad, PrimState, RealWorld, primitive)
import Control.Monad.Reader (liftIO)
import Control.Monad.State (MonadState, get, put, state)

import Data.Kind (Type)
import Data.SrcLoc (SrcLoc)
import Data.SrcLoc qualified as SrcLoc
import Data.Text (Text)
import Data.Text qualified as Text

import GHC.Exts (State#)
import GHC.IO (IO (IO))

--------------------------------------------------------------------------------

import Text.Parsel.ParseError (ParseError, makeExnBottom)

-- Parse State -----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ParseState = ParseState
  { state'source :: {-# UNPACK #-} !Text
  -- ^ TODO
  , state'length :: {-# UNPACK #-} !Int
  -- ^ TODO
  , state'location :: {-# UNPACK #-} !SrcLoc
  -- ^ TODO
  }

-- Parse State - Construction --------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newParseState :: Text -> ParseState
newParseState src = ParseState src (Text.length src) SrcLoc.empty
{-# INLINE CONLIKE newParseState #-}

-- Parse State - Modification --------------------------------------------------

-- | TODO
--
-- @since 1.0.0
feedParseState :: Char -> ParseState -> ParseState
feedParseState chr (ParseState src len loc) = 
  let !src' = Text.drop 1 src
      !loc' = SrcLoc.feed loc chr
   in ParseState src' len loc'

-- | TODO
--
-- @since 1.0.0
feedsParseState :: Int -> Text -> ParseState -> ParseState
feedsParseState size text (ParseState src len loc) = 
  let !src' = Text.drop size src
      !loc' = Text.foldl' SrcLoc.feed loc text
   in ParseState src' len loc'

-- | TODO
--
-- @since 1.0.0
-- pushParseState :: PrimMonad m => ParseError -> ParseState (PrimState m) -> m (ParseState (PrimState m))
-- pushParseState exn (ParseState loc exns) = fmap (ParseState loc) (Vector.push exn exns)

-- | TODO
--
-- @since 1.0.0
-- pushParseState# :: ParseError -> ParseState s -> State# s -> (# State# s, ParseState s #)
-- pushParseState# exn (ParseState loc exns0) st0# =
--   let !(# st1#, exns1 #) = Vector.push# exn exns0 st0#
--    in (# st1#, ParseState loc exns1 #)

-- Parse Monad ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runPrimParse ::
  PrimMonad m =>
  ParseState ->
  Parse (PrimState m) a ->
  m (ParseState, Either ParseError a)
runPrimParse env0 (Parse k) =
  primitive \rw0# -> case k env0 rw0# of
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
  {unParse :: ParseState -> State# s -> (# State# s, ParseState, (# ParseError| a #) #)}

-- | @since 1.0.0
instance Functor (Parse s) where
  fmap f (Parse k) =
    Parse \env0 st0# -> case k env0 st0# of
      (# st1#, env1, (# e | #) #) -> (# st1#, env1, (# e | #) #)
      (# st1#, env1, (# | x #) #) -> (# st1#, env1, (# | f x #) #)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative (Parse s) where
  pure x = Parse \env st# -> (# st#, env, (# | x #) #)
  {-# INLINE pure #-}

  Parse f <*> Parse g =
    Parse \env0 st0# -> case f env0 st0# of
      (# st1#, env1, (# e | #) #) -> (# st1#, env1, (# e | #) #)
      (# st1#, env1, (# | k #) #) -> case g env1 st1# of
        (# st2#, env2, (# e | #) #) -> (# st2#, env2, (# e | #) #)
        (# st2#, env2, (# | x #) #) -> (# st2#, env2, (# | k x #) #)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Alternative (Parse s) where
  empty =
    Parse \env st# ->
      let !src = state'source env
          !loc = state'location env
       in (# st#, env, (# makeExnBottom loc src | #) #)
  {-# INLINE empty #-}

  Parse f <|> Parse g =
    Parse \env0 st0# -> case f env0 st0# of
      (# st1#, _, (# _ | #) #) -> g env0 st1#
      (# st1#, env1, (# | x #) #) -> (# st1#, env1, (# | x #) #)
  {-# INLINE (<|>) #-}

-- | @since 1.0.0
instance Monad (Parse s) where
  Parse k >>= f =
    Parse \env0 st0# -> case k env0 st0# of
      (# st1#, env1, (# e | #) #) -> (# st1#, env1, (# e | #) #)
      (# st1#, env1, (# | x #) #) -> unParse (f x) env1 st1#
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance PrimMonad (Parse s) where
  type PrimState (Parse s) = s

  primitive k =
    Parse \env st0# -> case k st0# of
      (# st1#, x #) -> (# st1#, env, (# | x #) #)
  {-# INLINE primitive #-}

-- | @since 1.0.0
instance MonadIO (Parse RealWorld) where
  liftIO (IO k) =
    Parse \env rw0# -> case k rw0# of
      (# rw1#, x #) -> (# rw1#, env, (# | x #) #)
  {-# INLINE liftIO #-}

-- | @since 1.0.0
instance MonadError ParseError (Parse s) where
  throwError e = Parse \env st# -> (# st#, env, (# e | #) #)
  {-# INLINE throwError #-}

  catchError (Parse k) f =
    Parse \env0 st0# -> case k env0 st0# of
      (# st1#, env1, (# e | #) #) -> unParse (f e) env1 st1#
      (# st1#, env1, (# | x #) #) -> (# st1#, env1, (# | x #) #)
  {-# INLINE catchError #-}

-- | @since 1.0.0
instance MonadState ParseState (Parse s) where
  get = Parse \env st# -> (# st#, env, (# | env #) #)
  {-# INLINE get #-}

  put env = Parse \_ st# -> (# st#, env, (# | () #) #)
  {-# INLINE put #-}

  state k =
    Parse \env0 st# -> case k env0 of
      (x, env1) -> (# st#, env1, (# | x #) #)
  {-# INLINE state #-}
