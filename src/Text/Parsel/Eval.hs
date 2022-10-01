{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Text.Parsel.Eval
  ( -- * Running Eval
    evalIO,
    evalST,
    runEvalIO,

    -- * Evaluating Terms
    evalTerm,

    -- * Errors
    raiseChrMismatch,
    raiseEoF,

    -- * Operations
    single,
    advance,
    lookahead,
    alt,

    -- * Monad
    Eval (Eval, unEval),
  )
where

import Control.Applicative (liftA2)

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify')

import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List qualified as List
import Data.SrcLoc (SrcLoc, posn)
import Data.SrcLoc qualified as SrcLoc

--------------------------------------------------------------------------------

import Text.Parsel.Core
  ( Parse (Alt, Bot, Chr, Fix, Map, Seq, Str, Val),
  )
import Text.Parsel.Eval.Context
  ( EvalCtx (EvalCtx, ctx'source),
  )
import Text.Parsel.Eval.Core (Eval (Eval, unEval), EvalIO, runEvalIO, runEvalST)
import Text.Parsel.Eval.Error
  ( ParseError (ParseError),
    ParseErrorInfo (ExnChrMismatch, ExnEndOfFile),
  )
import Text.Parsel.Eval.Store
  ( EvalStore (EvalStore, store'location),
  )
import Control.Monad.ST (runST)

-- Running Eval ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
evalST :: String -> (forall s. Eval s a) -> Either ParseError a
evalST src m = runST do
  let ctx = EvalCtx src
  let env = EvalStore SrcLoc.empty
  result <- runEvalST ctx env m
  pure (snd result)

-- | TODO
--
-- @since 1.0.0
evalIO :: String -> EvalIO a -> IO (Either ParseError a)
evalIO src m =
  let ctx = EvalCtx src
      env = EvalStore SrcLoc.empty
   in fmap snd (runEvalIO ctx env m)

-- Evaluating Terms ------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
evalTerm :: forall a s. Parse a -> Eval s (Maybe a)
evalTerm Bot = pure Nothing
evalTerm (Val val) = pure (Just val)
evalTerm (Chr chr) =
  single chr $> Just chr
evalTerm (Str str) =
  traverse_ single str $> Just str
evalTerm (Map f x) = do
  result <- evalTerm x
  case result of
    Nothing -> pure Nothing
    Just rx -> pure (Just (f rx))
evalTerm (Seq x y) = do
  rx <- evalTerm x
  ry <- evalTerm y
  pure (liftA2 (,) rx ry)
evalTerm (Alt x y) =
  alt (evalTerm x) (evalTerm y)
evalTerm (Fix fix) = do
  let fix' :: Parse a -> Eval s (Parse a)
      fix' tm = do
        result <- evalTerm tm
        case result of
          Nothing -> fix' (fix Bot)
          Just tm' -> fix' (fix (pure tm'))
   in evalTerm =<< fix' Bot

-- Errors ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
raiseEoF :: Eval s a
raiseEoF = do
  src <- asks ctx'source
  loc <- gets store'location
  throwError (ParseError ExnEndOfFile loc loc src)

-- | TODO
--
-- @since 1.0.0
raiseChrMismatch :: Char -> Eval s a
raiseChrMismatch chr = do
  src <- asks ctx'source
  begin <- gets store'location
  let end :: SrcLoc
      end = SrcLoc.feed begin chr
   in throwError (ParseError (ExnChrMismatch chr) begin end src)

-- Operations ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
single :: Char -> Eval s ()
single chr = do
  chr' <- peek
  if chr == chr'
    then advance
    else raiseChrMismatch chr

-- | TODO
--
-- @since 1.0.0
advance :: Eval s ()
advance = do
  src <- asks ctx'source
  loc <- gets store'location
  if posn loc >= length src
    then raiseEoF
    else do
      let chr = src List.!! posn loc
      modify' \env ->
        env {store'location = SrcLoc.feed env.store'location chr}

-- | TODO
--
-- @since 1.0.0
peek :: Eval s Char
peek = do
  src <- asks ctx'source
  pos <- gets (posn . store'location)
  if pos >= length src
    then raiseEoF
    else pure (src List.!! pos)

-- | TODO
--
-- @since 1.0.0
lookahead :: Eval s Char
lookahead = do
  src <- asks ctx'source
  pos <- gets (posn . store'location)
  if 1 + pos >= length src
    then raiseEoF
    else pure (src List.!! (1 + pos))

-- | TODO
--
-- @since 1.0.0
alt :: Eval s a -> Eval s a -> Eval s a
alt (Eval f) (Eval g) =
  Eval \ctx env0 st0# -> case f ctx env0 st0# of
    (# st1#, env1, (# _ | #) #) -> g ctx env1 st1#
    (# st1#, env1, (# | x #) #) -> (# st1#, env1, (# | x #) #)