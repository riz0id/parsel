{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Text.Parsel.Parse
  ( module Text.Parsel.Parse.Core,
  
    -- * Running Eval
    evalIO,
    evalST,

    -- * Evaluating Terms
    evalTerm,

    -- * Errors
    raiseChrMismatch,
    raiseEoF,

    -- * Operations
    single,
    advance,
    alt,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Control.Monad.ST (runST)
import Control.Monad.State (gets, modify')

import Data.Functor (($>))
import Data.List qualified as List
import Data.SrcLoc (SrcLoc, posn)
import Data.SrcLoc qualified as SrcLoc

--------------------------------------------------------------------------------

import Control.Applicative (liftA2)
import Text.Parsel.Grammar.Core
  ( Grammar (Alt, Bot, Chr, Fix, Loc, Map, Seq, Eps),
  )
import Text.Parsel.Parse.Context
  ( ParseCtx (ParseCtx, ctx'source),
  )
import Text.Parsel.Parse.Core
import Text.Parsel.Parse.Store
  ( ParseStore (ParseStore, store'location, store'branches),
  )
import Text.Parsel.ParseError
  ( ParseError (ParseError),
    ParseErrorInfo (ExnChrMismatch, ExnEndOfFile, ExnEvalBottom),
  )

-- Running Eval ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
evalST :: String -> (forall s. Parse s a) -> Either ParseError a
evalST src m = runST do
  let ctx = ParseCtx src
  let env = ParseStore SrcLoc.empty []
  result <- runParseST ctx env m
  pure (snd result)

-- | TODO
--
-- @since 1.0.0
evalIO :: String -> ParseIO a -> IO (Either ParseError a)
evalIO src m =
  let ctx = ParseCtx src
      env = ParseStore SrcLoc.empty []
   in fmap snd (runParseIO ctx env m)

-- Evaluating Terms ------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
evalTerm :: forall a s. Grammar a -> Parse s a
evalTerm Bot = raiseBot
evalTerm Loc = gets store'location
evalTerm Eps = pure ()
evalTerm (Chr chr) = single chr $> chr
evalTerm (Map f x) = fmap f (evalTerm x)
evalTerm (Seq x y) = liftA2 (,) (evalTerm x) (evalTerm y)
evalTerm (Alt x y) = alt (evalTerm x) (evalTerm y)
evalTerm (Fix fix) = evalTerm (fix (Fix fix))

-- Errors ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
raiseEoF :: Parse s a
raiseEoF = do
  src <- asks ctx'source
  loc <- gets store'location
  throwError (ParseError ExnEndOfFile loc loc src)

-- | TODO
--
-- @since 1.0.0
raiseBot :: Parse s a
raiseBot = do
  src <- asks ctx'source
  loc <- gets store'location
  throwError (ParseError ExnEvalBottom loc loc src)

-- | TODO
--
-- @since 1.0.0
raiseChrMismatch :: Char -> Parse s a
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
single :: Char -> Parse s ()
single chr = do
  chr' <- peek
  if chr == chr'
    then advance
    else raiseChrMismatch chr

-- | TODO
--
-- @since 1.0.0
advance :: Parse s ()
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
peek :: Parse s Char
peek = do
  src <- asks ctx'source
  pos <- gets (posn . store'location)
  if pos >= length src
    then raiseEoF
    else pure (src List.!! pos)

-- | TODO
--
-- @since 1.0.0
alt :: Parse s a -> Parse s a -> Parse s a
alt (Parse f) (Parse g) =
  Parse \ctx env0 st0# -> case f ctx env0 st0# of
    (# st1#, env1, (# e | #) #) -> g ctx env1{store'location = env0.store'location, store'branches = e : env1.store'branches} st1#
    (# st1#, env1, (# | x #) #) -> (# st1#, env1, (# | x #) #)