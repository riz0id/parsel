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
    evalGrammar,

    -- * Errors
    raiseChrMismatch,
    raiseEoF,

    -- * Operations
    satisfy,
    advance,
  )
where

import Control.Applicative (empty, liftA2, (<|>))

import Control.Monad.Except (throwError)
import Control.Monad.ST (runST)
import Control.Monad.State (gets, modify')

import Data.Functor (($>))
import Data.SrcLoc (posn)
import Data.Text qualified as Text
import Data.Text (Text)

import Prelude hiding (take)

--------------------------------------------------------------------------------

import Text.Parsel.Grammar.Core
  ( Grammar (Alt, Bot, Chr, Eps, Fix, Loc, Map, Mat, Seq, Str),
    Match,
    member,
  )
import Text.Parsel.Parse.Core
import Text.Parsel.ParseError (ParseError)
import Text.Parsel.ParseError qualified as ParseError

-- Running Eval ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
evalST :: Text -> (forall s. Parse s a) -> Either ParseError a
evalST src m = runST do
  result <- runPrimParse (newParseState src) m
  pure (snd result)
{-# INLINE evalST #-}

-- | TODO
--
-- @since 1.0.0
evalIO :: Text -> ParseIO a -> IO (Either ParseError a)
evalIO src m = do
  result <- runPrimParse (newParseState src) m
  pure (snd result)
{-# INLINE evalIO #-}

-- Evaluating Terms ------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
evalGrammar :: forall a s. Grammar a -> Parse s a
evalGrammar Bot = empty
evalGrammar Loc = gets state'location
evalGrammar Eps = pure ()
evalGrammar (Chr chr) = single chr
evalGrammar (Str len str) = string len str
evalGrammar (Mat mat) = satisfy mat
evalGrammar (Map f x) = fmap f (evalGrammar x)
evalGrammar (Seq x y) = liftA2 (,) (evalGrammar x) (evalGrammar y)
evalGrammar (Alt x y) = evalGrammar x <|> evalGrammar y
evalGrammar (Fix f) = evalGrammar (f (Fix f))
{-# INLINE evalGrammar #-}


-- Errors ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
raiseEoF :: Parse s a
raiseEoF = do
  loc <- gets state'location
  src <- gets state'source
  throwError (ParseError.makeExnEndOfFile loc src)
{-# INLINE raiseEoF #-}

-- | TODO
--
-- @since 1.0.0
raiseChrMismatch :: Char -> Parse s a
raiseChrMismatch chr = do
  loc <- gets state'location
  src <- gets state'source
  throwError (ParseError.makeExnChar loc chr src)
{-# INLINE raiseChrMismatch #-}

-- | TODO
--
-- @since 1.0.0
raiseStrMismatch :: Text -> Parse s a
raiseStrMismatch text = do
  loc <- gets state'location
  src <- gets state'source
  throwError (ParseError.makeExnString loc text src)
{-# INLINE raiseStrMismatch #-}

-- Operations ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
single :: Char -> Parse s Char
single match = do
  chr <- peek
  if chr == match
    then advance chr $> chr
    else raiseChrMismatch chr
{-# INLINE single #-}

-- | TODO
--
-- @since 1.0.0
string :: Int -> Text -> Parse s Text
string size text = do
  substr <- take size 
  if text == substr
    then text <$ advances size text
    else raiseStrMismatch text
{-# INLINE string #-}

-- | TODO
--
-- @since 1.0.0
satisfy :: Match -> Parse s Char
satisfy match = do
  chr <- peek
  if member chr match
    then advance chr $> chr
    else raiseChrMismatch chr
{-# INLINE satisfy #-}

-- | TODO
--
-- @since 1.0.0
advance :: Char -> Parse s ()
advance chr = modify' (feedParseState chr)
{-# INLINE advance #-}

-- | TODO
--
-- @since 1.0.0
advances :: Int -> Text -> Parse s ()
advances size text = modify' (feedsParseState size text)
{-# INLINE advances #-}

-- | TODO
--
-- @since 1.0.0
peek :: Parse s Char
peek = do
  src <- gets state'source
  len <- gets state'length
  pos <- gets (posn . state'location)
  if pos < len
    then pure (Text.head src)
    else raiseEoF 
{-# INLINE peek #-}

-- | TODO
--
-- @since 1.0.0
take :: Int -> Parse s Text
take n = do
  src <- gets state'source
  len <- gets state'length
  pos <- gets (posn . state'location)
  if n + pos <= len
    then pure (Text.take n src)
    else raiseEoF
{-# INLINE take #-}