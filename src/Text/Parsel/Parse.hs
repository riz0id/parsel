{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Text.Parsel.Parse (
  module Text.Parsel.Parse.Core,

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
) where

import Control.Applicative (empty, liftA2, (<|>))

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, local)
import Control.Monad.ST (runST)
import Control.Monad.State (gets, modify')

import Data.Functor (($>))
import Data.SrcLoc (posn)
import Data.Text qualified as Text
import Data.Text.Array qualified as Text.Array
import Data.Text.Internal (Text (Text))
import Data.Text.Internal.Encoding.Utf8 qualified as Text.Utf8
import Data.Word (Word8)

import Prelude hiding (take)

--------------------------------------------------------------------------------

import Text.Parsel.Grammar.Core (
  Grammar (Alt, Bot, Chr, Eps, Fix, Lab, Loc, Map, Mat, Seq, Str),
  Match,
  member,
 )
import Text.Parsel.Parse.Core
import Text.Parsel.Parse.ParseContext (
  ParseContext (context'label),
  context'length,
  context'source,
  newParseContext,
  relabelParseContext,
 )
import Text.Parsel.Parse.ParseState (
  feedParseState,
  feedsParseState,
  newParseState,
  state'location,
  state'offset,
 )
import Text.Parsel.ParseError (ParseError)
import Text.Parsel.ParseError qualified as ParseError
import GHC.Exts (Int(I#))
import qualified GHC.Exts as GHC

-- Running Eval ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
evalST :: Text -> Text -> (forall s. Parse s a) -> Either ParseError a
evalST label source m = runST do
  let ctx = newParseContext label source
  let env = newParseState
  result <- runPrimParse ctx env m
  pure (snd result)
{-# INLINE evalST #-}

-- | TODO
--
-- @since 1.0.0
evalIO :: Text -> Text -> ParseIO a -> IO (Either ParseError a)
evalIO label source m = do
  let ctx = newParseContext label source
  let env = newParseState
  result <- runPrimParse ctx env m
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
evalGrammar (Str str) = string str
evalGrammar (Mat mat) = satisfy mat
evalGrammar (Lab s x) = local (relabelParseContext s) (evalGrammar x)
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
  lab <- asks context'label
  src <- asks (Text.takeWhile (/= '\n') . context'source)
  throwError (ParseError.makeExnEndOfFile lab loc src)
{-# INLINE raiseEoF #-}

-- | TODO
--
-- @since 1.0.0
raiseChrMismatch :: Char -> Parse s a
raiseChrMismatch chr = do
  loc <- gets state'location
  lab <- asks context'label
  src <- asks (Text.takeWhile (/= '\n') . context'source)
  throwError (ParseError.makeExnChar lab loc chr src)
{-# INLINE raiseChrMismatch #-}

-- | TODO
--
-- @since 1.0.0
raiseStrMismatch :: Text -> Parse s a
raiseStrMismatch text = do
  loc <- gets state'location
  lab <- asks context'label
  src <- asks (Text.takeWhile (/= '\n') . context'source)
  throwError (ParseError.makeExnString lab loc text src)
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
    else raiseChrMismatch match
{-# INLINE single #-}

-- | TODO
--
-- @since 1.0.0
string :: Text -> Parse s Text
string text@(Text _ i0 i1) = do
  substr <- take (i1 - i0)
  if text == substr
    then text <$ advances text
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
advances :: Text -> Parse s ()
advances text = modify' (feedsParseState text)
{-# INLINE advances #-}

-- | TODO
--
-- @since 1.0.0
peek :: Parse s Char
peek = do
  pos <- gets (posn . state'location)
  len <- asks context'length
  if pos < len
    then unsafePeek
    else raiseEoF
{-# INLINE peek #-}

unsafePeek :: Parse s Char
unsafePeek = do
  Text src _ _ <- asks context'source
  offset <- gets state'offset
  let byte0 :: Word8
      byte0 = Text.Array.unsafeIndex src offset
   in case Text.Utf8.utf8LengthByLeader byte0 of
        1 -> pure (toEnum (fromIntegral byte0))
        2 ->
          let byte1 = Text.Array.unsafeIndex src (1 + offset)
           in pure (Text.Utf8.chr2 byte0 byte1)
        3 ->
          let byte1 = Text.Array.unsafeIndex src (1 + offset)
              byte2 = Text.Array.unsafeIndex src (2 + offset)
           in pure (Text.Utf8.chr3 byte0 byte1 byte2)
        _ ->
          let byte1 = Text.Array.unsafeIndex src (1 + offset)
              byte2 = Text.Array.unsafeIndex src (2 + offset)
              byte3 = Text.Array.unsafeIndex src (3 + offset)
           in pure (Text.Utf8.chr4 byte0 byte1 byte2 byte3)
{-# INLINE unsafePeek #-}

-- | TODO
--
-- @since 1.0.0
take :: Int -> Parse s Text
take n@(I# n#) = do
  Text src@(Text.Array.ByteArray src#) _ _ <- asks context'source
  off@(I# off#) <- gets state'offset
  case n# GHC.+# off# GHC.<=# GHC.sizeofByteArray# src# of
    1# -> pure (Text src off n)
    _ -> raiseEoF
{-# INLINE take #-}
