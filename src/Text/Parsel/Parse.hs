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
  satisfyP,
  advance,
) where

import Control.Applicative (empty, liftA2, (<|>))

import Control.Lens (over)

import Control.Monad.Except (throwError, catchError)
import Control.Monad.Reader (asks, local)
import Control.Monad.ST (runST)
import Control.Monad.State (gets, modify')

import Data.Char qualified as Char
import Data.Text.Array qualified as Text
import Data.Text.Array qualified as Text.Array
import Data.Text.Internal (Text (Text))
import Data.Text.Internal.Encoding.Utf8 qualified as Text.Utf8
import Data.Word (Word8)

import GHC.Exts (Char (C#), Int (I#))
import GHC.Exts qualified as GHC

import Prelude hiding (take)

--------------------------------------------------------------------------------

import Text.Parsel.Grammar.Core (
  Grammar (Alt, Bot, Chr, Eps, Fix, Lab, Loc, Map, Mat, Seq, Str),
  Match (Alpha, Digit, Lower, Match, Space, Upper),
 )
import Text.Parsel.Parse.Core
import Text.Parsel.Parse.ParseLoc qualified as ParseLoc
import Text.Parsel.ParseError qualified as ParseError
import qualified Data.SrcSpan as SrcSpan

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
evalGrammar Loc = gets (ParseLoc.srcloc . state'end)
evalGrammar Eps = pure ()
evalGrammar (Chr chr) = single chr
evalGrammar (Str str) = string str
evalGrammar (Mat mat) = satisfy mat
evalGrammar (Lab s x) = local (relabelParseContext s) (evalGrammar x)
evalGrammar (Map f x) = fmap f (evalGrammar x)
evalGrammar (Seq x y) = liftA2 (,) (evalGrammar x) (evalGrammar y)
evalGrammar (Alt x y) = evalGrammar x <|> evalGrammar y
evalGrammar (Fix f) = do 
  loc0 <- fmap SrcSpan.begin getSourceSpan
  lab <- asks context'label
  catchError (evalGrammar (f (Fix f))) \exn -> do 
    loc1 <- fmap SrcSpan.end getSourceSpan
    let sp = SrcSpan.SrcSpan loc0 loc1
    throwError exn {exn'label = lab, exn'span = sp}
{-# INLINE evalGrammar #-}

-- Errors ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
raiseEoF :: Parse s a
raiseEoF = do
  sp <- getSourceSpan 
  lab <- asks context'label
  src <- parseSourceText
  throwError (ParseError.makeExnEndOfFile lab sp src)
{-# INLINE raiseEoF #-}

-- | TODO
--
-- @since 1.0.0
raiseChrMismatch :: Char -> Parse s a
raiseChrMismatch chr = do
  sp <- getSourceSpan 
  lab <- asks context'label
  src <- parseSourceText
  throwError (ParseError.makeExnChar lab sp chr src)
{-# INLINE raiseChrMismatch #-}

-- | TODO
--
-- @since 1.0.0
raiseStrMismatch :: Text -> Parse s a
raiseStrMismatch text = do
  loc <- gets (ParseLoc.srcloc . state'end)
  lab <- asks context'label
  src <- parseSourceText
  throwError (ParseError.makeExnString lab loc text src)
{-# INLINE raiseStrMismatch #-}

-- Operations ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
single :: Char -> Parse s Char
single match = do
  -- modify' \st -> st {state'begin = state'end st}
  chr <- peek
  advance chr
  if chr == match
    then pure chr
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

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
satisfy :: Match -> Parse s Char
satisfy (Match a b) = satisfyP (\chr -> a <= chr && chr <= b) (\chr -> chr <$ advance chr)
satisfy Lower = satisfyP Char.isLower (<$ modify' (over stwEnd ParseLoc.nextColn1))
satisfy Upper = satisfyP Char.isUpper (<$ modify' (over stwEnd ParseLoc.nextColn1))
satisfy Digit = satisfyP Char.isDigit (<$ modify' (over stwEnd ParseLoc.nextColn1))
satisfy Alpha = satisfyP Char.isAlpha (<$ modify' (over stwEnd ParseLoc.nextColn1))
satisfy Space = satisfyP Char.isSpace (\chr -> chr <$ advance chr)
{-# INLINE satisfy #-}

satisfyP :: (Char -> Bool) -> (Char -> Parse s Char) -> Parse s Char
satisfyP predicate yield = do
  chr <- peek1
  if predicate chr
    then yield chr
    else raiseChrMismatch chr
{-# INLINE satisfyP #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
peek1 :: Parse s Char
peek1 =
  Parse \ctx env st# ->
    let !(I# pos) = ParseLoc.offset (state'end env)
        !(I# len) = context'length ctx
     in case pos GHC.<# len of
          1# -> unParse unsafePeek1 ctx env st#
          _ -> (# st#, env, (# | '\NUL' #) #)

unsafePeek1 :: Parse s Char
unsafePeek1 =
  Parse \ctx env st# ->
    let !(Text.ByteArray buffer#) = context'buffer ctx
        !(I# offset#) = ParseLoc.offset (state'end env)
     in (# st#, env, (# | C# (GHC.indexCharArray# buffer# offset#) #) #)

-- | TODO
--
-- @since 1.0.0
peek :: Parse s Char
peek =
  Parse \ctx env st# ->
    let !(I# pos) = ParseLoc.offset (state'end env)
        !(I# len) = context'length ctx
     in case pos GHC.<# len of
          1# -> unParse unsafePeek ctx env st#
          _ -> (# st#, env, (# | '\NUL' #) #)
{-# INLINE peek #-}

unsafePeek :: Parse s Char
unsafePeek = do
  buffer <- asks context'buffer
  offset <- gets (ParseLoc.offset . state'end)
  let byte0 :: Word8
      byte0 = Text.Array.unsafeIndex buffer offset
   in case Text.Utf8.utf8LengthByLeader byte0 of
        1 -> unsafePeek1
        2 ->
          let byte1 = Text.Array.unsafeIndex buffer (1 + offset)
           in pure (Text.Utf8.chr2 byte0 byte1)
        3 ->
          let byte1 = Text.Array.unsafeIndex buffer (1 + offset)
              byte2 = Text.Array.unsafeIndex buffer (2 + offset)
           in pure (Text.Utf8.chr3 byte0 byte1 byte2)
        _ ->
          let byte1 = Text.Array.unsafeIndex buffer (1 + offset)
              byte2 = Text.Array.unsafeIndex buffer (2 + offset)
              byte3 = Text.Array.unsafeIndex buffer (3 + offset)
           in pure (Text.Utf8.chr4 byte0 byte1 byte2 byte3)
{-# INLINE unsafePeek #-}

--------------------------------------------------------------------------------

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
take :: Int -> Parse s Text
take n@(I# n#) = do
  I# len# <- asks context'length
  I# off# <- gets (ParseLoc.offset . state'end)
  case n# GHC.+# off# GHC.<=# len# of
    1# -> asks \ctx -> Text (context'buffer ctx) (I# off#) n
    _ -> raiseEoF
{-# INLINE take #-}