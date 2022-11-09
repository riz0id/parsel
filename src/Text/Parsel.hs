{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Text.Parsel (
  -- * TODO
  module Text.Parsel.Grammar,

  -- ** Parse Errors
  ParseError (ParseError, exn'kind, exn'span, exn'source),

  -- ** Parse Error Info
  ParseErrorInfo (ExnEoF, ExnBot, ExnChr, ExnStr),

  -- * TODO
  parse,
  parseIO,

  -- * Whitespace
  whitespace,
  whitespaces,
  whitespaces1,

  -- * TODO
  between,
  parens,
  bracks,
  braces,
  angles,

  -- * Choice
  choice,
) where

import Control.Applicative (empty, many, some, (<|>))

import Data.Foldable (foldr')
import Data.Functor (void)
import Data.Text (Text)

--------------------------------------------------------------------------------

import Text.Parsel.Grammar
import Text.Parsel.Parse (evalGrammar, evalIO, evalST)
import Text.Parsel.ParseError (ParseError (..), ParseErrorInfo (..))

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
parse :: Text -> Text -> Grammar a -> Either ParseError a
parse label source p = evalST label source (evalGrammar p)

-- | TODO
--
-- @since 1.0.0
parseIO :: Text -> Text -> Grammar a -> IO (Either ParseError a)
parseIO label source p = evalIO label source (evalGrammar p)

-- Whitespace ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
whitespace :: Grammar ()
whitespace = void (Mat Space)
{-# INLINE CONLIKE whitespace #-}

-- | TODO
--
-- @since 1.0.0
whitespaces :: Grammar ()
whitespaces = void (many whitespace)
{-# INLINE CONLIKE whitespaces #-}

-- | TODO
--
-- @since 1.0.0
whitespaces1 :: Grammar ()
whitespaces1 = void (some whitespace)
{-# INLINE CONLIKE whitespaces1 #-}

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
between :: Grammar l -> Grammar r -> Grammar c -> Grammar c
between tml tmr tm = tml *> tm <* tmr
{-# INLINE CONLIKE between #-}

-- | TODO
--
-- @since 1.0.0
parens :: Grammar a -> Grammar a
parens x = char '(' *> x <* char ')'
{-# INLINE CONLIKE parens #-}

-- | TODO
--
-- @since 1.0.0
bracks :: Grammar a -> Grammar a
bracks x = char '[' *> x <* char ']'
{-# INLINE CONLIKE bracks #-}

-- | TODO
--
-- @since 1.0.0
braces :: Grammar a -> Grammar a
braces x = char '{' *> x <* char '}'
{-# INLINE CONLIKE braces #-}

-- | TODO
--
-- @since 1.0.0
angles :: Grammar a -> Grammar a
angles x = char '<' *> x <* char '>'
{-# INLINE CONLIKE angles #-}

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
choice :: Foldable f => f (Grammar a) -> Grammar a
choice = foldr' (<|>) empty
{-# INLINE CONLIKE choice #-}
{-# SPECIALIZE INLINE choice :: [Grammar a] -> Grammar a #-}
