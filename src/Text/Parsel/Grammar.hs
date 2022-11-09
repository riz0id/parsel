{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}

module Text.Parsel.Grammar (
  -- * TODO
  module Text.Parsel.Grammar.Core,

  -- * Primitive Queries
  location,
  position,
  line,
  column,

  -- * Primitive Grammars
  char,
  lower,
  upper,
  alpha,
  digit,
  alphaNum,
  string,
  text,
) where

import Control.Applicative ((<|>))

import Data.SrcLoc (SrcLoc)
import Data.SrcLoc qualified as SrcLoc
import Data.Text (Text)

import Text.Emit.Doc.Prim (makeText)

--------------------------------------------------------------------------------

import Text.Parsel.Grammar.Core

-- Primitive Queries -----------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
location :: Grammar SrcLoc
location = Loc
{-# INLINE CONLIKE location #-}

-- | TODO
--
-- @since 1.0.0
position :: Grammar Int
position = Map SrcLoc.line Loc
{-# INLINE CONLIKE position #-}

-- | TODO
--
-- @since 1.0.0
line :: Grammar Int
line = Map SrcLoc.line Loc
{-# INLINE CONLIKE line #-}

-- | TODO
--
-- @since 1.0.0
column :: Grammar Int
column = Map SrcLoc.line Loc
{-# INLINE CONLIKE column #-}

-- Primitive Grammers ----------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
char :: Char -> Grammar Char
char = Chr
{-# INLINE CONLIKE char #-}

-- | TODO
--
-- @since 1.0.0
lower :: Grammar Char
lower = Mat Lower
{-# INLINE CONLIKE lower #-}

-- | TODO
--
-- @since 1.0.0
upper :: Grammar Char
upper = Mat Upper
{-# INLINE CONLIKE upper #-}

-- | TODO
--
-- @since 1.0.0
alpha :: Grammar Char
alpha = Mat Alpha
{-# INLINE CONLIKE alpha #-}

-- | TODO
--
-- @since 1.0.0
digit :: Grammar Char
digit = Mat Digit
{-# INLINE CONLIKE digit #-}

-- | TODO
--
-- @since 1.0.0
alphaNum :: Grammar Char
alphaNum = alpha <|> digit
{-# INLINE CONLIKE alphaNum #-}

-- | TODO
--
-- @since 1.0.0
string :: String -> Grammar Text
string x = Str (makeText x)
{-# INLINE CONLIKE string #-}

-- | TODO
--
-- @since 1.0.0
text :: Text -> Grammar Text
text = Str
{-# INLINE CONLIKE text #-}