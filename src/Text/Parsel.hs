{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Text.Parsel
  ( -- * TODO
    Grammar,

    -- ** Parse Errors
    ParseError (ParseError, exn'kind, exn'span, exn'source),

    -- ** Parse Error Info
    ParseErrorInfo (ExnEoF, ExnBot, ExnChr, ExnStr),

    -- * TODO
    parse,
    parseIO,

    -- * TODO
    location,
    position,
    line,
    column,

    -- * Characters
    char,
    lower,
    upper,
    alpha,
    digit,
    alphaNum,

    -- * Strings
    string,

    -- * Whitespace
    whitespace,
    whitespaces,
    whitespaces1,

    -- * TODO
    between,
    parentheses,
    brackets,
    braces,
    angles,

    -- * Choice
    choice,
  )
where

import Control.Applicative (empty, many, some, (<|>))

import Data.Foldable (foldr')
import Data.SrcLoc (SrcLoc)
import Data.Functor (void)
import Data.SrcLoc qualified as SrcLoc
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

import Text.Parsel.Grammar.Core
  ( Grammar (Chr, Loc, Map, Mat, Str),
    Match (Alpha, Digit, Lower, Space, Upper),
  )
import Text.Parsel.Parse (evalIO, evalST, evalGrammar)
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

-- TODO ------------------------------------------------------------------------

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

-- TODO ------------------------------------------------------------------------

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
string :: Text -> Grammar Text
string = Str 
{-# INLINE CONLIKE string #-}

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
parentheses :: Grammar a -> Grammar a
parentheses x = char '(' *> x <* char ')'
{-# INLINE CONLIKE parentheses #-}

-- | TODO
--
-- @since 1.0.0
brackets :: Grammar a -> Grammar a
brackets x = char '[' *> x <* char ']'
{-# INLINE CONLIKE brackets #-}

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
