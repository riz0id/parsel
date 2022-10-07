{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}

module Text.Parsel
  ( -- * TODO
    Grammar,

    -- ** Parse Errors
    ParseError (ParseError, exn'kind, exn'begin, exn'end, exn'source),

    -- ** Parse Error Info
    ParseErrorInfo
      ( ExnEndOfFile,
        ExnEvalBottom,
        ExnChrMismatch,
        ExnStrMismatch
      ),

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
    whitespace,

    -- * Strings
    string,

    -- * TODO
    between,
    surround,
    parentheses,
    brackets,
    braces,
    angles,

    -- * Choice
    choice,
  )
where

import Control.Applicative ((<|>), liftA2)

import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.SrcLoc (SrcLoc)
import Data.SrcLoc qualified as SrcLoc

--------------------------------------------------------------------------------

import Text.Parsel.Grammar.Core (Grammar (Alt, Chr, Loc, Map))
import Text.Parsel.Parse (evalST, evalTerm, evalIO)
import Text.Parsel.ParseError (ParseError (..), ParseErrorInfo (..))

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
parse :: String -> Grammar a -> Either ParseError a
parse input p = evalST input (evalTerm p)

-- | TODO
--
-- @since 1.0.0
parseIO :: String -> Grammar a -> IO (Either ParseError a)
parseIO input p = evalIO input (evalTerm p)

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
lower = foldr (Alt . Chr) (Chr 'a') ['b' .. 'z']
{-# INLINE CONLIKE lower #-}

-- | TODO
--
-- @since 1.0.0
upper :: Grammar Char
upper = foldr (Alt . Chr) (Chr 'A') ['B' .. 'Z']
{-# INLINE CONLIKE upper #-}

-- | TODO
--
-- @since 1.0.0
alpha :: Grammar Char
alpha = Alt lower upper
{-# INLINE CONLIKE alpha #-}

-- | TODO
--
-- @since 1.0.0
digit :: Grammar Char
digit = foldr (Alt . Chr) (Chr '0') ['1' .. '9']
{-# INLINE CONLIKE digit #-}

-- | TODO
--
-- @since 1.0.0
alphaNum :: Grammar Char
alphaNum = Alt alpha digit
{-# INLINE CONLIKE alphaNum #-}

-- | TODO
--
-- @since 1.0.0
whitespace :: Grammar ()
whitespace = foldr (Alt . Chr) (Chr ' ') "\t\r\n" $> ()

-- | TODO
--
-- @since 1.0.0
string :: String -> Grammar String
string = foldr (liftA2 (:) . Chr) (pure "")
{-# INLINE CONLIKE string #-}

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
surround :: Grammar x -> Grammar a -> Grammar a
surround tm = between tm tm
{-# INLINE CONLIKE surround #-}

-- | TODO
--
-- @since 1.0.0
parentheses :: Grammar a -> Grammar a
parentheses = between (char '(') (char ')')
{-# INLINE CONLIKE parentheses #-}

-- | TODO
--
-- @since 1.0.0
brackets :: Grammar a -> Grammar a
brackets = between (char '[') (char ']')
{-# INLINE CONLIKE brackets #-}

-- | TODO
--
-- @since 1.0.0
braces :: Grammar a -> Grammar a
braces = between (char '{') (char '}')
{-# INLINE CONLIKE braces #-}

-- | TODO
--
-- @since 1.0.0
angles :: Grammar a -> Grammar a
angles = between (char '<') (char '>')
{-# INLINE CONLIKE angles #-}

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
choice :: NonEmpty (Grammar a) -> Grammar a
choice (tm :| tms) = foldr (<|>) tm tms
{-# INLINE CONLIKE choice #-}
