{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}

module Text.Parsel
  ( -- * TODO
    Parse,

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

import Text.Parsel.Core (Parse (Alt, Chr, Loc, Map))
import Text.Parsel.Eval (evalST, evalTerm, evalIO)
import Text.Parsel.ParseError (ParseError (..), ParseErrorInfo (..))

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
parse :: String -> Parse a -> Either ParseError a
parse input p = evalST input (evalTerm p)

-- | TODO
--
-- @since 1.0.0
parseIO :: String -> Parse a -> IO (Either ParseError a)
parseIO input p = evalIO input (evalTerm p)

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
location :: Parse SrcLoc
location = Loc
{-# INLINE CONLIKE location #-}

-- | TODO
--
-- @since 1.0.0
position :: Parse Int
position = Map SrcLoc.line Loc
{-# INLINE CONLIKE position #-}

-- | TODO
--
-- @since 1.0.0
line :: Parse Int
line = Map SrcLoc.line Loc
{-# INLINE CONLIKE line #-}

-- | TODO
--
-- @since 1.0.0
column :: Parse Int
column = Map SrcLoc.line Loc
{-# INLINE CONLIKE column #-}

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
char :: Char -> Parse Char
char = Chr
{-# INLINE CONLIKE char #-}

-- | TODO
--
-- @since 1.0.0
lower :: Parse Char
lower = foldr (Alt . Chr) (Chr 'a') ['b' .. 'z']
{-# INLINE CONLIKE lower #-}

-- | TODO
--
-- @since 1.0.0
upper :: Parse Char
upper = foldr (Alt . Chr) (Chr 'A') ['B' .. 'Z']
{-# INLINE CONLIKE upper #-}

-- | TODO
--
-- @since 1.0.0
alpha :: Parse Char
alpha = Alt lower upper
{-# INLINE CONLIKE alpha #-}

-- | TODO
--
-- @since 1.0.0
digit :: Parse Char
digit = foldr (Alt . Chr) (Chr '0') ['1' .. '9']
{-# INLINE CONLIKE digit #-}

-- | TODO
--
-- @since 1.0.0
alphaNum :: Parse Char
alphaNum = Alt alpha digit
{-# INLINE CONLIKE alphaNum #-}

-- | TODO
--
-- @since 1.0.0
whitespace :: Parse ()
whitespace = foldr (Alt . Chr) (Chr ' ') "\t\r\n" $> ()

-- | TODO
--
-- @since 1.0.0
string :: String -> Parse String
string = foldr (liftA2 (:) . Chr) (pure "")
{-# INLINE CONLIKE string #-}

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
between :: Parse l -> Parse r -> Parse c -> Parse c
between tml tmr tm = tml *> tm <* tmr
{-# INLINE CONLIKE between #-}

-- | TODO
--
-- @since 1.0.0
surround :: Parse x -> Parse a -> Parse a
surround tm = between tm tm
{-# INLINE CONLIKE surround #-}

-- | TODO
--
-- @since 1.0.0
parentheses :: Parse a -> Parse a
parentheses = between (char '(') (char ')')
{-# INLINE CONLIKE parentheses #-}

-- | TODO
--
-- @since 1.0.0
brackets :: Parse a -> Parse a
brackets = between (char '[') (char ']')
{-# INLINE CONLIKE brackets #-}

-- | TODO
--
-- @since 1.0.0
braces :: Parse a -> Parse a
braces = between (char '{') (char '}')
{-# INLINE CONLIKE braces #-}

-- | TODO
--
-- @since 1.0.0
angles :: Parse a -> Parse a
angles = between (char '<') (char '>')
{-# INLINE CONLIKE angles #-}

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
choice :: NonEmpty (Parse a) -> Parse a
choice (tm :| tms) = foldr (<|>) tm tms
{-# INLINE CONLIKE choice #-}
