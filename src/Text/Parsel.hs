{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}

module Text.Parsel
  ( -- * TODO 
    Parse,

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

    -- * Choice
    between,
  )
where

import Data.Functor (($>))

--------------------------------------------------------------------------------

import Text.Parsel.Core (Parse (Alt, Chr, Str))

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
between :: Parse a -> Parse b -> Parse c -> Parse c
between tml tmr tm = tml *> tm <* tmr
{-# INLINE CONLIKE between #-}

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
string = Str
{-# INLINE CONLIKE string #-}
