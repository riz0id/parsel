{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Parsel.Grammar.Core
  ( -- * Grammar
    Grammar (Bot, Loc, Eps, Chr, Str, Mat, Lab, Map, Seq, Alt, Fix),

    -- * Match
    Match (Match, Lower, Upper, Space, Alpha, Digit),
    member,
  )
where

import Control.Applicative (Alternative, empty, liftA2, many, some, (<|>))

import Control.Exception (assert)

import Data.Char qualified as Char
import Data.Data (Data)
import Data.Kind (Type)
import Data.SrcLoc (SrcLoc)
import Data.Text (Text)

import Language.Haskell.TH.Syntax (Lift)

-- Grammars --------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Grammar (a :: Type) :: Type where
  Bot :: Grammar a
  Loc :: Grammar SrcLoc
  Eps :: Grammar ()
  Chr :: {-# UNPACK #-} !Char -> Grammar Char
  Str :: {-# UNPACK #-} !Text -> Grammar Text
  Mat :: Match -> Grammar Char
  Lab :: {-# UNPACK #-} !Text -> Grammar a -> Grammar a
  Map :: (a -> b) -> Grammar a -> Grammar b
  Seq :: Grammar a -> Grammar b -> Grammar (a, b)
  Alt :: Grammar a -> Grammar a -> Grammar a
  Fix :: (Grammar a -> Grammar a) -> Grammar a

-- | @since 1.0.0
instance Show (Grammar a) where
  show Bot = "Bot"
  show Loc = "Loc"
  show Eps = "Eps"
  show (Chr chr) = "(Chr " ++ show chr ++ ")"
  show (Str str) = "(Str " ++ show str ++ ")"
  show (Mat x) = show x
  show (Lab s x) = "(Lab " ++ show s ++ " " ++ show x ++ ")"
  show (Map _ x) = "(Map " ++ show x ++ ")"
  show (Seq x y) = "(Seq " ++ show x ++ " " ++ show y ++ ")"
  show (Alt x y) = "(Alt " ++ show x ++ " " ++ show y ++ ")"
  show (Fix f) = "Fix " ++ show (f Bot)
  {-# INLINE show #-}

-- | @since 1.0.0
instance Functor Grammar where
  fmap = Map
  {-# INLINE CONLIKE fmap #-}

  (<$) x = Map (const x) 
  {-# INLINE CONLIKE (<$) #-}

-- | @since 1.0.0
instance Applicative Grammar where
  pure x = Map (const x) Eps 
  {-# INLINE CONLIKE pure #-}

  f <*> x = Map (uncurry ($)) (Seq f x)
  {-# INLINE CONLIKE (<*>) #-}

  liftA2 f x y = Map (uncurry f) (Seq x y)
  {-# INLINE CONLIKE liftA2 #-}

  x *> y = Map snd (Seq x y)
  {-# INLINE CONLIKE (*>) #-}

  x <* y = Map fst (Seq x y)
  {-# INLINE CONLIKE (<*) #-}

-- | @since 1.0.0
instance Alternative Grammar where
  empty = Bot
  {-# INLINE CONLIKE empty #-}

  (<|>) = Alt
  {-# INLINE CONLIKE (<|>) #-}

  many x = Fix \xs -> liftA2 (:) x xs <|> pure []
  {-# INLINE CONLIKE many #-}

  some x = liftA2 (:) x (many x)
  {-# INLINE CONLIKE some #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Match
  = Match {-# UNPACK #-} !Char {-# UNPACK #-} !Char
  | Lower
  | Upper
  | Space
  | Alpha
  | Digit
  deriving (Data, Eq, Lift, Ord, Show)

-- | TODO
--
-- @since 1.0.0
member :: Char -> Match -> Bool
member chr (Match a b) = assert (a <= b) (a <= chr && chr <= b)
member chr Lower = Char.isLower chr
member chr Upper = Char.isUpper chr
member chr Space = Char.isSpace chr
member chr Alpha = Char.isAlpha chr
member chr Digit = Char.isDigit chr
{-# INLINE member #-}

--------------------------------------------------------------------------------


  