{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Text.Parsel.Grammar.Core
  ( -- * Terms
    Grammar (Bot, Loc, Eps, Chr, Map, Seq, Alt, Fix),
  )
where

import Control.Applicative (Alternative, empty, liftA2, many, some, (<|>))

import Data.Kind (Type)
import Data.SrcLoc (SrcLoc)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Grammar (a :: Type) :: Type where
  Bot :: Grammar a
  Loc :: Grammar SrcLoc
  Eps :: Grammar ()
  Chr :: {-# UNPACK #-} !Char -> Grammar Char
  Map :: (a -> b) -> Grammar a -> Grammar b
  Seq :: Grammar a -> Grammar b -> Grammar (a, b)
  Alt :: Grammar a -> Grammar a -> Grammar a
  Fix :: (Grammar a -> Grammar a) -> Grammar a

-- | @since 1.0.0
instance Functor Grammar where
  fmap f x = Map f x
  {-# INLINE CONLIKE fmap #-}

  (<$) x = Map (\_ -> x)
  {-# INLINE (<$) #-}

-- | @since 1.0.0
instance Applicative Grammar where
  pure x = Map (\_ -> x) Eps
  {-# INLINE CONLIKE pure #-}

  x <*> y = Map (uncurry ($)) (Seq x y)
  {-# INLINE CONLIKE (<*>) #-}

  liftA2 f x y = Map (uncurry f) (Seq x y)
  {-# INLINE CONLIKE liftA2 #-}

-- | @since 1.0.0
instance Alternative Grammar where
  empty = Bot
  {-# INLINE CONLIKE empty #-}

  a <|> b = Alt a b
  {-# INLINE CONLIKE (<|>) #-}

  many x = Fix \xs -> Alt (liftA2 (:) x xs) (pure []) 
  {-# INLINE CONLIKE many #-}

  some x = liftA2 (:) x (many x)
  {-# INLINE CONLIKE some #-}