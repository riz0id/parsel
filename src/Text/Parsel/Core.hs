{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Text.Parsel.Core
  ( -- * Terms
    Parse (Bot, Loc, Val, Chr, Str, Map, Seq, Alt, Fix),
  )
where

import Control.Applicative (liftA2, many, some, Alternative, empty, (<|>))

import Data.Kind (Type)
import Data.SrcLoc (SrcLoc)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Parse (a :: Type) :: Type where
  Bot :: Parse a
  Loc :: Parse SrcLoc
  Val :: a -> Parse a
  Chr :: {-# UNPACK #-} !Char -> Parse Char
  Str :: String -> Parse String
  Map :: (a -> b) -> Parse a -> Parse b
  Seq :: Parse a -> Parse b -> Parse (a, b)
  Alt :: Parse a -> Parse a -> Parse a
  Fix :: (Parse a -> Parse a) -> Parse a

-- | @since 1.0.0
instance Functor Parse where
  fmap = Map
  {-# INLINE CONLIKE fmap #-}

  (<$) x = Map (\_ -> x) 
  {-# INLINE (<$) #-}

-- | @since 1.0.0
instance Applicative Parse where
  pure = Val
  {-# INLINE CONLIKE pure #-}

  x <*> y = Map (uncurry ($)) (Seq x y)
  {-# INLINE CONLIKE (<*>) #-}

  liftA2 f x y = Map (uncurry f) (Seq x y)
  {-# INLINE CONLIKE liftA2 #-}

-- | @since 1.0.0
instance Alternative Parse where
  empty = Bot 
  {-# INLINE CONLIKE empty #-}

  (<|>) = Alt
  {-# INLINE CONLIKE (<|>) #-}

  many x = Fix \xs -> liftA2 (:) x xs <|> pure []
  {-# INLINE CONLIKE many #-}

  some x = liftA2 (:) x (many x)
  {-# INLINE CONLIKE some #-}