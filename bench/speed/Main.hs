{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative (many, (<|>))

import Control.DeepSeq (NFData)

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, nf)

import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

import Text.Parsel
  ( Grammar,
    ParseError,
    char,
    digit,
    lower,
    parse,
    string,
    upper,
    whitespace,
  )

--------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain
    [ bench'parse
        "(char 'x')*"
        (\_ -> many (char 'x'))
        (newByteArrayIO 'x')
    , bench'parse
        "digit*"
        (\_ -> many digit)
        (tileByteArrayIO "0123456789")
    , bench'parse
        "(string 'hello')*"
        (\_ -> many (string "hello"))
        (tileByteArrayIO "hello")
    , bench'parse
        "(string ('hello')*)"
        (\n -> many (string (Text.replicate n "hello")))
        (tileByteArrayIO "hello")
    , bench'parse
        "(digit* ' ')*"
        (\_ -> many (many digit <* whitespace))
        (tileByteArrayIO "1234 ")
    , bench'parse
        "(digit | lower | upper)*"
        (\_ -> many (digit <|> lower <|> upper))
        (tileByteArrayIO "1aZ")
    ]

bench'parse ::
  forall a.
  NFData a =>
  -- | TODO
  String ->
  -- | TODO
  (Int -> Grammar a) ->
  -- | TODO
  (Int -> IO Text) ->
  Benchmark
bench'parse name parser gen =
  bgroup name (fmap mk'benchmark std'series)
  where
    mk'benchmark :: Int -> Benchmark
    mk'benchmark n = env (gen n) (bench (show n) . nf (run'benchmark n))

    run'benchmark :: Int -> Text -> Either ParseError a
    run'benchmark n input = parse (Text.pack name) input (parser n)

std'series :: [Int]
std'series = [500, 1000, 2000, 4000]

-- Generators ------------------------------------------------------------------

tileByteArrayIO :: Text -> Int -> IO Text
tileByteArrayIO text n = pure (Text.replicate (quot n (Text.length text)) text)
{-# INLINE tileByteArrayIO #-}

-- | Like 'Text.replicateChar', but performs the allocation for 'Text' in 'IO'
-- rather than 'ST'.
newByteArrayIO :: Char -> Int -> IO Text
newByteArrayIO chr n = pure (Text.replicate n (Text.pack [chr]))
{-# INLINE newByteArrayIO #-}