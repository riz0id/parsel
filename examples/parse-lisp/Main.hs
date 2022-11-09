
module Main where

import Control.Applicative (many, some, (<|>))

import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO

import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), withFile)

import Text.Parsel 

--------------------------------------------------------------------------------

data SExp = Symbol String | List [SExp]

instance Show SExp where
  show (Symbol x) = '\'' : x
  show (List xs) = "(" ++ unwords (map show xs) ++ ")"

sexp :: Grammar SExp
sexp = whitespaces *> atom

atom :: Grammar SExp
atom = (symbol <|> list) <* whitespaces

symbol :: Grammar SExp
symbol = do 
  str <- some lower
  pure (Symbol str)

list :: Grammar SExp
list = do 
  xs <- parens (many atom)
  pure (List xs)

parseSExp :: Text -> Either ParseError SExp
parseSExp src = parse "s-expression" src sexp

main :: IO ()
main = do
  pure ()
  -- filepaths <- getArgs
  -- for_ filepaths \filepath ->
  --   withFile filepath ReadMode \handle -> do
  --     source <- Text.IO.hGetContents handle
  --     case parseSExp source of 
  --       Left exn -> print exn
  --       Right rx -> print rx