
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

-- data SExp = Symbol String | List [SExp]

-- instance Show SExp where
--   show (Symbol x) = '\'' : x
--   show (List xs) = "(" ++ unwords (map show xs) ++ ")"

-- sexp :: Grammar SExp
-- sexp = whitespaces *> atom

-- atom :: Grammar SExp
-- atom = do 
--   x <- symbol <|> list
--   whitespaces
--   pure x

symbol :: Grammar String
symbol = some lower

-- list :: Grammar SExp
-- list = do 
--   xs <- parentheses (many atom)
--   pure (List xs)

parseSExp :: Text -> Either ParseError String
parseSExp src = parse src symbol

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