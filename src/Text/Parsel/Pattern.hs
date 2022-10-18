{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Parsel.Pattern
  ( Pattern (PatBot, PatVar, PatChr, PatStr, PatMat, PatSeq, PatAlt, PatFix),
    fromGrammar,

    -- * Show 
    docPattern,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text

import Text.Emit (Doc, emit, (<+>))
import Text.Emit qualified as Emit

--------------------------------------------------------------------------------

import Text.Parsel.Grammar

--------------------------------------------------------------------------------

data Pattern
  = PatBot
  | PatVar
  | PatChr {-# UNPACK #-} !Char
  | PatStr {-# UNPACK #-} !Text
  | PatMat Match
  | PatSeq Pattern Pattern
  | PatAlt Pattern Pattern
  | PatFix Pattern
  deriving (Eq, Show)

fromGrammar :: Grammar a -> Pattern
fromGrammar Bot = PatBot
fromGrammar Eps = PatBot
fromGrammar Loc = PatBot
fromGrammar (Chr x) = PatChr x
fromGrammar (Str x) = PatStr x
fromGrammar (Mat mat) = PatMat mat
fromGrammar (Lab _ x) = fromGrammar x
fromGrammar (Map f x) = fromGrammar x
fromGrammar (Seq x y) = PatSeq (fromGrammar x) (fromGrammar y)
fromGrammar (Alt x y) = PatAlt (fromGrammar x) (fromGrammar y)
fromGrammar (Fix fix) =
  PatFix (fixPattern (fix (Fix fix)))
  where
    fixPattern :: Grammar a -> Pattern
    fixPattern (Lab _ x) = fixPattern x
    fixPattern (Map _ x) = fixPattern x
    fixPattern (Seq x y) = PatSeq (fixPattern x) (fixPattern y)
    fixPattern (Alt x y) = PatAlt (fixPattern x) (fixPattern y)
    fixPattern (Fix _) = PatVar
    fixPattern grammar = fromGrammar grammar

-- Patterns - Show -------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
docPattern :: Pattern -> Doc a
docPattern PatBot = "<empty pattern>"
docPattern PatVar = "x"
docPattern (PatChr chr) = docPatternChar chr
docPattern (PatStr str) = "'" <> Emit.text str <> "'"
docPattern (PatMat mat) = docPatternMatch mat
docPattern (PatSeq x y) = Emit.parens (docPattern x <+> docPattern y)
docPattern (PatAlt x y) = Emit.parens (docPattern x <+> "|" <+> docPattern y)
docPattern (PatFix pat) = Emit.parens ("Fix x." <+> docPattern pat)

-- | TODO
--
-- @since 1.0.0
docPatternChar :: Char -> Doc a 
docPatternChar chr = "'" <> Emit.text (Text.singleton chr) <> "'"

-- | TODO
--
-- @since 1.0.0
docPatternMatch :: Match -> Doc a 
docPatternMatch (Match chr1 chr2) = 
  let doc'chr1 = docPatternChar chr1 
      doc'chr2 = docPatternChar chr2
   in Emit.parens (doc'chr1 <+> ".." <+> doc'chr2)
docPatternMatch Lower = "<lower>"
docPatternMatch Upper = "<lower>"
docPatternMatch Space = "<space>"
docPatternMatch Alpha = "<alpha>"
docPatternMatch Digit = "<digit>"