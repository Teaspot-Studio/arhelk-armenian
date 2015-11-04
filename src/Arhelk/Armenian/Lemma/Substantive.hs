module Arhelk.Armenian.Lemma.Substantive(
    substantive,
    stripHolov
  ) where

import Arhelk.Core.Rule
import Arhelk.Armenian.Lemma.Common
import Arhelk.Armenian.Lemma.Data
import Arhelk.Armenian.Lemma.Data.Substantive
import Control.Monad
import Data.Text as T 
import Data.List as L
import Lens.Simple

stripHolov :: [Text] -> Text -> Text
stripHolov h w = L.foldl (\acc e -> 
    if acc `endsWith` [e]
      then T.dropEnd (T.length e) acc
      else acc
    ) w h 

-- | Try to guess quantity, declension and case by ending of word
substantive :: Text -> Rule SubstantiveProperties
substantive w = do
  guessQuantity
  guessCase
  guessArticle
  where
    guessArticle = do
      let d = w `endsWith` ["ը"]  -- This rule is 100%
      let a = w `endsWith` ["ան", "են", "էն", "ին", "ուն", "ոն", "օն"] --This might be wrong
      when (d) $ imply substArticle Definite
      when (a) $ imply substArticle Definite
      when (not (a || d) ) $ imply substArticle Undefinite
      when (w `endsWith` ["ս"]) $ imply substArticle FirstPersonArticle
      when (w `endsWith` ["դ"]) $ imply substArticle SecondPersonArticle
      when (w `endsWith` ["ն"]) $ imply substArticle ThirdPersonArticle

    guessQuantity = do
      if (stripHolov (holovs Ուղղական) ws) `endsWith` ["եր","ներ"]
        then imply substQuantity GrammarMultiple
        else imply substQuantity GrammarSingle
      where ws = if (w `endsWith` ["ը"]) then T.dropEnd 1 w else w

    guessCase = do
      when (ws `endsWith` holovs Սերական) $ imply substCase Սերական
      when (ws `endsWith` holovs Տրական) $ imply substCase Տրական
      when (ws `endsWith` holovs Հայցական) $ imply substCase Հայցական
      when (ws `endsWith` holovs Բացարական) $ imply substCase Բացարական
      when (ws `endsWith` holovs Գործիական) $ imply substCase Գործիական
      when (ws `endsWith` holovs Ներգոյական) $ imply substCase Ներգոյական
      when (not (ws `endsWith` holovs Ուղղական) || (w `endsWith` [""])) $ imply substCase Ուղղական
      where ws = if (w `endsWith` ["ը"]) then T.dropEnd 1 w else w