module Arhelk.Armenian.Lemma.Adverb(
    adverb
  ) where

import Arhelk.Core.Rule
import Arhelk.Armenian.Lemma.Common
import Arhelk.Armenian.Lemma.Data
import Control.Monad
import Data.Text as T 

adverb :: Text -> Rule AdverbProperties
adverb w = do 
  when (w `endsWith` ["о"]) $ imply adverbDegree PositiveDegree
  when (w `endsWith` ["е"]) $ imply adverbDegree ComparitiveDegree