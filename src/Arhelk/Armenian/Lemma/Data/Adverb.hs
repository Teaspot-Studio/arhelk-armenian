module Arhelk.Armenian.Lemma.Data.Adverb where

import Arhelk.Armenian.Lemma.Data.Adjective

import Lens.Simple
import Data.Monoid
import TextShow 

data AdverbProperties = AdverbProperties {
  _adverbDegree :: Maybe AdjectiveDegree
} deriving (Eq, Show)

$(makeLenses ''AdverbProperties)

instance Monoid AdverbProperties where 
  mempty = AdverbProperties {
    _adverbDegree = Nothing
  }

  mappend a b = AdverbProperties {
    _adverbDegree = getFirst $ First (_adverbDegree a) <> First (_adverbDegree b)
  }

instance TextShow AdverbProperties where
  showb AdverbProperties{..} = unwordsB [
      maybe "" showb _adverbDegree
    ]