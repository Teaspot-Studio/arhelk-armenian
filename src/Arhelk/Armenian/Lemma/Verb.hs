module Arhelk.Armenian.Lemma.Verb(
    verb
  ) where

import Arhelk.Core.Rule
import Arhelk.Armenian.Lemma.Common
import Arhelk.Armenian.Lemma.Data 
import Control.Monad
import Data.Text as T 

-- | Tries to guess verb properties by endings
verb :: Text -> Rule VerbProperties
verb w = do 
  infinitive 
  conjugation
  where

  -- Infinitive endings
  infinitive = do 
    propose verbMood ModusInfinitivus $ do 
      when (w `endsWith` ["еть", "овать", "ать", "ять"]) $ imply verbConjugation FirstConjugation -- with many exceptions

  -- Guess by conjugation 
  conjugation = proposeMany verbTime [PresentTime, FutureTime] $ do  
    propose verbPerson FirstPerson $ do 
      propose verbQuantity GrammarSingle $ do
        when (w `endsWith` ["у", "ю"]) $ do
          imply verbConjugation FirstConjugation
          imply verbConjugation SecondConjugation
      propose verbQuantity GrammarMultiple $ do 
        when (w `endsWith` ["ем"]) $ imply verbConjugation FirstConjugation
        when (w `endsWith` ["им"]) $ imply verbConjugation SecondConjugation
    propose verbPerson SecondPerson $ do 
      propose verbQuantity GrammarSingle $ do 
        when (w `endsWith` ["ешь"]) $ imply verbConjugation FirstConjugation
        when (w `endsWith` ["ишь"]) $ imply verbConjugation SecondConjugation
      propose verbQuantity GrammarMultiple $ do 
        when (w `endsWith` ["ете"]) $ imply verbConjugation FirstConjugation
        when (w `endsWith` ["ите"]) $ imply verbConjugation SecondConjugation
    propose verbPerson ThirdPerson $ do 
      propose verbQuantity GrammarSingle $ do 
        when (w `endsWith` ["ет"]) $ imply verbConjugation FirstConjugation
        when (w `endsWith` ["ит"]) $ imply verbConjugation SecondConjugation
      propose verbQuantity GrammarMultiple $ do 
        when (w `endsWith` ["ут", "ют"]) $ imply verbConjugation FirstConjugation
        when (w `endsWith` ["ат", "ят"]) $ imply verbConjugation SecondConjugation
