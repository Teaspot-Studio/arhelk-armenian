module Arhelk.Armenian.Lemma.Adjective(
    adjective
  ) where

import Arhelk.Core.Rule
import Arhelk.Armenian.Lemma.Common
import Arhelk.Armenian.Lemma.Data
import Control.Monad
import Data.Text as T 

adjective :: Text -> Rule AdjectiveProperties
adjective w = do 
  propose adjQuantity GrammarSingle $ do
    when (w `endsWith` ["ой", "ый", "ий"]) $ implyNothing