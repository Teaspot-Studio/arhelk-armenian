module Arhelk.Armenian.Lemma(
    Lemma(..)
  , lemmanize
  , module X
  ) where 

import Arhelk.Core.Rule
import Arhelk.Armenian.Lemma.Adjective as X
import Arhelk.Armenian.Lemma.Adverb as X
import Arhelk.Armenian.Lemma.Data as X
import Arhelk.Armenian.Lemma.Particle as X
import Arhelk.Armenian.Lemma.Substantive as X
import Arhelk.Armenian.Lemma.Verb as X
import Control.Monad
import Data.Text as T
import Lens.Simple

-- | Parsed word with detached prefixes, postfixes and determinied speach part
data Lemma = Lemma {
-- | Initial full word
  lemmaSource :: Text
-- | Semanitc root
, lemmaRoot :: Text
-- | Semantic prefixes
, lemmaPrefixes :: [Text]
-- | Semantic postfixes
, lemmaPostfixes :: [Text]
-- | Determinied part of speach
, lemmaSpeachPart :: Maybe SpeachPart
}

-- | Calculate work lemma
lemmanize :: Text -> Lemma
lemmanize = error "unimplemented"