module Arhelk.Armenian.Lemma.Data.Adjective where

import Arhelk.Armenian.Lemma.Data.Common 
import Arhelk.Armenian.Lemma.Data.Substantive

import Lens.Simple
import Data.Monoid
import TextShow 

-- | Разряд прилагательного
data AdjectiveCategory = 
    QualitiveAdjective -- ^ Качественное
  | ComparativeAdjective -- ^ Относительное
  | PossessiveAdjective -- ^ Притяжательное
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow AdjectiveCategory where 
  showb v = case v of 
    QualitiveAdjective -> "качеств."
    ComparativeAdjective -> "сравн."
    PossessiveAdjective -> "притяж."

-- | Степень сравнения
data AdjectiveDegree =
    PositiveDegree -- ^ Положительная степерь
  | ComparitiveDegree -- ^ Сравнительная степень
  | SuperlativeDegree -- ^ Превосходная степень
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow AdjectiveDegree where 
  showb v = case v of 
    PositiveDegree -> "полож. степень"
    ComparitiveDegree -> "сравн. степень"
    SuperlativeDegree -> "превосх. степень"

data AdjectiveProperties = AdjectiveProperties {
  _adjCase :: Maybe GrammarCase
, _adjQuantity :: Maybe GrammarQuantity
, _adjShort :: Maybe Bool
, _adjCategory :: Maybe AdjectiveCategory
} deriving (Eq, Show)

$(makeLenses ''AdjectiveProperties)

instance Monoid AdjectiveProperties where 
  mempty = AdjectiveProperties {
    _adjCase = Nothing
  , _adjQuantity = Nothing
  , _adjShort = Nothing
  , _adjCategory = Nothing
  }

  mappend a b = AdjectiveProperties {
    _adjCase = getFirst $ First (_adjCase a) <> First (_adjCase b)
  , _adjQuantity = getFirst $ First (_adjQuantity a) <> First (_adjQuantity b)
  , _adjShort = getFirst $ First (_adjShort a) <> First (_adjShort b)
  , _adjCategory = getFirst $ First (_adjCategory a) <> First (_adjCategory b)
  }

instance TextShow AdjectiveProperties where
  showb AdjectiveProperties{..} = unwordsB [
      maybe "" showb _adjCase
    , maybe "" showb _adjQuantity
    , maybe "" showb _adjShort
    , maybe "" showb _adjCategory
    ]