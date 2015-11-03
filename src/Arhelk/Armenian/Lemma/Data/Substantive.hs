module Arhelk.Armenian.Lemma.Data.Substantive where

import Arhelk.Armenian.Lemma.Data.Common

import Lens.Simple
import Data.Monoid
import TextShow 

-- | Склонение. Describes declension of substantives
data Declension = 
    FirstDeclension
  | SecondDeclension
  | ThirdDeclension
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow Declension where 
  showb v = case v of 
    FirstDeclension -> "I скл."
    SecondDeclension -> "II скл."
    ThirdDeclension -> "III скл."

-- | Падеж. Grammatical case.
data GrammarCase =
    Nominativus -- ^ Иминительный
  | Genitivus -- ^ Родительный
  | Dativus -- ^ Дательный
  | Accusativus -- ^ Винительный
  | Ablativus -- ^ Творительный
  | Praepositionalis -- ^ Предложный
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow GrammarCase where 
  showb v = case v of 
    Nominativus -> "им. падеж"
    Genitivus -> "род. падеж"
    Dativus -> "дат. падеж"
    Accusativus -> "вин. падеж"
    Ablativus -> "твор. падеж"
    Praepositionalis -> "предл. падеж"

-- | Имя нарицательное или собственное
data Appellativity =
    AppellativeNoun -- ^ Нарицательное
  | ProperNoun -- ^ Собственное
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow Appellativity where 
  showb v = case v of 
    AppellativeNoun -> "нариц."
    ProperNoun -> "собств."

-- | Одушевленность 
data Animacy = 
    AnimateNoun -- ^ Одушевленное
  | InanimateNoun -- ^ Неодушевленное
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow Animacy where 
  showb v = case v of
    AnimateNoun -> "одушвл."
    InanimateNoun -> "неодушвл."

-- | Substantive morphological properties
data SubstantiveProperties = SubstantiveProperties {
  _substAppellativity :: Maybe Appellativity
, _substAnimacy :: Maybe Animacy
, _substDeclension :: Maybe Declension
, _substQuantity :: Maybe GrammarQuantity
, _substCase :: Maybe GrammarCase
} deriving (Eq, Show)

$(makeLenses ''SubstantiveProperties)

instance Monoid SubstantiveProperties where 
  mempty = SubstantiveProperties {
    _substAppellativity = Nothing
  , _substAnimacy = Nothing
  , _substDeclension = Nothing
  , _substQuantity = Nothing
  , _substCase = Nothing
  }

  mappend a b = SubstantiveProperties {
    _substAppellativity = getFirst $ First (_substAppellativity a) <> First (_substAppellativity b)
  , _substAnimacy = getFirst $ First (_substAnimacy a) <> First (_substAnimacy b) 
  , _substDeclension = getFirst $ First (_substDeclension a) <> First (_substDeclension b)
  , _substQuantity = getFirst $ First (_substQuantity a) <> First (_substQuantity b)
  , _substCase = getFirst $ First (_substCase a) <> First (_substCase b)
  }

instance TextShow SubstantiveProperties where 
  showb SubstantiveProperties{..} = unwordsB [
      maybe "" showb _substAppellativity
    , maybe "" showb _substAnimacy
    , maybe "" showb _substDeclension
    , maybe "" showb _substQuantity
    , maybe "" showb _substCase
    ]