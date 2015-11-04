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

-- | Имя нарицательное или собственное
data Appellativity =
    AppellativeNoun -- ^ Нарицательное
  | ProperNoun -- ^ Собственное
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow Appellativity where 
  showb v = case v of 
    AppellativeNoun -> "нариц."
    ProperNoun -> "собств."

data Article = 
    Definite
  | Undefinite
  | FirstPersonArticle
  | SecondPersonArticle
  | ThirdPersonArticle
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow Article where
  showb v = case v of
    Definite -> "опр. артикль"
    Undefinite -> "неопр. артикль"
    FirstPersonArticle -> "1л притяж. артикль"
    SecondPersonArticle -> "2л притяж. артикль"
    ThirdPersonArticle -> "3л притяж. артикль"


-- | Substantive morphological properties
data SubstantiveProperties = SubstantiveProperties {
  _substAppellativity :: Maybe Appellativity
, _substDeclension :: Maybe Declension
, _substQuantity :: Maybe GrammarQuantity
, _substCase :: Maybe Հոլով
, _substArticle :: Maybe Article
} deriving (Eq, Show)

$(makeLenses ''SubstantiveProperties)

instance Monoid SubstantiveProperties where 
  mempty = SubstantiveProperties {
    _substAppellativity = Nothing
  , _substDeclension = Nothing
  , _substQuantity = Nothing
  , _substCase = Nothing
  , _substArticle = Nothing
  }

  mappend a b = SubstantiveProperties {
    _substAppellativity = getFirst $ First (_substAppellativity a) <> First (_substAppellativity b)
  , _substDeclension = getFirst $ First (_substDeclension a) <> First (_substDeclension b)
  , _substQuantity = getFirst $ First (_substQuantity a) <> First (_substQuantity b)
  , _substCase = getFirst $ First (_substCase a) <> First (_substCase b)
  , _substArticle = getFirst $ First (_substArticle a) <> First (_substArticle b)
  }

instance TextShow SubstantiveProperties where 
  showb SubstantiveProperties{..} = unwordsB [
      maybe "" showb _substAppellativity
    , maybe "" showb _substDeclension
    , maybe "" showb _substQuantity
    , maybe "" showb _substCase
    , maybe "" showb _substArticle
    ]