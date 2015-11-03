module Arhelk.Armenian.Lemma.Data.Common where

import TextShow 

-- | Describes possible part of speach in Russian
data SpeachPart = 
    Substantive -- ^ Существительное
  | Adjective -- ^ Прилагательное
  | Numeral -- ^ Числительное
  | Pronoun -- ^ Местоимение
  | Verb -- ^ Глагол
  | Adverb -- ^ Наречие
  | Preposition -- ^ Предлог
  | Conjunction -- ^ Союз
  | GrammarParticle -- ^ Частица
  | Interjection -- ^ Междуметие
  | Participle -- ^ Причастие
  | Transgressive -- ^ Деепричастие
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow SpeachPart where 
  showb p = case p of 
    Substantive -> "сущ."
    Adjective -> "прил."
    Numeral -> "числ."
    Pronoun -> "мест."
    Verb -> "гл."
    Adverb -> "нар."
    Preposition -> "предл."
    Conjunction -> "союз"
    GrammarParticle -> "част."
    Interjection -> "межд."
    Participle -> "прич."
    Transgressive -> "деепр."

-- | Множественное или единственное число
data GrammarQuantity =
    GrammarSingle -- ^ Единственное
  | GrammarMultiple -- ^ Множественное
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow GrammarQuantity where 
  showb v = case v of 
    GrammarSingle -> "ед. число"
    GrammarMultiple -> "мн. число"

-- | Лицо
data GrammarPerson =
    FirstPerson -- ^ Первое лицо
  | SecondPerson -- ^ Второе лицо
  | ThirdPerson -- ^ Третье лицо
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow GrammarPerson where 
  showb v = case v of 
    FirstPerson -> "1 лицо"
    SecondPerson -> "2 лицо"
    ThirdPerson -> "3 лицо"
