module Arhelk.Armenian.Lemma.Data.Verb where

import Arhelk.Armenian.Lemma.Data.Common

import Lens.Simple
import Data.Monoid
import TextShow 

-- ^ Время глагола
data GrammarTime =
    PastTime -- ^ Прошедшее
  | PresentTime -- ^ Настоящее
  | FutureTime -- ^ Будущее
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow GrammarTime where 
  showb v = case v of 
    PastTime -> "прошл."
    PresentTime -> "наст."
    FutureTime -> "буд."

-- | Наклонение глагола
data GrammarMood =
    ModusIndicativus -- ^ Изъявительное
  | ModusConditionalis -- ^ Условное
  | ModusImperativus -- ^ Повелительное
  | ModusInfinitivus -- ^ Инфинитив
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow GrammarMood where 
  showb v = case v of 
    ModusIndicativus -> "изъяв."
    ModusConditionalis -> "усл."
    ModusImperativus -> "повел."
    ModusInfinitivus -> "инф."
    
-- | Вид глагола
data GrammarAspect = 
    PerfectiveAspect -- ^ Совершенный вид
  | ImperfectiveAspect -- ^ Несовершенный вид
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow GrammarAspect where 
  showb v = case v of 
    PerfectiveAspect -> "соверш."
    ImperfectiveAspect -> "несоверш."

-- | Залог глагола
data GrammarVoice =
    ActiveVoice -- ^ Творительный залог
  | PassiveVoice -- ^ Страдательный залог
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow GrammarVoice where 
  showb v = case v of 
    ActiveVoice -> "твор. залог"
    PassiveVoice -> "страд. залог"

-- | Спряжение глаголов
data GrammarConjugation = 
    FirstConjugation
  | SecondConjugation
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow GrammarConjugation where 
  showb v = case v of 
    FirstConjugation -> "I спрж."
    SecondConjugation -> "II спрж."

data VerbProperties = VerbProperties {
  _verbPerson :: Maybe GrammarPerson
, _verbQuantity :: Maybe GrammarQuantity
, _verbTime :: Maybe GrammarTime
, _verbMood :: Maybe GrammarMood
, _verbAspect :: Maybe GrammarAspect
, _verbVoice :: Maybe GrammarVoice 
, _verbConjugation :: Maybe GrammarConjugation
} deriving (Eq, Show)

$(makeLenses ''VerbProperties)

instance Monoid VerbProperties where 
  mempty = VerbProperties {
      _verbPerson = Nothing
    , _verbQuantity = Nothing
    , _verbTime = Nothing
    , _verbMood = Nothing
    , _verbAspect = Nothing
    , _verbVoice = Nothing
    , _verbConjugation = Nothing
    }

  mappend a b = VerbProperties {
      _verbPerson = getFirst $ First (_verbPerson a) <> First (_verbPerson b)
    , _verbQuantity = getFirst $ First (_verbQuantity a) <> First (_verbQuantity b)
    , _verbTime = getFirst $ First (_verbTime a) <> First (_verbTime b)
    , _verbMood = getFirst $ First (_verbMood a) <> First (_verbMood b)
    , _verbAspect = getFirst $ First (_verbAspect a) <> First (_verbAspect b)
    , _verbVoice = getFirst $ First (_verbVoice a) <> First (_verbVoice b)
    , _verbConjugation = getFirst $ First (_verbConjugation a) <> First (_verbConjugation b)
  }

instance TextShow VerbProperties where 
  showb VerbProperties{..} = unwordsB [
      maybe "" showb _verbPerson
    , maybe "" showb _verbQuantity
    , maybe "" showb _verbTime
    , maybe "" showb _verbMood
    , maybe "" showb _verbAspect
    , maybe "" showb _verbVoice
    , maybe "" showb _verbConjugation
    ]