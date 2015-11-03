module Arhelk.Armenian.Lemma.Substantive(
    substantive
  ) where

import Arhelk.Core.Rule
import Arhelk.Armenian.Lemma.Common
import Arhelk.Armenian.Lemma.Data
import Control.Monad
import Data.Text as T 
import Lens.Simple

-- | Try to guess quantity, declension and case by ending of word
substantive :: Text -> Rule SubstantiveProperties
substantive w = do
  propose substCase Nominativus $ do 
    propose substQuantity GrammarSingle $ do
      when (w `endsWith` ["а", "я"]) $ imply substDeclension FirstDeclension
      when (w `endsWith` ["", "о", "е"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` [""]) $ imply substDeclension ThirdDeclension
    propose substQuantity GrammarMultiple $ do
      when (w `endsWith` ["ы", "и"]) $ imply substDeclension  FirstDeclension
      when (w `endsWith` ["ы", "и", "а", "я"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` ["и"]) $ imply substDeclension ThirdDeclension

  propose substCase Genitivus $ do 
    propose substQuantity GrammarSingle $ do 
      when (w `endsWith` ["ы", "и"]) $ imply substDeclension FirstDeclension
      when (w `endsWith` ["а", "я", "у", "ю"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` ["и"]) $ imply substDeclension ThirdDeclension
    propose substQuantity GrammarMultiple $ do 
      when (w `endsWith` ["", "ов", "ев", "ей"]) implyNothing

  propose substCase Dativus $ do 
    propose substQuantity GrammarSingle $ do 
      when (w `endsWith` ["е", "и"]) $ imply substDeclension FirstDeclension
      when (w `endsWith` ["у", "ю"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` ["и"]) $ imply substDeclension ThirdDeclension
    propose substQuantity GrammarMultiple $ do 
      when (w `endsWith` ["ам", "ям"]) implyNothing

  propose substCase Accusativus $ do 
    propose substQuantity GrammarSingle $ do 
      when (w `endsWith` ["у", "ю"]) $ imply substDeclension FirstDeclension
      when (w `endsWith` ["", "о", "е", "а", "я", "у", "ю"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` [""]) $ imply substDeclension ThirdDeclension
    propose substQuantity  GrammarMultiple $ do 
      when (w `endsWith` ["", "ы", "и", "а", "я", "ов", "ев", "ей"]) implyNothing

  propose substCase Ablativus $ do 
    propose substQuantity GrammarSingle $ do 
      when (w `endsWith` ["ой", "ою", "ей", "ею"]) $ imply substDeclension FirstDeclension
      when (w `endsWith` ["ом", "ем"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` ["ью"]) $ imply substDeclension ThirdDeclension
    propose substQuantity GrammarMultiple $ do 
      when (w `endsWith` ["ами", "ями", "ми"]) implyNothing

  propose substCase Praepositionalis $ do 
    propose substQuantity GrammarSingle $ do 
      when (w `endsWith` ["е", "и"]) $ imply substDeclension FirstDeclension
      when (w `endsWith` ["е", "и"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` ["и"]) $ imply substDeclension ThirdDeclension
    propose substQuantity GrammarMultiple $ do 
      when (w `endsWith` ["ах", "ях"]) implyNothing