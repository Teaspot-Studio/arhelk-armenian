module Arhelk.Armenian.Lemma.Particle(
    particle
  ) where

import Arhelk.Core.Rule
import Arhelk.Armenian.Lemma.Common
import Arhelk.Armenian.Lemma.Data
import Control.Monad
import Data.Text as T 
import qualified Data.Text.IO as T 
import System.IO.Unsafe (unsafePerformIO)

particles :: [Text]
particles = unsafePerformIO $ do 
  ts <- T.readFile "config/particles.txt"
  return $ T.strip <$> T.lines ts

{-# NOINLINE particles #-}

-- | TODO: many words particles
particle :: Text -> Rule ParticleProperties
particle w = when (w `elem` particles) implyNothing