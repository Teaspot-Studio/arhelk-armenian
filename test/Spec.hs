import Test.Framework
import Test.Framework.Providers.QuickCheck2

import qualified Test.Arhelk.Armenian.Lemma as Lemma

main :: IO ()
main = defaultMainWithOpts [
    Lemma.testModule
  ]
  mempty
