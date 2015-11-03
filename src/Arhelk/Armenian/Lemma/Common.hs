module Arhelk.Armenian.Lemma.Common(
    endsWith
  ) where

import Data.Text as T

endsWith :: Text -> [Text] -> Bool
endsWith w ws = or $ ends <$> ws
  where
    ends s = takeEnd (T.length s) w == s 
