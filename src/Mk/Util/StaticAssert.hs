-- | Provides compile-time assertions via template haskell.
-- Thanks to https://stackoverflow.com/a/6654903 for the initial implementation.
module Mk.Util.StaticAssert
  ( staticAssert
  , staticAssertExp
  ) where

import Control.Monad (unless)
import qualified Language.Haskell.TH as TH

-- Note: return value needs to be a list if we want to be able to use it as a top-level splice
staticAssert :: Bool -> String -> TH.Q [a]
staticAssert cond msg = do
    unless cond $ TH.reportError $ "Compile time assertion failed: " ++ msg
    return []

staticAssertExp :: Bool -> String -> TH.ExpQ -> TH.ExpQ
staticAssertExp cond msg e = staticAssert cond msg >> e
