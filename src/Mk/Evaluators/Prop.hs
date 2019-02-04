{-# LANGUAGE TemplateHaskell #-}

module Mk.Evaluators.Prop where

-- mk
import Mk.Evaluators (rawBuiltinEvaluators_uniqueVars_prop)
import Mk.Util.StaticAssert (staticAssert)


$(staticAssert
   rawBuiltinEvaluators_uniqueVars_prop
   "property Mk.Evaluators.rawBuiltinEvaluators_uniqueVars_prop failed!")
