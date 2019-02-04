{-# LANGUAGE TemplateHaskell #-}

module Mk.Config.Prop () where

-- mk
import Mk.Config (readShowCursorPos_prop)
import Mk.Util.StaticAssert (staticAssert)


$(staticAssert readShowCursorPos_prop "property Mk.Config.readShowCursorPos_prop failed!")
