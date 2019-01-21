{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Mk.Evaluators
  ( allEvaluators
  ) where

-- base
import Control.Monad.IO.Class

-- bytestring
import Data.ByteString (ByteString)
-- import qualified Data.ByteString as ByteString

-- containers
import Data.Map (Map)
import qualified Data.Map.Strict as Map

-- mtl
import Control.Monad.Except

-- path
import Path

-- mk
import Mk.Template



allEvaluators
  :: (MonadError String m, MonadIO m)
  => Map Var (Var -> Path Abs File -> m ByteString)
allEvaluators = Map.fromList
  [ (Var "HASKELLRESOLVER", testEvaluator)
  , (Var "HASKELLMODULE", testEvaluator)
  , (Var "HERE", testEvaluator)
  ]

testEvaluator :: (MonadError String m, MonadIO m) => Var -> Path Abs File -> m ByteString
testEvaluator var _ = do
  liftIO $ putStrLn $ "testEvaluator was called for " <> show var
  pure "HAHAHA"
