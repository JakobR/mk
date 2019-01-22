{-# LANGUAGE ConstraintKinds #-}
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
import Control.Monad.Reader

-- path
import Path

-- mk
import Mk.Template



allEvaluators
  :: (MonadError String m, MonadIO m)
  => Path Abs File  -- ^ path to target file
  -> Map Var (m ByteString)
allEvaluators target = Map.fromList assocsWithCtx
  where
    assocs =
      [ (Var "HASKELLRESOLVER", testEvaluator)
      , (Var "HASKELLMODULE", testEvaluator)
      ]
    ctx v = Ctx{ ctxVar = v
               , ctxTarget = target
               }
    assocsWithCtx = [ (v, runReaderT mb (ctx v)) | (v, mb) <- assocs ]
{-# INLINABLE allEvaluators #-}


data Ctx = Ctx
  { ctxVar :: !Var
  , ctxTarget :: !(Path Abs File)
  }

type MonadEvaluator m = (MonadReader Ctx m, MonadError String m, MonadIO m)

testEvaluator :: MonadEvaluator m => m ByteString
testEvaluator = do
  var <- asks ctxVar
  liftIO $ putStrLn $ "testEvaluator was called for " <> show var
  pure "HAHAHA"
