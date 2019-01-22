{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mk.Evaluators
  ( allEvaluators
  ) where

-- base
import Control.Monad.IO.Class

-- bytestring
import Data.ByteString (ByteString)
-- import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as B8

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


-- | Each variable evaluator has access to this data.
data Ctx = Ctx
  { ctxVar :: !Var
  , ctxTarget :: !(Path Abs File)
  }

type MonadEvaluator m = (MonadReader Ctx m, MonadError String m, MonadIO m)

data Evaluator m = forall a. EvaluatorResult a => Evaluator !(m a)

class EvaluatorResult a where
  toByteString :: MonadError String m => a -> m ByteString

instance EvaluatorResult ByteString where
  toByteString = pure . id

instance EvaluatorResult String where
  toByteString = pure . B8.pack  -- TODO: use proper UTF-8 encoding


rawEvaluators
  :: forall m. (MonadError String m, MonadIO m)
  => [(Var, Evaluator (ReaderT Ctx m))]
rawEvaluators =
      [ (Var "HASKELLRESOLVER", Evaluator testEvaluator)
      , (Var "HASKELLMODULE", Evaluator testEvaluator2)
      ]

allEvaluators
  :: forall m. (MonadError String m, MonadIO m)
  => Path Abs File  -- ^ path to target file
  -> Map Var (m ByteString)
allEvaluators target = Map.fromList $ applyCtx' <$> rawEvaluators
  where
    applyCtx' (v, e) = (v, applyCtx (mkCtx v) e)
    applyCtx ctx (Evaluator rma) = runReaderT rma ctx >>= toByteString
    mkCtx v = Ctx{ ctxVar = v
                 , ctxTarget = target
                 }
{-# INLINABLE allEvaluators #-}

testEvaluator :: MonadEvaluator m => m ByteString
testEvaluator = do
  var <- asks ctxVar
  liftIO $ putStrLn $ "testEvaluator was called for " <> show var
  pure "HAHAHA"

testEvaluator2 :: MonadEvaluator m => m String
testEvaluator2 = do
  var <- asks ctxVar
  liftIO $ putStrLn $ "testEvaluator2 was called for " <> show var
  pure "HAHAHA"
