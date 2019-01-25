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

-- containers
import Data.Map (Map)
import qualified Data.Map.Strict as Map

-- mtl
import Control.Monad.Except
import Control.Monad.Reader

-- path
import Path

-- text
import Data.Text (Text)
import qualified Data.Text as Text

-- mk
import Mk.Template


-- | Each variable evaluator is evaluated with a context of this type.
data Ctx = Ctx
  { ctxVar :: !Var
  , ctxTarget :: !(Path Abs File)
  }

type MonadEvaluator m = (MonadReader Ctx m, MonadError String m, MonadIO m)

data Evaluator m = forall a. EvaluatorResult a => Evaluator !(m a)

class EvaluatorResult a where
  toText :: MonadError String m => a -> m Text

instance EvaluatorResult String where
  toText = pure . Text.pack

instance EvaluatorResult Text where
  toText = pure . id


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
  -> Map Var (m Text)
allEvaluators target = Map.fromList $ applyCtx' <$> rawEvaluators
  where
    applyCtx' (v, e) = (v, applyCtx (mkCtx v) e)
    applyCtx ctx (Evaluator rma) = runReaderT rma ctx >>= toText
    mkCtx v = Ctx{ ctxVar = v
                 , ctxTarget = target
                 }
{-# INLINABLE allEvaluators #-}

testEvaluator :: MonadEvaluator m => m Text
testEvaluator = do
  var <- asks ctxVar
  liftIO $ putStrLn $ "testEvaluator was called for " <> show var
  pure "HAHAHA"

testEvaluator2 :: MonadEvaluator m => m String
testEvaluator2 = do
  var <- asks ctxVar
  liftIO $ putStrLn $ "testEvaluator2 was called for " <> show var
  pure "HAHAHA"
