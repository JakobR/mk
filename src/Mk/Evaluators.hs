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
import qualified Data.Text.Encoding as Text

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
  -- Note:
  -- Later we might want to add an argument to specify a certain encoding that should be used.
  -- Might be specified via a command-line argument.
  -- (But we could also apply a UTF-8 -> ??? transformation on the outside.)

  -- | Encode the evaluator result using UTF-8, if applicable.
  toByteString :: MonadError String m => a -> m ByteString

instance EvaluatorResult ByteString where
  toByteString = pure . id

instance EvaluatorResult String where
  toByteString = pure . Text.encodeUtf8 . Text.pack

instance EvaluatorResult Text where
  toByteString = pure . Text.encodeUtf8


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
