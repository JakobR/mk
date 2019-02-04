{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Mk.Evaluators.Types
  ( Ctx(..)
  , MonadEvaluator
  , EvaluatorAction
  , EvaluatorInfo(..)
  , Var(..)  -- from Mk.Template
  , Evaluator(..)
  , EvaluatorResult(..)
  , mkEvalInfo
  , runEvaluatorAction
  , runEval
  ) where

-- base
import Control.Monad.IO.Class
import Data.Void

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
  { ctxTarget :: !(Path Abs File)
  }


-- TODO: include some kind of MonadLogger here, so we can log debug info from the evaluators
-- (should only be displayed when "--verbose" is set)
type MonadEvaluator m = (MonadError String m, MonadIO m)


data EvaluatorAction m = forall a. EvaluatorResult a => EvaluatorAction !(ReaderT Ctx m a)


data EvaluatorInfo m = EvaluatorInfo
  { eiIntendedVar :: !Var
  , eiDescription :: !Text
  , eiAction :: !(EvaluatorAction m)
  }


data Evaluator m = Evaluator
  { getDescription :: !Text
  , runEvaluator :: !(m Text)
  }


mkEvalInfo :: EvaluatorResult a => Var -> Text -> ReaderT Ctx m a -> EvaluatorInfo m
mkEvalInfo var description action = EvaluatorInfo var description (EvaluatorAction action)
{-# INLINABLE mkEvalInfo #-}


class EvaluatorResult a where
  toText :: MonadError String m => a -> m Text

instance EvaluatorResult Text where
  toText = pure
  {-# INLINABLE toText #-}

instance EvaluatorResult String where
  toText = toText . Text.pack
  {-# INLINABLE toText #-}

instance EvaluatorResult (Path b t) where
  toText = toText . toFilePath
  {-# INLINABLE toText #-}

instance EvaluatorResult Void where
  toText = absurd
  {-# INLINABLE toText #-}


runEvaluatorAction
  :: forall m. MonadError String m
  => EvaluatorAction m
  -> Ctx
  -> m Text
runEvaluatorAction (EvaluatorAction rma) ctx =
  runReaderT rma ctx >>= toText
{-# INLINABLE runEvaluatorAction #-}


runEval
  :: forall m. MonadEvaluator m
  => EvaluatorInfo m
  -> ReaderT Ctx m Text
runEval (eiAction -> EvaluatorAction action) =
  action >>= toText
{-# INLINABLE runEval #-}
