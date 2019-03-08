{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Mk.Evaluators
  ( Evaluator(..)
  , MonadEvaluator
  , builtinEvaluators
  , constEvaluator
  , commandEvaluator
  , rawBuiltinEvaluators_uniqueVars_prop
  ) where

-- base
import Data.Void

-- containers
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Strict as Map

-- mtl
import Control.Monad.Except

-- path
import Path

-- text
import Data.Text (Text)
import qualified Data.Text as Text

--mk
import Mk.Evaluators.Config
import Mk.Evaluators.Filename
import Mk.Evaluators.Haskell
import Mk.Evaluators.System
import Mk.Evaluators.Time
import Mk.Evaluators.Types


-- | Built-in variable evaluators.
--
-- We want to include at least those supported by `vim-template`.
-- See https://github.com/aperezdc/vim-template/blob/master/doc/template.txt
builtinEvaluators
  :: forall m. MonadEvaluator m
  => Path Abs File  -- ^ path to target file
  -> Map Var (Evaluator m)
builtinEvaluators target = Map.fromList $ runWithCtx <$> rawBuiltinEvaluators
  where
    runWithCtx EvaluatorInfo{..} =
      ( eiIntendedVar
      , Evaluator eiDescription (runEvaluatorAction eiAction ctx)
      )
    ctx = Ctx{ ctxTarget = target
             }
{-# INLINABLE builtinEvaluators #-}


rawBuiltinEvaluators
  :: forall m. MonadEvaluator m
  => [EvaluatorInfo m]
rawBuiltinEvaluators =
      [ evalDAY
      , evalMONTH
      , evalYEAR
      , evalDATE
      , evalTIME
      , evalFDATE
      , evalFILE
      , evalEXT
      , evalFFILE
      , evalHOST
      , evalUSER
      , evalGUARD
      , evalCLASS
      , evalMACROCLASS
      , evalCAMELCLASS
      , evalHASKELLRESOLVER
      , evalHASKELLMODULE
      , unsupported (Var "MAIL") "Email address of the current user."
      , unsupported (Var "LICENSE") "Abbreviation of the project's license, e.g. \"MIT\"."
      ]


rawBuiltinEvaluators_uniqueVars_prop :: Bool
rawBuiltinEvaluators_uniqueVars_prop =
  let vars = eiIntendedVar <$> rawBuiltinEvaluators @(ExceptT String IO)
  in Set.size (Set.fromList vars) == length vars


unsupported :: forall m. MonadEvaluator m => Var -> Text -> EvaluatorInfo m
unsupported var@(Var varName) shortDesc = mkEvalInfo var description action
  where
    description = shortDesc <> " No built-in support; must be set in the configuration file."
    action =
      throwError @_ @_ @Void
        ("no built-in support for variable %" <> Text.unpack varName <> "%, "
         <> "please assign its value in the configuration file.")
