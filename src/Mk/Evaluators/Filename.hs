{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Mk.Evaluators.Filename
  ( evalFILE
  , evalEXT
  , evalFFILE
  , evalGUARD
  , evalCLASS
  , evalMACROCLASS
  , evalCAMELCLASS
  , getRootName
  ) where

-- base
import Data.Char (isAlpha, isAlphaNum, toUpper)
import Data.Function (on)

-- lens
import Control.Lens

-- mtl
import Control.Monad.Except
import Control.Monad.Reader

-- path
import Path

-- safe
import Safe

-- text
import qualified Data.Text as Text

-- mk
import Mk.Evaluators.Types


getRootName :: MonadError String m => Path b File -> m (Path Rel File)
getRootName path =
  case setFileExtension "" (filename path) of
    Left e -> throwError ("getRootName: " <> show e)
    Right rootName -> pure rootName
{-# INLINABLE getRootName #-}

evalFILE :: MonadEvaluator m => EvaluatorInfo m
evalFILE = mkEvalInfo (Var "FILE") description action
  where
    description = "File name, without extension."
    action = asks ctxTarget >>= getRootName
{-# INLINABLE evalFILE #-}

evalEXT :: MonadEvaluator m => EvaluatorInfo m
evalEXT = mkEvalInfo (Var "EXT") description action
  where
    description = "File extension (component after the last dot)."
    action = asks (tailSafe . fileExtension . ctxTarget)
    -- NOTE: fileExtension returns the extension including the dot (e.g., ".txt")
{-# INLINABLE evalEXT #-}

evalFFILE :: MonadEvaluator m => EvaluatorInfo m
evalFFILE = mkEvalInfo (Var "FFILE") description action
  where
    description = "File name, with extension. This is equivalent to expanding '%FILE%.%EXT%'."
    action = asks (filename . ctxTarget)
{-# INLINABLE evalFFILE #-}

evalGUARD :: MonadEvaluator m => EvaluatorInfo m
evalGUARD = mkEvalInfo (Var "GUARD") description action
  where
    description = "A string suitable to be used as preprocessor guards for C or C++ header files."
    action = do
      let cleanChar c | isAlphaNum c = c
                      | otherwise    = '_'
      name <- asks (map cleanChar . toFilePath . filename . ctxTarget)
      Text.toUpper <$> toText name
{-# INLINABLE evalGUARD #-}

evalCLASS :: MonadEvaluator m => EvaluatorInfo m
evalCLASS = mkEvalInfo (Var "CLASS") description action
  where
    description = "File name without extension, and the first character of each word capitalised."
    action = do
      rootName <- runEval evalFILE
      let groups = Text.groupBy ((==) `on` isAlpha) rootName
          capitalisedGroups = groups & each._head %~ toUpper
      return (Text.concat capitalisedGroups)
{-# INLINABLE evalCLASS #-}

evalMACROCLASS :: MonadEvaluator m => EvaluatorInfo m
evalMACROCLASS = mkEvalInfo (Var "MACROCLASS") description action
  where
    description = "File name without extension, in upper case."
    action = Text.toUpper <$> runEval evalFILE
{-# INLINABLE evalMACROCLASS #-}

evalCAMELCLASS :: MonadEvaluator m => EvaluatorInfo m
evalCAMELCLASS = mkEvalInfo (Var "CAMELCLASS") description action
  where
    description = "File name without extension, the first character of each work capitalised, "
                  <> "and underscores removed."
    action = Text.replace "_" "" <$> runEval evalCLASS
{-# INLINABLE evalCAMELCLASS #-}
