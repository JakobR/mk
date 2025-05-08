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
import Data.Maybe (fromMaybe)

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


-- | Get root name of path, i.e., file name without directories and without file extension.
--
-- Returns 'Nothing' if the target filename consists only of an extension without proper filename (e.g., @.xyz@).
getRootName :: MonadError String m => Path b File -> m (Maybe (Path Rel File))
getRootName path =
  case setFileExtension "" (filename path) of
    Left e ->
      -- TODO: fix this function (the file extension test is probably wrong now)
      -- TODO: at this point, it would be smart to add tests for all the evaluators
      if fileExtension path == Just (toFilePath (filename path))
      then pure Nothing
      else throwError ("getRootName: " <> show e)
    Right rootName -> pure (Just rootName)
{-# INLINABLE getRootName #-}

evalFILE :: MonadEvaluator m => EvaluatorInfo m
evalFILE = mkEvalInfo (Var "FILE") description action
  where
    description = "File name, without extension."
    action = asks ctxTarget >>= getRootName >>= maybe (pure "") toText
{-# INLINABLE evalFILE #-}

evalEXT :: MonadEvaluator m => EvaluatorInfo m
evalEXT = mkEvalInfo (Var "EXT") description action
  where
    description = "File extension (component after the last dot)."
    action = asks (tailSafe . fromMaybe "" . fileExtension . ctxTarget)
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
