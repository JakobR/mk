{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Mk.Evaluators.Haskell
  ( evalHASKELLRESOLVER
  , evalHASKELLMODULE
  ) where

-- base
import Control.Monad.IO.Class
import Data.Char (isUpper, toUpper)
import Data.List (intercalate)

-- filepath
import System.FilePath (dropTrailingPathSeparator)

-- lens
import Control.Lens

-- lens-aeson
import Data.Aeson.Lens (key, _String)

-- mtl
import Control.Monad.Reader

-- path
import Path

-- path-io
import Path.IO

-- transformers
import Control.Monad.Trans.Maybe

-- yaml
import Data.Yaml (decodeFileThrow, Value(..))

-- mk
import Mk.Evaluators.Filename (getRootName)
import Mk.Evaluators.Types


evalHASKELLRESOLVER :: MonadEvaluator m => EvaluatorInfo m
evalHASKELLRESOLVER = mkEvalInfo (Var "HASKELLRESOLVER") description action
  where
    description = "Current stackage resolver, see https://www.stackage.org/"
    action = do
      targetDir <- parent <$> asks ctxTarget
      confMay <- runMaybeT $ findStackConf targetDir >>= readStackConf
      case confMay ^? _Just . key "resolver" . _String of
        Nothing ->
          -- Unable to find stack.conf or resolver value, so let the user fill out the value.
          return "TODO"
        Just resolver ->
          return resolver
{-# INLINABLE evalHASKELLRESOLVER #-}


readStackConf :: MonadIO m => Path Abs File -> m Value
readStackConf path = decodeFileThrow (toFilePath path)


findStackConf :: MonadIO m => Path Abs Dir -> MaybeT m (Path Abs File)
findStackConf dir = MaybeT $ do
  -- It should be possible to create files via symbolic links pointing to a project subdirectory
  canonicalDir <- canonicalizePath dir
  findStackConf' canonicalDir


findStackConf' :: MonadIO m => Path Abs Dir -> m (Maybe (Path Abs File))
findStackConf' dir = do
  let stackConf = dir </> [relfile|stack.yaml|]
  doesFileExist stackConf >>= \case
    True -> return (Just stackConf)
    False | parent dir /= dir -> findStackConf' (parent dir)
          | otherwise -> do
              homeDir <- getHomeDir
              let globalStackConf = homeDir </> [relfile|.stack/global-project/stack.yaml|]
              doesFileExist globalStackConf >>= \case
                True -> return (Just globalStackConf)
                False -> return Nothing


evalHASKELLMODULE :: MonadEvaluator m => EvaluatorInfo m
evalHASKELLMODULE = mkEvalInfo (Var "HASKELLMODULE") description action
  where
    description = "Haskell module name."
    action = do
      -- Example: path "/bla/bla/project/src/Blah/Blup/Blop.hs" => module name "Blah.Blup.Blop"
      -- Algorithm: include all parent directories whose names start with an upper-case character.
      target <- asks ctxTarget
      targetRootName <- getRootName target
      let components = moduleDirs (parent target)
                       ++ [toFilePath targetRootName & _head %~ toUpper]
      return (intercalate "." components)

    moduleDirs :: Path Abs Dir -> [String]
    moduleDirs = go []
      where
        go !xs dir =
          case toFilePath (dirname dir) of
            h:_ | isUpper h -> let component = dropTrailingPathSeparator . toFilePath . dirname $ dir
                               in go (component : xs) (parent dir)
            _ -> xs
{-# INLINABLE evalHASKELLMODULE #-}
