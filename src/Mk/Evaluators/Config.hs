{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Mk.Evaluators.Config
  ( constEvaluator
  , commandEvaluator
  ) where

-- base
import Control.Exception
import Control.Monad.IO.Class
import System.Environment.Blank
import System.Exit

-- mtl
import Control.Monad.Except

-- path
import Path

-- process
import System.Process (shell, readCreateProcessWithExitCode)

-- text
import Data.Text (Text)
import qualified Data.Text as Text

--mk
import Mk.Evaluators.Types


-- | @'constEvaluator' x@ always evaluates to @x@.
constEvaluator :: Monad m => Text -> Evaluator m
constEvaluator txt = Evaluator "Returns a constant value." (pure txt)
{-# INLINABLE constEvaluator #-}


-- | @'commandEvaluator' target cmd@ executes the shell command @cmd@ and returns its output.
-- The absolute path to the target file is available in the environment variable @MK_TARGET@.
commandEvaluator :: MonadEvaluator m => Path Abs File -> Text -> Evaluator m
commandEvaluator target (Text.unpack -> cmd) =
  Evaluator "Returns the standard output of a shell command" action
  where
    action = do
      (exitCode, out, err) <-
        liftIO $
        maskEnv "MK_TARGET" (toFilePath target) $
        readCreateProcessWithExitCode (shell cmd) ""
      case exitCode of
        ExitSuccess -> return (Text.pack out)
        _ -> throwError ("Error running shell command `" <> cmd <> "`: "
                         <> "exitCode = " <> show exitCode <> "\n\n"
                         <> "Output on stdout: \n" <> out <> "\n\n"
                         <> "Output on stderr: \n" <> err <> "\n")
{-# INLINABLE commandEvaluator #-}

maskEnv :: String -> String -> IO a -> IO a
maskEnv key tempValue action = do
  oldValue <- getEnv key
  finally (setEnv key tempValue True >> action)
          (maybe (unsetEnv key) (\v -> setEnv key v True) oldValue)
