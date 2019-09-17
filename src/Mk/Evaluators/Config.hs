{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Mk.Evaluators.Config
  ( constEvaluator
  , commandEvaluator
  ) where

-- base
import Control.Exception (finally)
import Control.Monad.IO.Class (liftIO)
import System.Environment.Blank (getEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(ExitSuccess))

-- mtl
import Control.Monad.Except (throwError)

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
--
-- Strips off the last newline of the output to make it easier to use with shell commands.
commandEvaluator :: MonadEvaluator m => Path Abs File -> Text -> Evaluator m
commandEvaluator target cmd =
  Evaluator "Returns the standard output of a shell command" action
  where
    action = do
      (exitCode, out, err) <-
        liftIO $
        maskEnv "MK_TARGET" (toFilePath target) $
        readCreateProcessWithExitCode (shell $ Text.unpack cmd) ""
      case exitCode of
        ExitSuccess -> return (Text.pack $ stripLastNewline out)
        _ -> throwError ("Error running shell command " <> show cmd <> ":\n"
                         <> "exitCode = " <> show exitCode <> "\n\n"
                         <> "Output on stdout:\n" <> out <> "\n\n"
                         <> "Output on stderr:\n" <> err <> "\n")
{-# INLINABLE commandEvaluator #-}


stripLastNewline :: String -> String
stripLastNewline [] = []
stripLastNewline ['\n'] = []
stripLastNewline (x:xs) = x : stripLastNewline xs
{-# INLINABLE stripLastNewline #-}


maskEnv :: String -> String -> IO a -> IO a
maskEnv key tempValue action = do
  oldValue <- getEnv key
  finally (setEnv key tempValue True >> action)
          (maybe (unsetEnv key) (\v -> setEnv key v True) oldValue)
