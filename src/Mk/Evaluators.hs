{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Mk.Evaluators
  ( Evaluator(..)
  , MonadEvaluator
  , builtinEvaluators
  , constEvaluator
  , commandEvaluator
  ) where

-- base
import Control.Monad.IO.Class
import Data.Void
import System.Exit

-- containers
import Data.Map (Map)
import qualified Data.Map.Strict as Map

-- mtl
import Control.Monad.Except
import Control.Monad.Reader

-- path
import Path

-- process
import System.Process (shell, readCreateProcessWithExitCode)

-- text
import Data.Text (Text)
import qualified Data.Text as Text

-- time
import Data.Time.Format
import Data.Time.LocalTime

-- mk
import Mk.Template


-- | Each variable evaluator is evaluated with a context of this type.
data Ctx = Ctx
  { ctxTarget :: !(Path Abs File)
  }

type MonadEvaluator m = (MonadError String m, MonadIO m)

data EvaluatorAction m = forall a. EvaluatorResult a => EvaluatorAction !(ReaderT Ctx m a)

-- TODO:
-- The intended variable name could be a phantom type parameter!
-- Then it should be possible to exclude duplicates at compile time.
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

class EvaluatorResult a where
  toText :: MonadError String m => a -> m Text

instance EvaluatorResult Text where
  toText = pure

instance EvaluatorResult String where
  toText = toText . Text.pack

instance EvaluatorResult (Path b t) where
  toText = toText . toFilePath

instance EvaluatorResult Void where
  toText = absurd


runEvaluatorAction
  :: forall m. MonadError String m
  => EvaluatorAction m
  -> Ctx
  -> m Text
runEvaluatorAction (EvaluatorAction rma) ctx =
  runReaderT rma ctx >>= toText


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


-- TODO:
-- Assert that there are no duplicate variable names in the builtin evaluators!
-- (this could happen if I make a copy&paste error and forget to change the variable name)
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
      , unsupported (Var "MAIL") "E-mail address of the current user."
      , unsupported (Var "USER") "Name of the currently logged-in user."
        -- TODO: should get user full name from system (with fall back on the login name)
      {-
      , (Var "HASKELLRESOLVER", Evaluator testEvaluator)
      , (Var "HASKELLMODULE", Evaluator testEvaluator2) -}
      ]

unsupported :: forall m. MonadEvaluator m => Var -> Text -> EvaluatorInfo m
unsupported var@(Var varName) shortDesc = mkEvalInfo var description action
  where
    description = shortDesc <> " No built-in support; must be set in the configuration file."
    action =
      throwError @_ @_ @Void
        ("no built-in support for variable %" <> Text.unpack varName <> "%, "
         <> "please assign its value in the configuration file.")

{- TODO:
`%LICENSE%`
Tries to determine the project's license it the following order:
1. Using `licensee` if installed. This can be disabled by
setting the `g:templates_use_licensee` variable to `0`.
2. Using the `g:license` variable.
3. If all else fails: Default to `MIT`.

`%HOST%`
Current host name.

`%GUARD%`
A string containing only alphanumeric characters, and
underscores, suitable to be used as preprocessor guards
for C/C++/Objective-C header file.

`%CLASS%`
File name, without extension, and the first character of
each word capitalised. This is typically used for Java/C++
class names.
`%MACROCLASS%`
File name, without extension, in all-capitals.

`%CAMELCLASS%`
File name, without extension, with the first character of
every word capitalised, and underscores removed.
-}

-- | The current local time in the given format.
-- See @Data.Time.Format@ for a description of the format string syntax.
formatCurrentLocalTime :: MonadIO m => String -> m String
formatCurrentLocalTime fmt = do
  zlt <- liftIO getZonedTime
  pure (formatTime defaultTimeLocale fmt zlt)

evalLocalTimeWithFormat :: MonadEvaluator m => String -> Var -> Text -> EvaluatorInfo m
evalLocalTimeWithFormat fmt intendedVar description = mkEvalInfo intendedVar description action
  where action = formatCurrentLocalTime fmt

evalDAY :: MonadEvaluator m => EvaluatorInfo m
evalDAY = evalLocalTimeWithFormat "%d" (Var "DAY") description
  where description = "Current day of the month as numeric value."

evalMONTH :: MonadEvaluator m => EvaluatorInfo m
evalMONTH = evalLocalTimeWithFormat "%m" (Var "MONTH") description
  where description = "Current month of the year as numeric value."

evalYEAR :: MonadEvaluator m => EvaluatorInfo m
evalYEAR = evalLocalTimeWithFormat "%0Y" (Var "YEAR") description
  where description = "Current year as numeric value."

evalDATE :: MonadEvaluator m => EvaluatorInfo m
evalDATE = evalLocalTimeWithFormat "%0Y-%m-%d" (Var "DATE") description
  where description = "Current date in `YYYY-mm-dd` format. "
                      <> "This is equivalent to expanding `%YEAR%-%MONTH%-%DAY%`."

evalTIME :: MonadEvaluator m => EvaluatorInfo m
evalTIME = evalLocalTimeWithFormat "%H:%M" (Var "TIME") description
  where description = "Current time in `HH:MM` format."

evalFDATE :: MonadEvaluator m => EvaluatorInfo m
evalFDATE = evalLocalTimeWithFormat "%0Y-%m-%d %H:%M" (Var "FDATE") description
  where description = "Current full date (date and time) in `YYYY-mm-dd HH:MM` format. "
                      <> "This is equivalent to expanding `%DATE% %TIME%`."

evalFILE :: MonadEvaluator m => EvaluatorInfo m
evalFILE = mkEvalInfo (Var "FILE") description action
  where
    description = "File name, without extension."
    action = do
      name <- asks (filename . ctxTarget)
      case setFileExtension "" name of
        Left e -> throwError ("evalFILE: " <> show e)
        Right basename -> pure basename

evalEXT :: MonadEvaluator m => EvaluatorInfo m
evalEXT = mkEvalInfo (Var "EXT") description action
  where
    description = "File extension (component after the last period)."
    action = asks (fileExtension . ctxTarget)

evalFFILE :: MonadEvaluator m => EvaluatorInfo m
evalFFILE = mkEvalInfo (Var "FFILE") description action
  where
    description = "File name, with extension. This is equivalent to expanding `%FILE%.%EXT%`."
    action = asks (filename . ctxTarget)

-- | @constEvaluator x@ always evaluates to `x`.
constEvaluator :: Monad m => Text -> Evaluator m
constEvaluator txt = Evaluator "Returns a constant value." (pure txt)
{-# INLINABLE constEvaluator #-}

-- | @commandEvaluator cmd@ executes the shell command `cmd` and returns its output.
-- TODO: pass the path of the target file as environment variable (MK_TARGET?)
commandEvaluator :: MonadEvaluator m => Text -> Evaluator m
commandEvaluator (Text.unpack -> cmd) = Evaluator "Returns the standard output of a shell command" action
  where
    action = do
      (exitCode, out, err) <- liftIO $ readCreateProcessWithExitCode (shell cmd) ""
      case exitCode of
        ExitSuccess -> return (Text.pack out)
        _ -> throwError ("Error running shell command `" <> cmd <> "`: exitCode = " <> show exitCode <> "\n\n"
                         <> "Output on stdout: \n" <> out <> "\n\n"
                         <> "Output on stderr: \n" <> err <> "\n")
{-# INLINABLE commandEvaluator #-}
