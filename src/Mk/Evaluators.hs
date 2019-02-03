{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Exception
import Control.Monad.IO.Class
import Data.Char
import Data.Function (on)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Void
import System.Environment.Blank
import System.Exit

-- containers
import Data.Map (Map)
import qualified Data.Map.Strict as Map

-- filepath
import System.FilePath (dropTrailingPathSeparator)

-- hostname
import Network.HostName (getHostName)

-- lens
import Control.Lens

-- lens-aeson
import Data.Aeson.Lens (key, _String)

-- mtl
import Control.Monad.Except
import Control.Monad.Reader

-- path
import Path

-- path-io
import Path.IO

-- process
import System.Process (shell, readCreateProcessWithExitCode)

-- safe
import Safe

-- text
import Data.Text (Text)
import qualified Data.Text as Text

-- time
import Data.Time.Format
import Data.Time.LocalTime

-- unix
import System.Posix.User (getRealUserID, UserEntry(..), getUserEntryForID)

-- yaml
import Data.Yaml (decodeFileThrow, Value(..))

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
      , evalHOST
      , evalUSER
      , evalGUARD
      , evalCLASS
      , evalMACROCLASS
      , evalCAMELCLASS
      , evalHASKELLRESOLVER
      , evalHASKELLMODULE
      , unsupported (Var "MAIL") "E-mail address of the current user."
      , unsupported (Var "LICENSE") "Abbreviation of the project's license, e.g. \"MIT\"."
      ]

unsupported :: forall m. MonadEvaluator m => Var -> Text -> EvaluatorInfo m
unsupported var@(Var varName) shortDesc = mkEvalInfo var description action
  where
    description = shortDesc <> " No built-in support; must be set in the configuration file."
    action =
      throwError @_ @_ @Void
        ("no built-in support for variable %" <> Text.unpack varName <> "%, "
         <> "please assign its value in the configuration file.")

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
    action = asks ctxTarget >>= getRootName

getRootName :: MonadError String m => Path b File -> m (Path Rel File)
getRootName path =
  case setFileExtension "" (filename path) of
    Left e -> throwError ("getRootName: " <> show e)
    Right rootName -> pure rootName

evalEXT :: MonadEvaluator m => EvaluatorInfo m
evalEXT = mkEvalInfo (Var "EXT") description action
  where
    description = "File extension (component after the last dot)."
    action = asks (tailSafe . fileExtension . ctxTarget)
    -- NOTE: fileExtension returns the extension including the dot (e.g., ".txt")

evalFFILE :: MonadEvaluator m => EvaluatorInfo m
evalFFILE = mkEvalInfo (Var "FFILE") description action
  where
    description = "File name, with extension. This is equivalent to expanding `%FILE%.%EXT%`."
    action = asks (filename . ctxTarget)

evalHOST :: MonadEvaluator m => EvaluatorInfo m
evalHOST = mkEvalInfo (Var "HOST") description action
  where
    description = "Current host name."
    action = liftIO getHostName

evalUSER :: MonadEvaluator m => EvaluatorInfo m
evalUSER = mkEvalInfo (Var "USER") description action
  where
    description = "Name of the currently logged-in user."
    action = liftIO $ do
      uid <- getRealUserID
      UserEntry{..} <- getUserEntryForID uid
      return (if not (isEmpty userGecos) then userGecos else userName)
    isEmpty = all isSpace

evalGUARD :: MonadEvaluator m => EvaluatorInfo m
evalGUARD = mkEvalInfo (Var "GUARD") description action
  where
    description = "A string suitable to be used as preprocessor guards for C or C++ header files."
    action = do
      let cleanChar c | isAlphaNum c = c
                      | otherwise    = '_'
      name <- asks (map cleanChar . toFilePath . filename . ctxTarget)
      Text.toUpper <$> toText name

run :: MonadEvaluator m => EvaluatorInfo m -> ReaderT Ctx m Text
run (eiAction -> EvaluatorAction action) = action >>= toText

evalCLASS :: MonadEvaluator m => EvaluatorInfo m
evalCLASS = mkEvalInfo (Var "CLASS") description action
  where
    description = "File name without extension, and the first character of each word capitalised."
    action = do
      rootName <- run evalFILE
      let groups = Text.groupBy ((==) `on` isAlpha) rootName
          capitalisedGroups = groups & each._head %~ toUpper
      return (Text.concat capitalisedGroups)

evalMACROCLASS :: MonadEvaluator m => EvaluatorInfo m
evalMACROCLASS = mkEvalInfo (Var "MACROCLASS") description action
  where
    description = "File name without extension, in upper case."
    action = Text.toUpper <$> run evalFILE

evalCAMELCLASS :: MonadEvaluator m => EvaluatorInfo m
evalCAMELCLASS = mkEvalInfo (Var "CAMELCLASS") description action
  where
    description = "File name without extension, the first character of each work capitalised, "
                  <> "and underscores removed."
    action = Text.replace "_" "" <$> run evalCLASS

evalHASKELLRESOLVER :: MonadEvaluator m => EvaluatorInfo m
evalHASKELLRESOLVER = mkEvalInfo (Var "HASKELLRESOLVER") description action
  where
    description = "Current stackage resolver, see https://www.stackage.org/"
    action = do
      targetDir <- parent <$> asks ctxTarget
      findStackConf targetDir >>= \case
        Nothing ->
          -- Unable to find stack.conf, so let the user fill out the value.
          return "TODO"
        Just stackConf -> do
          conf :: Value <- decodeFileThrow (toFilePath stackConf)
          return $ fromMaybe "TODO" (conf ^? key "resolver" . _String)

findStackConf :: MonadIO m => Path Abs Dir -> m (Maybe (Path Abs File))
findStackConf dir = do
  let stackConf = dir </> [relfile|stack.yaml|]
  doesFileExist stackConf >>= \case
    True -> return (Just stackConf)
    False | parent dir /= dir -> findStackConf (parent dir)
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
                       ++ [toFilePath targetRootName]
      return (intercalate "." components)

    moduleDirs :: Path Abs Dir -> [String]
    moduleDirs = go []
      where
        go !xs dir =
          case toFilePath (dirname dir) of
            h:_ | isUpper h -> let component = dropTrailingPathSeparator . toFilePath . dirname $ dir
                               in go (component : xs) (parent dir)
            _ -> xs

-- | @constEvaluator x@ always evaluates to `x`.
constEvaluator :: Monad m => Text -> Evaluator m
constEvaluator txt = Evaluator "Returns a constant value." (pure txt)
{-# INLINABLE constEvaluator #-}

-- | @commandEvaluator cmd@ executes the shell command `cmd` and returns its output.
-- The absolute path to the target file is available in the environment variable `MK_TARGET`.
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
        _ -> throwError ("Error running shell command `" <> cmd <> "`: exitCode = " <> show exitCode <> "\n\n"
                         <> "Output on stdout: \n" <> out <> "\n\n"
                         <> "Output on stderr: \n" <> err <> "\n")
{-# INLINABLE commandEvaluator #-}

maskEnv :: String -> String -> IO a -> IO a
maskEnv name tempValue action = do
  oldValue <- getEnv name
  finally (setEnv name tempValue True >> action)
          (maybe (unsetEnv name) (\v -> setEnv name v True) oldValue)
