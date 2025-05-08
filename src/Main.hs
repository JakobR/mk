{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

-- base
import Control.Monad (when, unless, msum, filterM)
import Data.List (isPrefixOf)
import System.Exit
import System.IO

-- extra
import Data.List.Extra (replace)

-- Glob
import qualified System.FilePath.Glob as Glob

-- mtl
import Control.Monad.Except
import Control.Monad.Reader

-- path
import Path

-- path-io
import qualified Path.IO

-- safe
import Safe.Foldable

-- text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy.IO as Text.Lazy.IO

-- transformers
import Control.Monad.Trans.Maybe

-- mk
import Mk.Evaluators
import Mk.Config
import Mk.Parser
import Mk.Template


main :: IO ()
main = do
  cfg@Config{..} <- loadConfig
  when cfgVerbose $ hPrint stderr cfg

  result <- runReaderT (runExceptT main') cfg
  case result of
    Left err -> die $ "Error: " ++ err
    Right () -> pure ()


main' :: (MonadReader Config m, MonadError String m, MonadIO m) => m ()
main' = do
  Config{..} <- ask

  -- Find matching template file
  templatePath <- getTemplate
  putVerboseLn $ "Selected template: " <> show templatePath

  -- Read and parse template file
  templateRaw <-
    liftIO $ Text.IO.readFile (toFilePath templatePath)
  template <-
    withErrorPrefix ("when parsing " <> show templatePath <> ":\n") $
    parseTemplate (Just $ toFilePath templatePath) templateRaw
  putVerboseLn $ "Parsed template: " <> show template

  -- Evaluate the variables in the template
  let overrideEvaluators = evalVarValue cfgTarget <$> cfgVariableOverrides
      -- Note: (<>) for Map is left-biased
      allEvaluators = overrideEvaluators <> builtinEvaluators cfgTarget
  (renderedTemplate, cursorPositions) <- do
    renderTemplate (runEvaluator <$> allEvaluators) template
  putVerboseLn $ "Rendered template: " <> show renderedTemplate
  putVerboseLn $ "Cursor positions: " <> show cursorPositions

  -- Write rendered template
  case cfgWriteToStdout of
    True -> do
      putVerboseLn ""  -- blank line for some visual separation on the terminal
      liftIO $ Text.Lazy.IO.putStr renderedTemplate
    False -> do
      unless cfgForce $ do
        targetExists <- Path.IO.doesFileExist cfgTarget
        when targetExists $
          throwError "Target file already exists. Pass the option --force to overwrite it anyways."
      Path.IO.ensureDir (parent cfgTarget)
      liftIO $ Text.Lazy.IO.writeFile (toFilePath cfgTarget) renderedTemplate

  let firstCursorPos = minimumDef initialPos cursorPositions
  case cfgCursorPos of
    CursorPosNone ->
      return ()
    CursorPosOne ->
      putCursorLn firstCursorPos
    CursorPosAll ->
      mapM_ putCursorLn cursorPositions
    CursorPosVim ->
      -- For vim mode, only output the first cursor position so
      -- we can just pass the output directly to vim as command-line argument.
      -- (usually a template will only have one cursor position anyways.)
      -- If no position is specified in the template, we will output position (0, 0);
      -- this way we don't have to check if the result is empty before passing it to vim.
      putCursorVim firstCursorPos
    CursorPosEmacs ->
      putCursorEmacs firstCursorPos


evalVarValue :: (MonadError String m, MonadIO m) => Path Abs File -> VarValue -> Evaluator m
evalVarValue _ (VarConst txt) = constEvaluator txt
evalVarValue target (VarCommand cmd) = commandEvaluator target cmd


-- | Prints the given position in the format "<abs>:<row>:<col>".
putCursorLn :: MonadIO m => Pos -> m ()
putCursorLn Pos{..} = liftIO $ do
  putStr (show posAbsolute)
  putChar ':'
  putStr (show posRow)
  putChar ':'
  putStrLn (show posCol)


-- | Intended use is as a command line argument to vim:
--   vim "+call cursor(<LINE>, <COLUMN>)" <FILE>
-- (via https://stackoverflow.com/a/3313469)
putCursorVim :: MonadIO m => Pos -> m ()
putCursorVim Pos{..} = liftIO $
  -- Note that line/column are one-based in vim
  let row = posRow + 1
      col = posCol + 1
  in putStr ("call cursor(" <> show row <> ", " <> show col <> ")")


-- | Intended use is as a command line argument to emacsclient:
--   emacsclient "+<LINE>:<COLUMN>" <FILE>
putCursorEmacs :: MonadIO m => Pos -> m ()
putCursorEmacs Pos{..} = liftIO $
  -- Note that line/column are one-based in Emacs
  let row = posRow + 1
      col = posCol + 1
  in putStr (show row <> ":" <> show col)


whenVerbose :: MonadReader Config m => m () -> m ()
whenVerbose action = do
  isVerbose <- asks cfgVerbose
  when isVerbose action


putVerboseLn :: (MonadReader Config m, MonadIO m) => String -> m ()
putVerboseLn msg =
  whenVerbose (liftIO $ hPutStrLn stderr msg)


getTemplate :: (MonadReader Config m, MonadError String m, MonadIO m) => m (Path Abs File)
getTemplate = do
  Config{..} <- ask
  case cfgTemplate of
    Just t -> pure t
    Nothing -> findTemplate


findTemplate :: (MonadReader Config m, MonadError String m, MonadIO m) => m (Path Abs File)
findTemplate = do
  searchDirs <- asks cfgTemplateSearchDirs

  when (null searchDirs) $
    throwError "Error: no template search paths"

  matchingTemplates <-
    runMaybeT $ msum (MaybeT . findTemplateInDir <$> searchDirs)

  case matchingTemplates of
    Just t -> pure t
    Nothing -> throwError "Error: no matching template found"


findTemplateInDir
  :: (MonadReader Config m, MonadError String m, MonadIO m)
  => Path Abs Dir
  -> m (Maybe (Path Abs File))
findTemplateInDir dir = do
  putVerboseLn $ "Searching directory: " <> show dir

  target <- asks cfgTarget
  (_, files) <- Path.IO.listDir dir

  let isHidden = ("." `isPrefixOf`) . toFilePath . filename
      templates = filter (not . isHidden) files
  putVerboseLn $ "Found templates: " <> show templates

  matchingTemplates <-
    filterM (target `matches`) templates
  putVerboseLn $ "Matching templates: " <> show matchingTemplates

  case matchingTemplates of
    [] -> pure Nothing
    [t] -> pure (Just t)
    ts@(_:_) -> throwError $ "Error: ambiguous match: " <> show ts


-- | Template file names are treated as glob-like patterns which target file name is matched against.
-- At the moment, only basic wildcards (@*@ and @?@) are enabled.
--
-- For compatibility with vim-template, the string @=template=@ acts like @*@.
matches
  :: (MonadReader Config m, MonadError String m, MonadIO m)
  => Path Abs File  -- ^ target file path
  -> Path Abs File  -- ^ template file path
  -> m Bool
matches target template = do
  case Glob.tryCompileWith compileOptions templateName' of
    Left err ->
      throwError $ "Error trying to match against " <> show template <> ": " <> err
    Right templatePattern -> do
      let result = Glob.matchWith matchOptions templatePattern targetName
      putVerboseLn ("Matching " <> show targetName
                    <> " against pattern " <> show templatePattern <> ": " <> show result)
      pure result

  where
    targetName = toFilePath (filename target)
    templateName = toFilePath (filename template)
    templateName' = replace "=template=" "*" templateName

    compileOptions =
      Glob.CompOptions{ Glob.characterClasses = False
                      , Glob.characterRanges = True
                      , Glob.numberRanges = False
                      , Glob.wildcards = True
                      , Glob.recursiveWildcards = False
                      , Glob.pathSepInRanges = False
                      , Glob.errorRecovery = False
                      }

    matchOptions =
      Glob.MatchOptions{ Glob.matchDotsImplicitly = True
                       , Glob.ignoreCase = False
                       , Glob.ignoreDotSlash = False
                       }


withErrorPrefix
  :: MonadError String m
  => String
  -> m a
  -> m a
withErrorPrefix prefix m =
  m `catchError` \err -> throwError (prefix <> err)
