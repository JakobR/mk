{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

-- base
import Control.Monad.IO.Class
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

-- text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy.IO as Text.Lazy.IO

-- transformers
import Control.Monad.Trans.Maybe

-- mk
import Mk.Evaluators
import Mk.Options
import Mk.Parser
import Mk.Template


main :: IO ()
main = do
  opts@Options{..} <- execOptionsParser
  when optVerbose $ hPrint stderr opts

  result <- runReaderT (runExceptT main') opts
  case result of
    Left err -> die $ "Error: " ++ err
    Right x -> pure x


main' :: (MonadReader Options m, MonadError String m, MonadIO m) => m ()
main' = do
  opts@Options{..} <- ask
  printVerbose opts

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
  (renderedTemplate, cursorPositions) <- do
    renderTemplate (allEvaluators optTarget) template
  putVerboseLn $ "Rendered template: " <> show renderedTemplate
  putVerboseLn $ "Cursor positions: " <> show cursorPositions

  -- Write target file
  unless optForce $ do
    targetExists <- Path.IO.doesFileExist optTarget
    when targetExists $
      throwError "Target file already exists. Pass the option --force to overwrite it anyways."
  liftIO $ Text.Lazy.IO.writeFile (toFilePath optTarget) renderedTemplate

  unless optQuiet $
    forM_ cursorPositions putCursorLn


-- | Prints the given position in the format "<abs>:<row>:<col>".
putCursorLn :: MonadIO m => Pos -> m ()
putCursorLn Pos{..} = liftIO $ do
  putStr (show posAbsolute)
  putChar ':'
  putStr (show posRow)
  putChar ':'
  putStrLn (show posCol)


whenVerbose :: MonadReader Options m => m () -> m ()
whenVerbose action = do
  isVerbose <- asks optVerbose
  when isVerbose action


putVerboseLn :: (MonadReader Options m, MonadIO m) => String -> m ()
putVerboseLn msg =
  whenVerbose (liftIO $ hPutStrLn stderr msg)


printVerbose :: (MonadReader Options m, MonadIO m, Show a) => a -> m ()
printVerbose = putVerboseLn . show


getTemplate :: (MonadReader Options m, MonadError String m, MonadIO m) => m (Path Abs File)
getTemplate = do
  Options{..} <- ask
  case optTemplate of
    Just t -> pure t
    Nothing -> findTemplate


findTemplate :: (MonadReader Options m, MonadError String m, MonadIO m) => m (Path Abs File)
findTemplate = do
  searchDirs <- asks optTemplateSearchDirs

  when (null searchDirs) $
    throwError "Error: no template search paths"

  matchingTemplates <-
    runMaybeT $ msum (MaybeT . findTemplateInDir <$> searchDirs)

  case matchingTemplates of
    Just t -> pure t
    Nothing -> throwError "Error: no matching template found"


findTemplateInDir
  :: (MonadReader Options m, MonadError String m, MonadIO m)
  => Path Abs Dir
  -> m (Maybe (Path Abs File))
findTemplateInDir dir = do
  putVerboseLn $ "Searching directory: " <> show dir

  target <- asks optTarget
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
-- At the moment, only basic wildcards (`*` and `?`) are enabled.
--
-- For compatibility with vim-template, the string `=template=` acts like `*`.
matches
  :: (MonadReader Options m, MonadError String m, MonadIO m)
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
                      , Glob.characterRanges = False
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
