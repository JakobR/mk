{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

-- base
import Control.Monad.IO.Class
import Data.List
import System.Exit
import System.IO

-- bytestring
-- import Data.ByteString (ByteString)

-- containers
-- import Data.Map.Strict (Map)

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

-- transformers
import Control.Monad.Trans.Maybe

-- mk
import Options


-- newtype Var = Var { unVar :: ByteString }

-- type VarEvaluator = Path Abs File -> IO ByteString
-- -- type VarMap = Map Var VarEvaluator

-- data Chunk
--   = ChunkVerbatim !ByteString
--   | ChunkVar !Var

-- type RawTemplate = [Chunk]


main :: IO ()
main = do
  opts@Options{..} <- execOptionsParser
  when optVerbose $ hPrint stderr opts

  result <- runReaderT (runExceptT main') opts
  either die pure result


main' :: (MonadReader Options m, MonadError String m, MonadIO m) => m ()
main' = do
  ask >>= printVerbose

  whenVerbose $ liftIO $ do
    hSetBuffering stderr NoBuffering

  -- Find matching template file
  templatePath <- getTemplate
  putVerbose $ "Selected template: " <> show templatePath

  error "TODO"
  -- TODO
  -- Parse template
  -- Evaluate variable
  -- Write new file
  -- TODO: Check if new file already exists, only overwrite if given flag "-f/--force"


whenVerbose :: MonadReader Options m => m () -> m ()
whenVerbose action = do
  isVerbose <- asks optVerbose
  when isVerbose action


putVerbose :: (MonadReader Options m, MonadIO m) => String -> m ()
putVerbose msg =
  whenVerbose (liftIO $ hPutStrLn stderr msg)


printVerbose :: (MonadReader Options m, MonadIO m, Show a) => a -> m ()
printVerbose = putVerbose . show


getTemplate :: (MonadReader Options m, MonadError String m, MonadIO m) => m (Path Abs File)
getTemplate = do
  Options{..} <- ask
  case optTemplate of
    Just t -> pure t
    Nothing -> findTemplate


findTemplate :: (MonadReader Options m, MonadError String m, MonadIO m) => m (Path Abs File)
findTemplate = do
  dirs <- asks optTemplateSearchDirs

  when (null dirs) $
    throwError "Error: no template search paths"

  matchingTemplates <-
    runMaybeT $ msum (MaybeT . findTemplateInDir <$> dirs)

  case matchingTemplates of
    Just t -> pure t
    Nothing -> throwError "Error: no matching template found"


findTemplateInDir
  :: (MonadReader Options m, MonadError String m, MonadIO m)
  => Path Abs Dir
  -> m (Maybe (Path Abs File))
findTemplateInDir dir = do
  putVerbose $ "Searching directory: " <> show dir

  target <- asks optTarget
  (_, files) <- Path.IO.listDir dir

  let isHidden = ("." `isPrefixOf`) . toFilePath . filename
      templates = filter (not . isHidden) files
  putVerbose $ "Found templates: " <> show templates

  matchingTemplates <-
    filterM (target `matches`) templates
  putVerbose $ "Matching templates: " <> show matchingTemplates

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
  case Glob.tryCompileWith compOptions templateName' of
    Left err ->
      throwError $ "Error trying to match against " <> show template <> ": " <> err
    Right templatePattern -> do
      let result = Glob.matchWith matchOptions templatePattern targetName
      putVerbose $ "Matching " <> show targetName <>
        " against pattern " <> show templatePattern <> ": " <> show result
      pure result

  where
    targetName = toFilePath (filename target)
    templateName = toFilePath (filename template)
    templateName' = replace "=template=" "*" templateName

    compOptions =
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
