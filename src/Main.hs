{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

-- base
import Control.Monad.IO.Class
import Data.Maybe
import qualified System.Exit
import System.IO

-- bytestring
-- import Data.ByteString (ByteString)

-- containers
-- import Data.Map.Strict (Map)

-- mtl
import Control.Monad.Reader

-- path
import Path

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

  runReaderT main' opts


main' :: (MonadReader Options m, MonadIO m) => m ()
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


die :: MonadIO m => String -> m a
die = liftIO . System.Exit.die
-- TODO: instead of using `die` everywhere, use a constraint `MonadError String m`


getTemplate :: (MonadReader Options m, MonadIO m) => m (Path Abs File)
getTemplate = do
  Options{..} <- ask
  case optTemplate of
    Just t -> pure t
    Nothing -> findTemplate


findTemplate :: (MonadReader Options m, MonadIO m) => m (Path Abs File)
findTemplate = do
  Options{..} <- ask
  when (null optTemplateSearchDirs) $
    die "Error: no template search paths"

  matchingTemplates <-
    catMaybes <$> traverse findTemplateInDir optTemplateSearchDirs

  case matchingTemplates of
    t:_ -> pure t
    [] -> die "Error: no matching template found"


findTemplateInDir
  :: (MonadReader Options m, MonadIO m)
  => Path Abs Dir
  -> m (Maybe (Path Abs File))
findTemplateInDir dir = do
  putVerbose $ "Searching directory: " <> show dir
  -- TODO: return an error if the match is ambigous (i.e., more than one file matches)
  pure Nothing
  -- error "TODO"


{-
findTemplatePath :: MonadIO m => Options -> m (Path Abs File)
findTemplatePath Options{..}

  | Just templatePath <- optTemplate =
      pure templatePath

  | optTemplateSearchDirs == [] =
      fail "Error: no template search paths"

  | otherwise = do
      error "TODO"
-}

-- findTemplatePath :: Maybe (Path Abs File) -> [Path Abs Dir] -> IO (Path Abs File)
-- findTemplatePath (Just templatePath) _ = pure templatePath
-- findTemplatePath Nothing [] = fail "Error: no template search paths"
-- findTemplatePath Nothing dirs = do
--   error "TODO"
