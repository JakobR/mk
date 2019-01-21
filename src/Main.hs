{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

-- base
import Control.Monad.IO.Class
import Data.Char (chr, isAlphaNum, ord)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Data.Word (Word8)
import System.Exit
import System.IO

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

-- containers
-- import Data.Map.Strict (Map)

-- conversion
import Conversion (convert)

-- extra
import Data.List.Extra (replace)

-- Glob
import qualified System.FilePath.Glob as Glob

-- megaparsec
import Text.Megaparsec
-- import Text.Megaparsec.Byte
-- import Text.Megaparsec.Byte.Lexer

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


newtype Var = Var { unVar :: ByteString }
  deriving Show

data Chunk
  = ChunkVerbatim !ByteString
  | ChunkVar !Var
  deriving Show

newtype Template = Template { unTemplate :: [Chunk] }
  deriving Show


-- type VarEvaluator = Path Abs File -> IO ByteString
-- -- type VarMap = Map Var VarEvaluator


type Parser = Parsec Void ByteString

ord8 :: Char -> Word8
ord8 c =
  case convert (ord c) of
    Just x -> x
    Nothing -> error $ "cannot to convert Char " <> show c <> " to Word8"

chr8 :: Word8 -> Char
chr8 = chr . convert

isAlphaNum8 :: Word8 -> Bool
isAlphaNum8 = isAlphaNum . chr8

varP :: Parser Var
varP = do
  void $ single (ord8 '%')
  varName <- takeWhile1P (Just "alphanumeric character") isAlphaNum8
  void $ single (ord8 '%')
  return (Var varName)

chunkP :: Parser Chunk
chunkP = fmap ChunkVar varP
         <|> fmap ChunkVerbatim verbatimP
  where
    verbatimP = takeWhile1P Nothing (/= ord8 '%')

templateP :: Parser Template
templateP = Template <$> many chunkP

setPosition :: SourcePos -> Parser ()
setPosition pos =
  updateParserState $ \s -> s{statePosState = (statePosState s){ pstateSourcePos = pos }}

parseTemplate'
  :: MonadError String m
  => SourcePos
  -> ByteString
  -> m Template
parseTemplate' pos str =
  case parse (setPosition pos *> templateP <* eof) "" str of
    Left err -> throwError $ errorBundlePretty err
    Right x -> return x

parseTemplate
  :: MonadError String m
  => Maybe FilePath  -- ^ name of source file
  -> ByteString      -- ^ text to parse
  -> m Template
parseTemplate srcName =
  parseTemplate' (initialPos $ fromMaybe "<string>" srcName)



main :: IO ()
main = do
  opts@Options{..} <- execOptionsParser
  when optVerbose $ hPrint stderr opts

  result <- runReaderT (runExceptT main') opts
  either (die . ("Error: "++)) pure result


main' :: (MonadReader Options m, MonadError String m, MonadIO m) => m ()
main' = do
  ask >>= printVerbose

  whenVerbose $ liftIO $ do
    hSetBuffering stderr NoBuffering

  -- Find matching template file
  templatePath <- getTemplate
  putVerbose $ "Selected template: " <> show templatePath

  -- Read and parse template file
  templateRaw <-
    liftIO $ ByteString.readFile (toFilePath templatePath)
  template <-
    withErrorPrefix ("when parsing " <> show templatePath <> ":\n") $
    parseTemplate (Just $ toFilePath templatePath) templateRaw
  putVerbose $ "Parsed template: " <> show template

  error "TODO"
  -- TODO
  -- Evaluate variables
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

withErrorPrefix
  :: MonadError String m
  => String
  -> m a
  -> m a
withErrorPrefix prefix m =
  m `catchError` \err -> throwError (prefix <> err)
