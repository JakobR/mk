{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Mk.Options
  ( Options'(..)
  , Options
  , execOptionsParser
  , Config(..)
  , loadConfig
  ) where

-- base
import Control.Monad
import Control.Monad.IO.Class
import Data.Char (toLower)
import Data.Foldable
import Data.List (isPrefixOf)
import Data.Maybe

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- dhall
import Dhall
import qualified Dhall.Core as D
import qualified Dhall.Map as DMap

-- filepath
import qualified System.FilePath as FilePath

-- optparse-applicative
import Options.Applicative

-- path
import Path

-- path-io
import Path.IO

-- text
import qualified Data.Text as Text

-- mk
import Mk.Template



data Config = Config
  { cfgTemplateSearchDirs :: [Path Abs Dir]
  -- ^ we search these directories to discover templates
  , cfgVariableOverrides :: Map Var VarValue
  , cfgTemplate :: Maybe (Path Abs File)
  , cfgForce :: Bool
  , cfgVerbose :: Bool
  , cfgQuiet :: Bool
  , cfgTarget :: Path Abs File
  }
  deriving Show


data VarValue
  = VarConst !Text
  | VarCommand !Text
  deriving Show

data UnresolvedVarOverride = UVOverride
  { uvoName :: Text
  , uvoValue :: VarValue
  }
  deriving Generic

data UnresolvedConfigFile = UnresolvedConfigFile
  { ucfgTemplateSearchDirs :: [FilePath]
  , ucfgVariableOverrides :: [UnresolvedVarOverride]
  }
  deriving Generic


instance Interpret VarValue where
  autoWith _ = Type{ expected = vvExpected
                   , extract = vvExtract
                   }
    where
      vvExpected = D.Union . DMap.fromList $
        [ ("Const", D.Text)
        , ("Command", D.Text)
        ]

      vvExtract (D.UnionLit "Const" v _) = VarConst <$> extract strictText v
      vvExtract (D.UnionLit "Command" v _) = VarCommand <$> extract strictText v
      vvExtract _ = Nothing

instance Interpret UnresolvedVarOverride
instance Interpret UnresolvedConfigFile


configInterpretOptions :: InterpretOptions
configInterpretOptions =
  defaultInterpretOptions{ fieldModifier = cfgFieldModifier }
                         -- , constructorModifier = cfgConstructorModifier }
  where
    cfgFieldModifier t = fromMaybe t $ asum [ headToLower <$> Text.stripPrefix pfx t | pfx <- fieldPfxs ]
    fieldPfxs = ["uvo", "ucfg"]

    -- cfgConstructorModifier t = fromMaybe t $ asum [ Text.stripPrefix pfx t | pfx <- constructorPfxs ]
    -- constructorPfxs = ["Var"]

    -- dropPrefix :: Text -> Text -> Text
    -- dropPrefix pfx (dropPrefixMay pfx -> Just name) = name
    -- dropPrefix _ name = name

    -- dropPrefixMay :: Text -> Text -> Maybe Text
    -- dropPrefixMay pfx pfxName = do
    --   capName <- Text.stripPrefix pfx pfxName
    --   (x, xs) <- Text.uncons capName
    --   let name = Text.cons (toLower x) xs
    --   return name

    headToLower :: Text -> Text
    headToLower (Text.uncons -> Just (x,xs)) = Text.cons (toLower x) xs
    headToLower xs = xs


resolveConfig
  :: MonadIO m
  => UnresolvedConfigFile
  -> Options
  -> m Config
resolveConfig c Options{..} = do

  resolvedTemplateSearchDirs <-
    traverse (resolveTilde >=> resolveDir') (ucfgTemplateSearchDirs c)

  let toPair x = (Var (uvoName x), uvoValue x)
      rawVarOverrides = toPair <$> ucfgVariableOverrides c

      repeatedElems = Map.keys . Map.filter (>1) . Map.fromList . map (, 1 :: Int)
      repeatedVarNames =  repeatedElems $ map fst rawVarOverrides
      resolvedVarOverrides = Map.fromList rawVarOverrides

  unless (null repeatedVarNames) $
    fail ("duplicate definitions in variableOverrides: " <> show repeatedVarNames)

  return Config{ cfgTemplateSearchDirs = resolvedTemplateSearchDirs
               , cfgVariableOverrides = resolvedVarOverrides
               , cfgTemplate = optTemplate
               , cfgForce = optForce
               , cfgVerbose = optVerbose
               , cfgQuiet = optQuiet
               , cfgTarget = optTarget
               }


resolveTilde
  :: MonadIO m
  => FilePath
  -> m FilePath
resolveTilde p
  | pfx `isPrefixOf` p = do
      homeDir <- getHomeDir
      return (toFilePath homeDir FilePath.</> drop (length pfx) p)
  | otherwise =
      return p
  where
    pfx = "~/"

-- resolveDirWithTilde'
--   :: MonadIO m
--   => FilePath
--   -> m (Path Abs Dir)
-- resolveDirWithTilde' = resolveTilde >=> resolveDir'
--   -- | "~/" `isPrefixOf` p = do
--   --     homeDir <- getHomeDir
--   --     resolveDir' (toFilePath homeDir FilePath.</> drop 2 p)
--   -- | otherwise = resolveDir' p


loadConfig :: IO Config
loadConfig = do
  opts@Options{..} <- execOptionsParser
  unresolvedConfig <- inputFile (autoWith configInterpretOptions) (toFilePath optConfigFile)
  config <- resolveConfig unresolvedConfig opts
  return config


data Options' dir file = Options
  { optTemplateSearchDirs :: [dir]  -- TODO: remove from options (move to config file)
  , optConfigFile :: file
  , optTemplate :: Maybe file
  , optForce :: Bool
  , optVerbose :: Bool
  , optQuiet :: Bool
  , optTarget :: file
  }
  deriving Show

type Options = Options' (Path Abs Dir) (Path Abs File)
type OptionsUnresolved = Options' FilePath FilePath


optionsParser :: Path Abs File -> Parser OptionsUnresolved
optionsParser defaultConfigFile =
  pure Options
  <*> many templateSearchDir
  <*> configFile
  <*> optional template
  <*> forceFlag
  <*> verboseFlag
  <*> quietFlag
  <*> target

  where

    target =
      argument str $
      help "Path of the file to create"
      <> metavar "TARGET"

    configFile =
      strOption $
      short 'c'
      <> long "config-file"
      <> value (toFilePath defaultConfigFile)
      <> showDefault
      <> help "The path of the configuration file."
      <> metavar "CONFIG-FILE"

    template =
      strOption $
      short 't'
      <> long "template"
      <> help ("Specify the path of the template to use explicitly rather than "
               ++ "choosing it by matching on the target name.")
      <> metavar "TEMPLATE"

    templateSearchDir =
      strOption $
      short 'd'
      <> long "search-dir"
      <> help ("Search the given directory for a matching template. "
               ++ "This option may be given multiple times. "
               ++ "Multiple directories are searched in the order they are given.")
      <> metavar "DIR"

    forceFlag =
      switch $
      short 'f'
      <> long "force"
      <> help "Force overwriting if target file already exists"

    verboseFlag =
      switch $
      short 'v'
      <> long "verbose"
      <> help "More output on stderr (mostly for debugging)"

    quietFlag =
      switch $
      short 'q'
      <> long "quiet"
      <> help "Less output on stdout, in particular the cursor positions will not be printed."


optionsParserInfo :: Path Abs File -> ParserInfo OptionsUnresolved
optionsParserInfo defaultConfigFile =
  info (optionsParser defaultConfigFile <**> helper)
       (fullDesc
        <> progDesc "Creates the file TARGET from a template."
        <> header "mk")


resolveOptions
  :: MonadIO m
  => OptionsUnresolved
  -> m Options
resolveOptions o = do

  resolvedConfigFile <-
    resolveFile' (optConfigFile o)

  resolvedTarget <-
    resolveFile' (optTarget o)

  resolvedTemplateSearchDirs <-
    traverse resolveDir' (optTemplateSearchDirs o)

  resolvedTemplate <-
    traverse resolveFile' (optTemplate o)

  return Options{ optTarget = resolvedTarget
                , optConfigFile = resolvedConfigFile
                , optTemplateSearchDirs = resolvedTemplateSearchDirs
                , optTemplate = resolvedTemplate
                , optForce = optForce o
                , optVerbose = optVerbose o
                , optQuiet = optQuiet o
                }


getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = do
  configDirSuffix <- parseRelDir "mk"
  configFilename <- parseRelFile "mk.dhall"
  defaultConfigDir <- getXdgDir XdgConfig (Just configDirSuffix)
  return (defaultConfigDir </> configFilename)


execOptionsParser :: IO Options
execOptionsParser = do
  defaultConfigFile <- getDefaultConfigFile
  unresolvedOptions <- execParser (optionsParserInfo defaultConfigFile)
  resolveOptions unresolvedOptions
