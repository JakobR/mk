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

module Mk.Config
  ( Config(..)
  , loadConfig
  ) where

-- base
import Control.Monad
import Control.Monad.IO.Class
import Data.Char (toLower)
import Data.Foldable
import Data.List (isPrefixOf)
import Data.Maybe
import System.IO (hPutStrLn, stderr)

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



-- | The full configuration.
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


-- | Command-line options.
data UnresolvedOptions = UnresolvedOptions
  { optConfigFile :: FilePath
  , optTemplate :: Maybe FilePath
  , optForce :: Bool
  , optVerbose :: Bool
  , optQuiet :: Bool
  , optTarget :: FilePath
  }


data UnresolvedVarOverride = UVOverride
  { uvoName :: Text
  , uvoValue :: VarValue
  }
  deriving Generic


-- | Settings in the config file.
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


configFileInterpretOptions :: InterpretOptions
configFileInterpretOptions =
  defaultInterpretOptions{ fieldModifier = cfgFieldModifier }
  where
    cfgFieldModifier t = fromMaybe t $ asum [ headToLower <$> Text.stripPrefix pfx t | pfx <- fieldPfxs ]
    fieldPfxs = ["uvo", "ucfg"]

    headToLower :: Text -> Text
    headToLower (Text.uncons -> Just (x,xs)) = Text.cons (toLower x) xs
    headToLower xs = xs


resolveConfig
  :: MonadIO m
  => UnresolvedConfigFile
  -> UnresolvedOptions
  -> m Config
resolveConfig UnresolvedConfigFile{..} UnresolvedOptions{..} = do

  resolvedTarget <-
    resolveFile' optTarget

  resolvedTemplate <-
    traverse resolveFile' optTemplate

  resolvedTemplateSearchDirs <-
    traverse (resolveTilde >=> resolveDir') ucfgTemplateSearchDirs

  let toPair x = (Var (uvoName x), uvoValue x)
      rawVarOverrides = toPair <$> ucfgVariableOverrides

      repeatedElems = Map.keys . Map.filter (>1) . Map.fromList . map (, 1 :: Int)
      repeatedVarNames =  repeatedElems $ map fst rawVarOverrides
      resolvedVarOverrides = Map.fromList rawVarOverrides

  unless (null repeatedVarNames) $
    fail ("duplicate definitions in variableOverrides: " <> show repeatedVarNames)

  return Config{ cfgTemplateSearchDirs = resolvedTemplateSearchDirs
               , cfgVariableOverrides = resolvedVarOverrides
               , cfgTemplate = resolvedTemplate
               , cfgForce = optForce
               , cfgVerbose = optVerbose
               , cfgQuiet = optQuiet
               , cfgTarget = resolvedTarget
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


optionsParser :: Path Abs File -> Parser UnresolvedOptions
optionsParser defaultConfigFile =
  pure UnresolvedOptions
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


optionsParserInfo :: Path Abs File -> ParserInfo UnresolvedOptions
optionsParserInfo defaultConfigFile =
  info (optionsParser defaultConfigFile <**> helper)
       (fullDesc
        <> progDesc "Creates the file TARGET from a template."
        <> header "mk")


getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = do
  configDirSuffix <- parseRelDir "mk"
  configFilename <- parseRelFile "mk.dhall"
  defaultConfigDir <- getXdgDir XdgConfig (Just configDirSuffix)
  return (defaultConfigDir </> configFilename)


loadConfig :: IO Config
loadConfig = do
  defaultConfigFile <- getDefaultConfigFile

  unresolvedOptions@UnresolvedOptions{..} <-
    execParser (optionsParserInfo defaultConfigFile)

  resolvedConfigFile <- resolveFile' optConfigFile

  when optVerbose $
    hPutStrLn stderr ("Loading configuration from "
                      <> show resolvedConfigFile <> "...")

  unresolvedConfigFile <-
    inputFile (autoWith configFileInterpretOptions) (toFilePath resolvedConfigFile)

  resolveConfig unresolvedConfigFile unresolvedOptions
