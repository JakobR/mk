{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Mk.Config
  ( Config(..)
  , VarValue(..)
  , CursorPosFormat(..)
  , loadConfig
  , readShowCursorPos_prop
  ) where

-- base
import Control.Monad
import Control.Monad.IO.Class
import Data.Char (toLower)
import Data.Foldable
import Data.List (intercalate, isPrefixOf)
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

-- lens
import Control.Lens (over, _head)

-- optparse-applicative
import Options.Applicative
import qualified Options.Applicative.Help.Pretty as Doc

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
  , cfgCursorPos :: CursorPosFormat
  , cfgWriteToStdout :: Bool
  , cfgTarget :: Path Abs File
  }
  deriving Show


data VarValue
  = VarConst !Text
  | VarCommand !Text
  deriving Show


data CursorPosFormat
  = CursorPosNone
  | CursorPosOne
  | CursorPosAll
  | CursorPosVim
  | CursorPosEmacs
  deriving (Eq, Enum, Bounded, Show)


-- TODO: Add an option to view descriptions of variables (should also list variables from config-file).
-- | Command-line options.
data UnresolvedOptions = UnresolvedOptions
  { optConfigFile :: FilePath
  , optTemplate :: Maybe FilePath
  , optForce :: Bool
  , optVerbose :: Bool
  , optCursorPos :: CursorPosFormat
  , optWriteToStdout :: Bool
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
        [ ("Const", Just D.Text)
        , ("Command", Just D.Text)
        ]

      vvExtract (D.App (D.Field _ "Const") v) = VarConst <$> extract strictText v
      vvExtract (D.App (D.Field _ "Command") v) = VarCommand <$> extract strictText v
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
    headToLower = over _head toLower


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
               , cfgCursorPos = optCursorPos
               , cfgWriteToStdout = optWriteToStdout
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
  <*> cursorPos
  <*> writeToStdoutFlag
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

    writeToStdoutFlag =
      switch $
      long "write-to-stdout"
      <> help ("Write output to stdout instead of TARGET. Note that TARGET is still "
               ++ "required for template matching and variable evaluation.")

    cursorPos =
      option (maybeReader readCursorPos) $
      short 'p'
      <> long "cursor-pos"
      <> value CursorPosNone
      <> showDefaultWith showCursorPos
      <> helpDoc (Just cursorPosHelp)

    cursorPosHelp = Doc.vsep
      [ paragraph ("Control how the initial cursor position indicated in the template is "
                   ++ "printed to standard output.")
      , Doc.text ("(values: " ++ cursorPosValues ++ ")")
      ]

    cursorPosValues = intercalate ", " $ showCursorPos <$> [minBound..maxBound]

    paragraph :: String -> Doc.Doc
    paragraph = foldr (Doc.</>) mempty . map Doc.string . words


readShowCursorPos_prop :: Bool
readShowCursorPos_prop = all (\x -> Just x == readCursorPos (showCursorPos x)) [minBound..maxBound]


showCursorPos :: CursorPosFormat -> String
showCursorPos CursorPosNone = "none"
showCursorPos CursorPosOne = "one"
showCursorPos CursorPosAll = "all"
showCursorPos CursorPosVim = "vim"
showCursorPos CursorPosEmacs = "emacs"


readCursorPos :: String -> Maybe CursorPosFormat
readCursorPos "none" = Just CursorPosNone
readCursorPos "one" = Just CursorPosOne
readCursorPos "all" = Just CursorPosAll
readCursorPos "vim" = Just CursorPosVim
readCursorPos "emacs" = Just CursorPosEmacs
readCursorPos _ = Nothing


optionsParserInfo :: Path Abs File -> ParserInfo UnresolvedOptions
optionsParserInfo defaultConfigFile =
  info (optionsParser defaultConfigFile <**> helper)
       (fullDesc
        <> progDesc "Creates the file TARGET from a template."
        <> header "mk")


-- | @~/.config/mk/mk.dhall@
getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = do
  defaultConfigDir <- getXdgDir XdgConfig (Just [reldir|mk|])
  return (defaultConfigDir </> [relfile|mk.dhall|])


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
