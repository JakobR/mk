module Mk.Options
  ( Options'(..)
  , Options
  , execOptionsParser
  ) where

-- base
import Control.Monad.IO.Class

-- optparse-applicative
import Options.Applicative

-- path
import Path

-- path-io
import Path.IO


data Options' dir file = Options
  { optTemplateSearchDirs :: [dir]
  -- ^ we search these directories to discover templates
  , optTemplate :: Maybe file
  -- , oprVarCommands :: [ TODO ]
  , optForce :: Bool
  , optVerbose :: Bool
  , optTarget :: file
  }
  deriving Show

type Options = Options' (Path Abs Dir) (Path Abs File)
type OptionsUnresolved = Options' FilePath FilePath


optionsParser :: Parser OptionsUnresolved
optionsParser =
  pure Options
  <*> many templateSearchDir
  <*> optional template
  <*> forceFlag
  <*> verboseFlag
  <*> target

  where

    target =
      argument str $
      help "Path of the file to create"
      <> metavar "TARGET"

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
      <> help "More output"


optionsParserInfo :: ParserInfo OptionsUnresolved
optionsParserInfo =
  info (optionsParser <**> helper)
       (fullDesc
        <> progDesc "Create new files from templates"
        <> header "mk")


resolveOptions
  :: MonadIO m
  => OptionsUnresolved
  -> m Options
resolveOptions o = do

  resolvedTarget <-
    resolveFile' (optTarget o)

  resolvedTemplateSearchDirs <-
    traverse resolveDir' (optTemplateSearchDirs o)

  resolvedTemplate <-
    traverse resolveFile' (optTemplate o)

  return Options{ optTarget = resolvedTarget
                , optTemplateSearchDirs = resolvedTemplateSearchDirs
                , optTemplate = resolvedTemplate
                , optForce = optForce o
                , optVerbose = optVerbose o
                }


execOptionsParser :: IO Options
execOptionsParser = execParser optionsParserInfo >>= resolveOptions
