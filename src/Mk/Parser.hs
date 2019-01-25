{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Mk.Parser
  ( parseTemplate'
  , parseTemplate
  ) where


-- base
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import Data.Void (Void)

-- megaparsec
import Text.Megaparsec

-- mtl
import Control.Monad.Except

-- text
import Data.Text (Text)

-- mk
import Mk.Template


type Parser = Parsec Void Text

varP :: Parser Var
varP = do
  void $ single '%'
  varName <- takeWhile1P (Just "alphanumeric character") isAlphaNum
  void $ single '%'
  return (Var varName)

chunkP :: Parser Chunk
chunkP = ChunkCursor <$ cursorP
         <|> fmap ChunkVar varP
         <|> fmap ChunkVerbatim verbatimP
  where
    cursorP = chunk "%HERE%"
    verbatimP = takeWhile1P Nothing (/= '%')

templateP :: Parser Template
templateP = Template <$> many chunkP

setPosition :: SourcePos -> Parser ()
setPosition pos =
  updateParserState $ \s -> s{statePosState = (statePosState s){ pstateSourcePos = pos }}

parseTemplate'
  :: MonadError String m
  => SourcePos
  -> Text
  -> m Template
parseTemplate' pos str =
  case parse (setPosition pos *> templateP <* eof) "" str of
    Left err -> throwError $ errorBundlePretty err
    Right x -> return x
{-# INLINABLE parseTemplate' #-}

parseTemplate
  :: MonadError String m
  => Maybe FilePath  -- ^ name of source file
  -> Text            -- ^ text to parse
  -> m Template
parseTemplate srcName =
  parseTemplate' (initialPos $ fromMaybe "<string>" srcName)
{-# INLINABLE parseTemplate #-}
