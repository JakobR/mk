{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Mk.Parser
  ( parseTemplate'
  , parseTemplate
  ) where


-- base
import Data.Maybe (fromMaybe)
import Data.Void (Void)

-- bytestring
import Data.ByteString (ByteString)

-- megaparsec
import Text.Megaparsec

-- mtl
import Control.Monad.Except

-- mk
import Mk.Char8
import Mk.Template


type Parser = Parsec Void ByteString

varP :: Parser Var
varP = do
  void $ single (ord8 '%')
  varName <- takeWhile1P (Just "alphanumeric character") isAlphaNum8
  void $ single (ord8 '%')
  return (Var varName)

chunkP :: Parser Chunk
chunkP = ChunkCursor <$ cursorP
         <|> fmap ChunkVar varP
         <|> fmap ChunkVerbatim verbatimP
  where
    cursorP = chunk "%HERE%"
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
{-# INLINABLE parseTemplate' #-}

parseTemplate
  :: MonadError String m
  => Maybe FilePath  -- ^ name of source file
  -> ByteString      -- ^ text to parse
  -> m Template
parseTemplate srcName =
  parseTemplate' (initialPos $ fromMaybe "<string>" srcName)
{-# INLINABLE parseTemplate #-}
