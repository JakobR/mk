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

-- | Characters that are allowed in variable names
isVarNameChar :: Char -> Bool
isVarNameChar = isAlphaNum

-- | Variable name (not surrounded by "%")
varNameP :: Parser Var
varNameP = Var <$> takeWhile1P (Just "alphanumeric character") isVarNameChar

chunkP :: Parser Chunk
chunkP =
  ChunkCursor <$ cursorP
  <|> fmap ChunkVerbatim escapedP
  -- Variable parser must be allowed to backtrack to support lone "%" (e.g., so TeX comments do not have to be escaped)
  <|> try (fmap ChunkVar varP)
  <|> fmap ChunkVerbatim verbatimP
  -- Variable parser already failed at this point, so any remaining single "%" is allowed.
  -- This is useful because it enables us to use TeX comments in templates without having to escape them.
  <|> fmap ChunkVerbatim (chunk "%")
  where
    cursorP = chunk "%HERE%"
    escapedP = "%" <$ chunk "%%"
    varP = single '%' *> varNameP <* single '%'
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
  parseTemplate' (Text.Megaparsec.initialPos $ fromMaybe "<string>" srcName)
{-# INLINABLE parseTemplate #-}
