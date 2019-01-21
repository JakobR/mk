{-# LANGUAGE FlexibleContexts #-}

module Mk.Parser
  ( parseTemplate'
  , parseTemplate
  ) where


-- base
import Data.Char (chr, isAlphaNum, ord)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Data.Word (Word8)

-- bytestring
import Data.ByteString (ByteString)

-- conversion
import Conversion (convert)

-- megaparsec
import Text.Megaparsec

-- mtl
import Control.Monad.Except

-- mk
import Mk.Template


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
