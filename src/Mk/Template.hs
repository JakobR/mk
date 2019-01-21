{-# LANGUAGE FlexibleContexts #-}

module Mk.Template
  ( Var(..)
  , Chunk(..)
  , Template(..)
  , renderTemplate
  ) where


-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder

-- containers
import Data.Map (Map)
import qualified Data.Map as Map

-- mtl
import Control.Monad.Except

-- path
import Path



newtype Var = Var { unVar :: ByteString }
  deriving (Eq, Ord, Show)

data Chunk
  = ChunkVerbatim !ByteString
  | ChunkVar !Var
  deriving Show

newtype Template = Template { unTemplate :: [Chunk] }
  deriving Show


renderTemplate
  :: MonadError String m
  => Map Var (Var -> Path Abs File -> m ByteString)
  -> Path Abs File
  -> Template
  -> m BL.ByteString
renderTemplate evals target (Template chunks) = do
  renderedChunks <- traverse (renderChunk evals target) chunks
  pure $ toLazyByteString (mconcat renderedChunks)

renderChunk
  :: MonadError String m
  => Map Var (Var -> Path Abs File -> m ByteString)
  -> Path Abs File
  -> Chunk
  -> m Builder
renderChunk _ _ (ChunkVerbatim bs) = pure (byteString bs)
renderChunk evals target (ChunkVar var) =
  case evals Map.!? var of
    Nothing ->
      throwError $ "no evaluator for variable " <> show var
    Just eval ->
      byteString <$> eval var target
