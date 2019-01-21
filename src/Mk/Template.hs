{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Mk.Template
  ( Var(..)
  , Chunk(..)
  , Template(..)
  , Pos(..)
  , renderTemplate
  ) where


-- base
import Data.Foldable
import Data.Word (Word8)

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder

-- containers
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- mtl
import Control.Monad.Except

-- path
import Path

-- mk
import Mk.Char8



newtype Var = Var { unVar :: ByteString }
  deriving (Eq, Ord, Show)


data Chunk
  = ChunkVerbatim !ByteString
  | ChunkVar !Var
  | ChunkCursor
  deriving Show


newtype Template = Template { unTemplate :: [Chunk] }
  deriving Show


renderTemplate
  :: MonadError String m
  => Map Var (Var -> Path Abs File -> m ByteString)
  -> Path Abs File
  -> Template
  -> m (BL.ByteString, Set Pos)
renderTemplate evals target (Template chunks) = do
  renderedChunks <- traverse (renderChunk evals target) chunks
  let Pair b cs = combineRenderedChunks renderedChunks
  pure $ (toLazyByteString (blBuilder b), cs)
{-# INLINABLE renderTemplate #-}


data RenderedChunk
  = RenderedBytes !BuilderP
  | RenderedCursor


data Pair a b = Pair !a !b


combineRenderedChunks :: [RenderedChunk] -> Pair BuilderP (Set Pos)
combineRenderedChunks = foldl' go z
  where
    go :: Pair BuilderP (Set Pos) -> RenderedChunk -> Pair BuilderP (Set Pos)
    go (Pair b cs) RenderedCursor =
      Pair b (Set.insert (blPos b) cs)
    go (Pair b cs) (RenderedBytes b') =
      Pair (b <> b') cs

    z :: Pair BuilderP (Set Pos)
    z = Pair mempty mempty


renderChunk
  :: MonadError String m
  => Map Var (Var -> Path Abs File -> m ByteString)
  -> Path Abs File
  -> Chunk
  -> m RenderedChunk
renderChunk evals target = \case
  ChunkCursor ->
    pure RenderedCursor
  ChunkVerbatim bs ->
    pure . RenderedBytes $ byteStringP bs
  ChunkVar var ->
    case evals Map.!? var of
      Nothing ->
        throwError $ "no evaluator for variable " <> show var
      Just eval ->
        RenderedBytes . byteStringP <$> eval var target


-- | Represents a position in a string.
data Pos = Pos
  { posAbsolute :: !Word
    -- ^ index into the bytestring (i.e., the number of bytes to the left of the position)
  , posRow :: !Word
    -- ^ the row of the position (starting at 0)
  , posCol :: !Word
    -- ^ the column of the position (starting at 0)
  }
  deriving (Eq, Ord, Show)


initialPos :: Pos
initialPos = Pos 0 0 0


updatePos :: Pos -> Word8 -> Pos
updatePos (Pos i r c) b
  | b == newline8 = Pos (i+1) (r+1) 0
  | otherwise = Pos (i+1) r (c+1)


-- | Like @Builder@, but also stores the length, and row/column of the last position.
data BuilderP = BuilderP
  { blPos :: !Pos
    -- ^ The position at the end of the @Builder@.
    -- This means @posAbsolute@ is the length,
    -- @posRow@ is the number of newlines,
    -- and @posCol@ is the number of bytes on the last line.
  , blBuilder :: !Builder
  }


instance Semigroup BuilderP where
  BuilderP (Pos l1 r1 c1) b1 <> BuilderP (Pos l2 r2 c2) b2 =
    BuilderP (Pos l r c) (b1 <> b2)
    where
      l = l1 + l2
      r = r1 + r2
      c = case r2 of
            0 -> c1 + c2
            _ -> c2


instance Monoid BuilderP where
    mempty = BuilderP initialPos mempty


byteStringP :: ByteString -> BuilderP
byteStringP b = BuilderP (B.foldl updatePos initialPos b) (byteString b)
