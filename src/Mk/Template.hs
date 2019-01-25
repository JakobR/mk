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

-- containers
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- mtl
import Control.Monad.Except

-- text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder



newtype Var = Var { unVar :: Text }
  deriving (Eq, Ord, Show)


data Chunk
  = ChunkVerbatim !Text
  | ChunkVar !Var
  | ChunkCursor
  deriving Show


newtype Template = Template { unTemplate :: [Chunk] }
  deriving Show


renderTemplate
  :: MonadError String m
  => Map Var (m Text)
  -> Template
  -> m (TL.Text, Set Pos)
renderTemplate evals (Template chunks) = do
  renderedChunks <- traverse (renderChunk evals) chunks
  let Pair b cs = combineRenderedChunks renderedChunks
  pure $ (toLazyText (bpBuilder b), cs)
  -- TODO: check out package Foldl for this
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
      Pair b (Set.insert (bpPos b) cs)
    go (Pair b cs) (RenderedBytes b') =
      Pair (b <> b') cs

    z :: Pair BuilderP (Set Pos)
    z = Pair mempty mempty


renderChunk
  :: MonadError String m
  => Map Var (m Text)
  -> Chunk
  -> m RenderedChunk
renderChunk evals = \case
  ChunkCursor ->
    pure RenderedCursor
  ChunkVerbatim t ->
    pure . RenderedBytes $ fromTextP t
  ChunkVar var ->
    case evals Map.!? var of
      Nothing ->
        throwError $ "no evaluator for variable " <> show var
      Just eval ->
        RenderedBytes . fromTextP <$> eval


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


updatePos :: Pos -> Char -> Pos
updatePos (Pos i r _) '\n' = Pos (i+1) (r+1) 0
updatePos (Pos i r c) _    = Pos (i+1) r (c+1)


finalPos :: Text -> Pos
finalPos = T.foldl updatePos initialPos


-- | Like @Builder@, but also stores the length, and row/column of the last position.
data BuilderP = BuilderP
  { bpPos :: !Pos
    -- ^ The position at the end of the @Builder@.
    -- This means @posAbsolute@ is the length,
    -- @posRow@ is the number of newlines,
    -- and @posCol@ is the number of bytes on the last line.
  , bpBuilder :: !Builder
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
  mappend = (<>)


fromTextP :: Text -> BuilderP
fromTextP t = BuilderP (finalPos t) (fromText t)
